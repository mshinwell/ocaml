(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

module DE = Downwards_env
module DA = Downwards_acc
module UA = Upwards_acc
module UE = Upwards_env

(* CR mshinwell: We need to emit [Warnings.Inlining_impossible] as
   required.
   When in fallback-inlining mode: if we want to follow Closure we should
   not complain about function declarations with e.g. [@inline always]
   if the function contains other functions and therefore cannot be
   inlined.  We should however contain at call sites if inlining is
   requested but cannot be done for this reason.  I think this will probably
   all happen without any specific code once [Inlining_impossible]
   handling is implemented for the non-fallback-inlining cases. *)

type t =
  | Environment_says_never_inline
  | Unrolling_depth_exceeded
  | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded
  | Never_inline_attribute
  | Speculatively_not_inline of {
      cost_metrics: Cost_metrics.t;
      evaluated_to: float;
      threshold: float;
    }
  | Attribute_always
  | Attribute_unroll of int
  | Definition_says_inline
  | Speculatively_inline of {
      cost_metrics: Cost_metrics.t;
      evaluated_to: float;
      threshold: float;
    }

let print ppf t =
  match t with
  | Environment_says_never_inline ->
    Format.fprintf ppf "Environment_says_never_inline"
  | Unrolling_depth_exceeded ->
    Format.fprintf ppf "Unrolling_depth_exceeded"
  | Max_inlining_depth_exceeded ->
    Format.fprintf ppf "Max_inlining_depth_exceeded"
  | Recursion_depth_exceeded ->
    Format.fprintf ppf "Recursion_depth_exceeded"
  | Never_inline_attribute ->
    Format.fprintf ppf "Never_inline_attribute"
  | Attribute_always ->
    Format.fprintf ppf "Attribute_unroll"
  | Definition_says_inline ->
    Format.fprintf ppf "Definition_says_inline"
  | Attribute_unroll unroll_to ->
    Format.fprintf ppf
      "@[<hov 1>(Attribute_unroll@ \
        @[<hov 1>(unroll_to@ %d)@]\
        )@]"
      unroll_to
  | Speculatively_not_inline { cost_metrics; threshold; evaluated_to; } ->
    Format.fprintf ppf
      "@[<hov 1>(Speculatively_not_inline@ \
        @[<hov 1>(cost_metrics@ %a)@]@ \
        @[<hov 1>(evaluated_to@ %f)@]@ \
        @[<hov 1>(threshold@ %f)@]\
        )@]"
      Cost_metrics.print cost_metrics
      evaluated_to
      threshold
  | Speculatively_inline { cost_metrics; threshold; evaluated_to; } ->
    Format.fprintf ppf
      "@[<hov 1>(Speculatively_inline@ \
        @[<hov 1>(cost_metrics@ %a)@]@ \
        @[<hov 1>(evaluated_to@ %f)@]@ \
        @[<hov 1>(threshold@ %f)@]\
        )@]"
      Cost_metrics.print cost_metrics
      evaluated_to
      threshold

type can_inline =
  | Do_not_inline
  | Inline of { unroll_to : int option; }

let can_inline (t : t) : can_inline =
  match t with
  | Environment_says_never_inline
  | Unrolling_depth_exceeded
  | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded
  | Speculatively_not_inline _
  | Never_inline_attribute -> Do_not_inline
  | Attribute_unroll unroll_to -> Inline { unroll_to = Some unroll_to; }
  | Definition_says_inline
  | Speculatively_inline _
  | Attribute_always -> Inline { unroll_to = None; }

let report_reason fmt t =
  match (t : t) with
  | Environment_says_never_inline ->
    Format.fprintf fmt "the@ environment@ says@ never@ to@ inline"
  | Unrolling_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ unrolling@ depth@ has@ been@ exceeded"
  | Max_inlining_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ inlining@ depth@ has@ been@ exceeded"
  | Recursion_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ recursion@ depth@ has@ been@ exceeded"
  | Never_inline_attribute ->
    Format.fprintf fmt "the@ call@ has@ an@ attribute@ forbidding@ inlining"
  | Attribute_always ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@inline always]@ attribute"
  | Attribute_unroll n ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@unroll %d]@ attribute" n
  | Definition_says_inline ->
    Format.fprintf fmt "this@ function@ was@ decided@ to@ be@ always@ \
                        inlined@ at@ its@ definition@ site (annotated@ by@ \
                        [@inlined always]@ or@ determined@ to@ be@ small@ \
                        enough)"
  | Speculatively_not_inline { cost_metrics; evaluated_to; threshold } ->
    Format.fprintf fmt
      "the@ function@ was@ not@ inlined@ after@ speculation@ as@ \
        its@ cost@ metrics were=%a,@ which@ was@ evaluated@ \
        to@ %f > threshold %f"
      Cost_metrics.print cost_metrics
      evaluated_to
      threshold
  | Speculatively_inline { cost_metrics; evaluated_to; threshold } ->
    Format.fprintf fmt
      "the@ function@ was@ inlined@ after@ speculation@ as@ \
        its@ cost@ metrics were=%a,@ which@ was@ evaluated@ \
        to@ %f <= threshold %f"
      Cost_metrics.print cost_metrics
      evaluated_to
      threshold

let report fmt t =
  Format.fprintf fmt
    "@[<v>The function call %s been inlined@ because @[<hov>%a@]@]"
    (match can_inline t with
      | Inline _ -> "has"
      | Do_not_inline -> "has not")
    report_reason t

(* CR mshinwell: Overhaul handling of the inlining depth tracking so that
   it takes into account the depth of closures (or code), as per
   conversation with lwhite. *)

(* CR mshinwell: This parameter needs to be configurable *)
let max_rec_depth = 1

module I = Flambda_type.Function_declaration_type.Inlinable

let speculative_inlining dacc ~apply ~function_decl ~simplify_expr
      ~return_arity =
  let dacc = DA.set_do_not_rebuild_terms_and_disable_inlining dacc in
  (* CR-someday poechsel: [Inlining_transforms.inline] is preparing the body
     for inlining. Right know it may be called twice (once there and once in
     [simplify_apply_expr]) on the same apply expr. It should be possible to
     only call it once and remove some allocations.
  *)
  let dacc, expr =
    (* The only way for [unroll_to] not to be None is when an explicit
       Unroll annotation is provided by the user. If this is the case then
       inliner will always inline the function and will not call
       [speculative_inlining]. Thus inside of [speculative_inlining] we
       will always have [unroll_to] = None. We are not disabling unrolling
       when speculating, it just happens that no unrolling can happen while
       speculating right now.
    *)
    Inlining_transforms.inline dacc ~apply ~unroll_to:None function_decl
  in
  let scope = DE.get_continuation_scope_level (DA.denv dacc) in
  let dummy_toplevel_cont =
    Continuation.create ~name:"dummy_toplevel_continuation" ()
  in
  let dacc =
    DA.map_data_flow dacc ~f:(fun _ ->
      Data_flow.init_toplevel dummy_toplevel_cont [] Data_flow.empty)
  in
  let _, uacc =
    simplify_expr dacc expr ~down_to_up:(fun dacc ~rebuild ->
      let exn_continuation = Apply.exn_continuation apply in
      let dacc =
        DA.map_data_flow dacc ~f:(
          Data_flow.exit_continuation dummy_toplevel_cont)
      in
      let data_flow = DA.data_flow dacc in
      (* The dataflow analysis *)
      let function_return_cont =
        match Apply.continuation apply with
        | Never_returns -> Continuation.create ()
        | Return cont -> cont
      in
      let { required_variables; } : Data_flow.result =
        Data_flow.analyze data_flow
          ~return_continuation:function_return_cont
          ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
      in
      let uenv = UE.add_exn_continuation UE.empty exn_continuation scope in
      let uenv =
        match Apply.continuation apply with
        | Never_returns -> uenv
        | Return return_continuation ->
          UE.add_return_continuation uenv return_continuation scope
            return_arity
      in
      let uacc = UA.create ~required_variables uenv dacc in
      rebuild uacc ~after_rebuild:(fun expr uacc -> expr, uacc)
    )
  in
  UA.cost_metrics uacc

let might_inline dacc ~apply ~function_decl ~simplify_expr ~return_arity : t =
  let denv = DA.denv dacc in
  let env_prohibits_inlining = not (DE.can_inline denv) in
  if I.must_be_inlined function_decl then
    Definition_says_inline
  else if env_prohibits_inlining then
    Environment_says_never_inline
  else
    let cost_metrics =
      speculative_inlining ~apply dacc ~simplify_expr ~return_arity
        ~function_decl
    in
    let args =
      Apply.inlining_arguments apply
      |> Inlining_arguments.meet (DA.denv dacc |> DE.inlining_arguments)
    in
    let evaluated_to = Cost_metrics.evaluate ~args cost_metrics in
    let threshold = Inlining_arguments.threshold args in
    let is_under_inline_threshold =
      Float.compare evaluated_to threshold <= 0
    in
    if is_under_inline_threshold then
      Speculatively_inline { cost_metrics; evaluated_to; threshold }
    else
      Speculatively_not_inline { cost_metrics; evaluated_to; threshold }

let make_decision dacc ~simplify_expr ~function_decl ~apply ~return_arity : t =
  let inline = Apply.inline apply in
  match inline with
  | Never_inline -> Never_inline_attribute
  | Default_inline | Unroll _ | Always_inline | Hint_inline ->
      if Inlining_state.is_depth_exceeded (Apply.inlining_state apply)
      then
        Max_inlining_depth_exceeded
      else
        match inline with
        | Never_inline -> assert false
        | Default_inline ->
          might_inline dacc ~apply ~function_decl ~simplify_expr ~return_arity
        | Unroll unroll_to ->
          Attribute_unroll unroll_to
        | Always_inline | Hint_inline ->
          Attribute_always

let _ = Unrolling_depth_exceeded
let _ = Recursion_depth_exceeded
let _ = max_rec_depth
