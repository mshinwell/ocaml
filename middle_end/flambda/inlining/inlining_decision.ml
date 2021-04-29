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

module Function_declaration_decision = struct
  type t =
    | Never_inline_attribute
    | Function_body_too_large of Code_size.t
    | Stub
    | Attribute_inline
    | Small_function of {
        size: Code_size.t;
        small_function_size: Code_size.t;
      }
    | Speculatively_inlinable of {
        size: Code_size.t;
        small_function_size: Code_size.t;
        large_function_size: Code_size.t;
      }

  type inlining_behaviour =
    | Cannot_be_inlined
    | Must_be_inlined
    | Could_possibly_be_inlined

  let behaviour t =
    match t with
    | Never_inline_attribute
    | Function_body_too_large _ -> Cannot_be_inlined
    | Stub
    | Attribute_inline
    | Small_function _ -> Must_be_inlined
    | Speculatively_inlinable _-> Could_possibly_be_inlined

  let print fmt = function
    | Never_inline_attribute ->
      Format.fprintf fmt "Never_inline_attribute"
    | Function_body_too_large large_function_size ->
      Format.fprintf fmt
        "@[<hov 1>(Function_body_too_large@ %a)@]"
        Code_size.print large_function_size
    | Stub ->
      Format.fprintf fmt "Stub"
    | Attribute_inline ->
      Format.fprintf fmt "Attribute_inline"
    | Small_function {size; small_function_size} ->
      Format.fprintf fmt
        "@[<hov 1>(Small_function@ \
         @[<hov 1>(size@ %a)@]@ \
         @[<hov 1>(small_function_size@ %a)@]\
          )@]"
        Code_size.print size
        Code_size.print small_function_size
    | Speculatively_inlinable {size;
                               small_function_size;
                               large_function_size} ->
      Format.fprintf fmt
        "@[<hov 1>(Speculatively_inlinable@ \
         @[<hov 1>(size@ %a)@]@ \
         @[<hov 1>(small_function_size@ %a)@]@ \
         @[<hov 1>(large_function_size@ %a)@]\
         )@]"
        Code_size.print size
        Code_size.print small_function_size
        Code_size.print large_function_size

  let report_reason fmt = function
    | Never_inline_attribute ->
      Format.fprintf fmt "%a"
        Format.pp_print_text "the function has an attribute preventing its inlining"
    | Function_body_too_large large_function_size ->
      Format.fprintf fmt "the@ function's@ body@ is@ too@ large,@ \
                          more@ specifically,@ it@ is@ larger@ than@ \
                          the@ large@ function@ size:@ %a"
        Code_size.print large_function_size
    | Stub ->
      Format.fprintf fmt "the@ function@ is@ a@ stub"
    | Attribute_inline ->
      Format.fprintf fmt "the@ function@ has@ an@ attribute@ forcing@ its@ inlining"
    | Small_function {size; small_function_size} ->
      Format.fprintf fmt "the@ function's@ body@ is@ smaller@ \
                          than@ the@ threshold@ size@ for@ small@ functions: \
                          size=%a <= \
                          large@ function@ size=%a"
        Code_size.print size Code_size.print small_function_size
    | Speculatively_inlinable {size;
                               small_function_size;
                               large_function_size} ->
      Format.fprintf fmt "the@ function's@ body@ is@ between@ \
                          the@ threshold@ size@ for@ small@ functions and \
                          the@ threshold@ size@ for@ large@ functions: \
                          small@ function@ size=%a < \
                          size=%a < \
                          large@ function@ size=%a"
        Code_size.print small_function_size
        Code_size.print size
        Code_size.print large_function_size

  let report fmt t =
    Format.fprintf fmt "@[<v>The function %s be inlined at its use-sites@ \
                        because @[<hov>%a@]@]"
      (match behaviour t with
       | Cannot_be_inlined -> "cannot"
       | Could_possibly_be_inlined -> "could"
       | Must_be_inlined -> "must")
      report_reason t

end

type cost_metrics_source = From_denv | Metrics of Cost_metrics.t

let make_decision_for_function_declaration denv ~cost_metrics_source function_decl
  : Function_declaration_decision.t =
  (* At present, we follow Closure, taking inlining decisions without
     first examining call sites. *)
  let code_id = Function_declaration.code_id function_decl in
  let code = DE.find_code denv code_id in
  let args =
    Code.inlining_arguments code
    |> Inlining_arguments.meet (DE.inlining_arguments denv)
  in
  match Code.inline code with
  | Never_inline -> Never_inline_attribute
  | Hint_inline | Always_inline -> Attribute_inline
  | Default_inline | Unroll _ ->
    if Code.stub code then Stub
    else
      let metrics =
        match cost_metrics_source with
        | Metrics metrics -> metrics
        | From_denv -> Code.cost_metrics code
      in
      let large_function_size =
        Inlining_arguments.large_function_size args |> Code_size.of_int
      in
      let small_function_size =
        Inlining_arguments.small_function_size args |> Code_size.of_int
      in
      let size = Cost_metrics.size metrics in
      let is_small = Code_size.(<=) size small_function_size in
      let is_large = Code_size.(<=) large_function_size size in
      if is_large then
        Function_body_too_large large_function_size
      else if is_small then
        Small_function {
          size = Cost_metrics.size metrics;
          small_function_size
        }
      else
        Speculatively_inlinable {
          size = Cost_metrics.size metrics;
          small_function_size;
          large_function_size
        }

module Call_site_decision = struct
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
end

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

let might_inline dacc ~apply ~function_decl ~simplify_expr ~return_arity
  : Call_site_decision.t =
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

let make_decision_for_call_site dacc ~simplify_expr ~function_decl
      ~function_decl_rec_info ~apply ~return_arity : Call_site_decision.t =
  let inline = Apply.inline apply in
  match inline with
  | Never_inline -> Never_inline_attribute
  | Default_inline | Unroll _ | Always_inline | Hint_inline ->
    match Rec_info.unroll_to function_decl_rec_info with
    | Some unroll_to ->
      if Rec_info.depth function_decl_rec_info >= unroll_to then
        Unrolling_depth_exceeded
      else
        might_inline dacc ~apply ~function_decl ~simplify_expr ~return_arity
    | None ->
      let apply_inlining_state = Apply.inlining_state apply in
      if Inlining_state.is_depth_exceeded apply_inlining_state
      then
        Max_inlining_depth_exceeded
      else
        match inline with
        | Never_inline -> assert false
        | Default_inline ->
          if Rec_info.depth function_decl_rec_info >= max_rec_depth then
            Recursion_depth_exceeded
          else
            might_inline dacc ~apply ~function_decl ~simplify_expr ~return_arity
        | Unroll unroll_to ->
          let unroll_to =
            Rec_info.depth function_decl_rec_info + unroll_to
          in
          Attribute_unroll unroll_to
        | Always_inline | Hint_inline ->
          Attribute_always
