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
    | Function_body_too_large of Inlining_cost.Threshold.t
    | Stub
    | Inline of (int * Inlining_cost.Threshold.t) option

  let can_inline t =
    match t with
    | Never_inline_attribute
    | Function_body_too_large _ -> false
    | Stub
    | Inline _ -> true

  let print fmt = function
    | Never_inline_attribute ->
      Format.fprintf fmt "Never_inline_attribute"
    | Function_body_too_large threshold ->
      Format.fprintf fmt "Function_body_too_large(%a)"
        Inlining_cost.Threshold.print threshold
    | Stub ->
      Format.fprintf fmt "Stub"
    | Inline None ->
      Format.fprintf fmt "Inline_no_cost_computed"
    | Inline Some (size, threshold) ->
      Format.fprintf fmt "Inline(size=%d, %a)" size
        Inlining_cost.Threshold.print threshold

  let report_reason fmt = function
    | Never_inline_attribute ->
      Format.fprintf fmt "%a"
        Format.pp_print_text "the function has an attribute preventing its inlining"
    | Function_body_too_large threshold ->
      Format.fprintf fmt "the@ function's@ body@ is@ too@ large,@ \
                          more@ specifically,@ it@ is@ larger@ than@ the@ threshold:@ %a"
        Inlining_cost.Threshold.print threshold
    | Stub ->
      Format.fprintf fmt "the@ function@ is@ a@ stub"
    | Inline None ->
      Format.fprintf fmt "the@ function@ has@ an@ attribute@ forcing@ its@ inlining"
    | Inline Some (size, threshold) ->
      Format.fprintf fmt "the@ function's@ body@ is@ smaller@ \
                          than@ the@ threshold:@ size=%d < threshold=%a"
        size Inlining_cost.Threshold.print threshold

  let report fmt t =
    Format.fprintf fmt "@[<v>The function %s be inlined at its use-sites@ \
                        because @[<hov>%a@]@]"
      (if can_inline t then "can" else "cannot") report_reason t

end

let make_decision_for_function_declaration denv ?params_and_body function_decl
      : Function_declaration_decision.t =
  (* At present, we follow Closure, taking inlining decisions without
     first examining call sites. *)
  let code_id = Function_declaration.code_id function_decl in
  let code = DE.find_code denv code_id in
  match Code.inline code with
  | Never_inline -> Never_inline_attribute
  | Hint_inline | Always_inline -> Inline None
  | Default_inline | Unroll _ ->
    if Code.stub code then Stub
    else
      let params_and_body =
        match params_and_body with
        | None ->
          Code.params_and_body_must_be_present code
            ~error_context:"Inlining decision"
        | Some params_and_body -> params_and_body
      in
      Function_params_and_body.pattern_match params_and_body
        ~f:(fun ~return_continuation:_ _exn_continuation _params ~body
                ~my_closure:_ ~is_my_closure_used:_
                : Function_declaration_decision.t ->
          let inlining_threshold : Inlining_cost.Threshold.t =
            let round = DE.round denv in
            let unscaled =
              Clflags.Float_arg_helper.get ~key:round !Clflags.inline_threshold
            in
            (* CR-soon pchambart: Add a warning if this is too big
               mshinwell: later *)
            Can_inline_if_no_larger_than
              (int_of_float
                (unscaled *.
                  (float_of_int Inlining_cost.scale_inline_threshold_by)))
          in
          match Inlining_cost.can_inline denv body inlining_threshold ~bonus:0 with
          | Can_inline size -> Inline (Some (size, inlining_threshold))
          | Cannot_inline -> Function_body_too_large inlining_threshold)

module Call_site_decision = struct
  type attribute_causing_inlining =
    | Unroll
    | Always

  let print_attribute_causing_inlining ppf attr =
    match attr with
    | Unroll -> Format.fprintf ppf "Unroll"
    | Always -> Format.fprintf ppf "Always"

  type t =
    | Environment_says_never_inline
    | Unrolling_depth_exceeded
    | Max_inlining_depth_exceeded
    | Recursion_depth_exceeded
    | Never_inline_attribute
    | Inline of {
        attribute : attribute_causing_inlining option;
        unroll_to : int option;
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
    | Inline { attribute; unroll_to; } ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(attribute@ %a)@]@ \
          @[<hov 1>(unroll_to@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print print_attribute_causing_inlining) attribute
        (Misc.Stdlib.Option.print Numbers.Int.print) unroll_to

  type can_inline =
    | Do_not_inline
    | Inline of { unroll_to : int option; }

  let can_inline (t : t) : can_inline =
    match t with
    | Environment_says_never_inline
    | Unrolling_depth_exceeded
    | Max_inlining_depth_exceeded
    | Recursion_depth_exceeded
    | Never_inline_attribute -> Do_not_inline
    | Inline { attribute = _; unroll_to; } -> Inline { unroll_to; }


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
    | Inline { attribute = None; unroll_to = None; } ->
      Format.fprintf fmt "the@ function@ was@ deemed@ inlinable@ from@ its@ declaration"
    | Inline { attribute = Some Always; unroll_to = _; } ->
      Format.fprintf fmt "the@ call@ has@ an@ [@@inline always]@ attribute"
    | Inline { attribute = Some Unroll; unroll_to = Some n; } ->
      Format.fprintf fmt "the@ call@ has@ an@ [@@unroll %d]@ attribute" n

    (* this should not happen *)
    | Inline { attribute = None; unroll_to = Some _; }
    | Inline { attribute = Some Unroll; unroll_to = None; }
      ->
      Misc.fatal_errorf "This should not happen (Inlining_decision.report is not in sync\
                         with Inlining_decision.make_decision_for_call_site)"

  let report fmt t =
    Format.fprintf fmt "@[<v>The function call %s been inlined@ because @[<hov>%a@]@]"
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

let make_decision_for_call_site denv ~function_decl_rec_info
      ~apply_inlining_state (inline : Inline_attribute.t)
      : Call_site_decision.t =
  if (not (DE.can_inline denv)) then
    Environment_says_never_inline
  else
    match inline with
    | Never_inline -> Never_inline_attribute
    | Default_inline | Unroll _ | Always_inline | Hint_inline ->
      match Rec_info.unroll_to function_decl_rec_info with
      | Some unroll_to ->
        if Rec_info.depth function_decl_rec_info >= unroll_to then
          Unrolling_depth_exceeded
        else
          Inline { attribute = None; unroll_to = None; }
      | None ->
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
              Inline { attribute = None; unroll_to = None; }
          | Unroll unroll_to ->
            let unroll_to =
              Rec_info.depth function_decl_rec_info + unroll_to
            in
            Inline { attribute = Some Unroll; unroll_to = Some unroll_to; }
          | Always_inline | Hint_inline ->
            Inline { attribute = Some Always; unroll_to = None; }
