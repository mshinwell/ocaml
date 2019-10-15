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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module DE = Simplify_env_and_result.Downwards_env

module Function_declaration_decision : sig
  type t =
    | Never_inline_attribute
    | Function_body_too_large
    | Stub
    | Inline

  let can_inline t =
    match t with
    | Never_inline_attribute
    | Function_body_too_large -> false
    | Stub
    | Inline -> true
end

let make_decision_for_function_declaration denv function_decl : Decision.t =
  (* At present, we follow Closure, taking inlining decisions without
     first examining call sites. *)
  match Function_declaration.inline function_decl with
  | Never_inline -> Never_inline_attribute
  | Always_inline | Default_inline | Unroll _ ->
    if Function_declaration.stub function_decl then Stub
    else
      Function_params_and_body.pattern_match
        (Function_declaration.params_and_body function_decl)
        ~f:(fun ~return_continuation:_ _exn_continuation _params ~body
                ~my_closure:_ ->
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
          if Inlining_cost.can_inline body inlining_threshold ~bonus:0
          then Inline
          else Function_body_too_large)

module Call_site_decision : sig
  type attribute_causing_inlining =
    | Unroll
    | Always

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

  let can_inline t =
    match t with
    | Environment_says_never_inline
    | Unrolling_depth_exceeded
    | Max_inlining_depth_exceeded
    | Recursion_depth_exceeded
    | Never_inline_attribute -> false
    | Inline _ -> true
end

(* CR mshinwell: This needs to be a configurable parameter *)
let max_inlining_depth = 10
let max_rec_depth = 1

let make_decision_for_call_site denv ~function_decl_rec_info
      (inline : Inline_attribute.t) : Decision.t =
  if (not (DE.can_inline denv)) then
    Environment_says_never_inline
  else
    match Rec_info.unroll_to function_decl_rec_info with
    | Some unroll_to ->
      if Rec_info.depth function_decl_rec_info >= unroll_to then
        Unrolling_depth_exceeded
      else
        Inline { attribute = None; unroll_to = None; }
    | None ->
      if apply_inlining_depth >= max_inlining_depth then
        Max_inlining_depth_exceeded
      else if Rec_info.depth function_decl_rec_info >= max_rec_depth then
        Recursion_depth_exceeded
      else
        match inline with
        | Never_inline -> Never_inline_attribute
        | Default_inline ->
          Inline { attribute = None; unroll_to = None; }
        | Unroll unroll_to ->
          Inline { attribute = Some Unroll; unroll_to = Some unroll_to; }
        | Always_inline ->
          Inline { attribute = Some Always; unroll_to = None; }
