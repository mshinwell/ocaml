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

let can_inline denv function_decl =
  (* At present, we follow Closure, taking inlining decisions without
     first examining call sites. *)
  match Function_declaration.inline function_decl with
  | Never_inline -> false
  | Always_inline | Default_inline | Unroll _ ->
    if Function_declaration.stub function_decl then true
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
          Inlining_cost.can_inline body inlining_threshold ~bonus:0)
