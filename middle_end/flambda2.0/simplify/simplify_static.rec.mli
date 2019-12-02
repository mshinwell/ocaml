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

(** Simplification of toplevel definitions. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val simplify_set_of_closures0
    : Downwards_acc.t
   -> result_dacc:Downwards_acc.t
   -> Flambda.Set_of_closures.t
   -> closure_symbols:Symbol.t Closure_id.Map.t
   -> closure_elements:Simple.t Var_within_closure.Map.t
   -> closure_element_types:Flambda_type.t Var_within_closure.Map.t
   -> Flambda.Set_of_closures.t
        * Downwards_acc.t
        * Downwards_acc.t
        * Flambda_type.t Symbol.Map.t
        * Flambda_static.Program_body.Static_structure.t

val simplify_program
   : Simplify_env_and_result.Downwards_env.t
  -> Flambda_static.Program.t
  -> Flambda_static.Program.t
