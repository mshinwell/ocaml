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

(** [simplify_named] returns the bindings in order (i.e. outermost first
    in the list). *)
val simplify_named
   : Downwards_acc.t
  -> bound_vars:Bindable_let_bound.t
  -> Flambda.Named.t
  -> (Bindable_let_bound.t * Reachable.t) list * Downwards_acc.t

type pre_simplification_types_of_my_closures = {
  internal_closure_types : Flambda_type.t Closure_id.Map.t;
  closure_types : Flambda_type.t Closure_id.Map.t;
}

val function_decl_type
   : Simplify_env_and_result.Downwards_env.t
  -> Flambda.Function_declaration.t
  -> Rec_info.t
  -> Flambda_type.t

val simplify_function
   : Downwards_acc.t
  -> Closure_id.t
  -> Flambda.Function_declaration.t
  -> pre_simplification_types_of_my_closures
  -> Flambda.Function_declaration.t * Flambda_type.function_declaration
       * Simplify_env_and_result.Result.t
