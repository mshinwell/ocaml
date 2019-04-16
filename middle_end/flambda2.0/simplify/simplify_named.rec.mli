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

(** The following are only for the use of [Simplify_static]. *)

val simplify_set_of_closures0
   : Downwards_acc.t
  -> result_dacc:Downwards_acc.t
  -> Flambda.Set_of_closures.t
  -> closure_bound_names:Name_in_binding_pos.t Closure_id.Map.t
  -> closure_elements:Simple.t Var_within_closure.Map.t
  -> closure_element_types:Flambda_type.t Var_within_closure.Map.t
  -> Flambda.Set_of_closures.t
       * Flambda_type.t Name_in_binding_pos.Map.t
       * Downwards_acc.t

val type_closure_elements_and_make_lifting_decision
   : Downwards_acc.t
  -> min_occurrence_kind:Name_occurrence_kind.t
  -> Flambda.Set_of_closures.t
  -> bool * Simple.t Var_within_closure.Map.t
       * Flambda_type.t Var_within_closure.Map.t
