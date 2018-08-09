(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Interface to type equality functions. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module T : Typing_world_abstract.S
  module Functor_T : Typing_world_abstract.Functor_S

  val fast_equal : T.Flambda_types.t -> T.Flambda_types.t -> bool

  val equal : T.Flambda_types.t -> T.Flambda_types.t -> bool

  (* CR mshinwell: Rename to [equal_in_env]. *)
  val equal_with_env
     : Type_equality_env.t
    -> T.Flambda_types.t
    -> T.Flambda_types.t
    -> bool

  val equal_closures_entry
     : Type_equality_env.t
    -> T.Flambda_types.closures_entry
    -> T.Flambda_types.closures_entry
    -> bool

  val equal_set_of_closures_entry
     : Type_equality_env.t
    -> T.Flambda_types.set_of_closures_entry
    -> T.Flambda_types.set_of_closures_entry
    -> bool
end
