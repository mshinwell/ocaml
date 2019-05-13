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

(** Construction of meet and join operations for types of kind Fabricated. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make
  (E : Either_meet_or_join_intf.S
    with type join_env := Join_env.t
    with type meet_env := Meet_env.t
    with type typing_env_extension := Typing_env_extension.t) :
sig
  include Meet_and_join_spec_intf.S
    with type flambda_types := Flambda_types.t
    with type join_env := Join_env.t
    with type typing_env_extension := Typing_env_extension.t
    with type of_kind_foo = Flambda_types.of_kind_fabricated

  val meet_or_join_set_of_closures_entry
     : Join_env.t
    -> Flambda_types.set_of_closures_entry
    -> Flambda_types.set_of_closures_entry
    -> (Flambda_types.set_of_closures_entry * Typing_env_extension.t)
         Or_absorbing.t
end
