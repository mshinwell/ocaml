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

(** Construction of meet and join operations for types of kind Value. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make
  (E : Either_meet_or_join_intf
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension) :
sig
  include Meet_and_join_spec_intf
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Typing_env_extension := Typing_env_extension
    with type of_kind_foo = Flambda_types.of_kind_value

  val meet_or_join_closures_entry
     : Join_env.t
    -> Flambda_types.closures_entry
    -> Flambda_types.closures_entry
    -> (Flambda_types.closures_entry * Typing_env_extension.t)
         Or_absorbing.t
end
