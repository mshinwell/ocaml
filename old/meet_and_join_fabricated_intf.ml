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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module T : Typing_world_abstract.S

  module Make (E : Either_meet_or_join_intf.S) : sig
    include Meet_and_join_spec_intf.S
      with module T := T
      with type of_kind_foo = T.Flambda_types.of_kind_fabricated

    val meet_or_join_set_of_closures_entry
       : T.Join_env.t
      -> T.Flambda_types.set_of_closures_entry
      -> T.Flambda_types.set_of_closures_entry
      -> (T.Flambda_types.set_of_closures_entry * T.Typing_env_extension.t)
           Or_absorbing.t
  end
end
