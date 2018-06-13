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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Make
    (T : Flambda_type0_internal_intf.S)
    (Make_meet_and_join : functor
      (S : Meet_and_join_spec_intf.S with module T := T)
        -> Meet_and_join_intf.S
             with module T := T
             with type of_kind_foo := S.of_kind_foo)
    (Meet_and_join_naked_immediate : Meet_and_join_intf.S with module T := T)
    (Meet_and_join_naked_float : Meet_and_join_intf.S with module T := T)
    (Meet_and_join_naked_int32 : Meet_and_join_intf.S with module T := T)
    (Meet_and_join_naked_int64 : Meet_and_join_intf.S with module T := T)
    (Meet_and_join_naked_nativeint : Meet_and_join_intf.S with module T := T)
    (Meet_and_join_fabricated : Meet_and_join_intf.S with module T := T)
    (Meet_and_join : Meet_and_join_intf.S_for_types with module T := T)
    (Typing_env : Typing_env_intf.S with module T := T)
    (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
    (E : Either_meet_or_join_intf.S with module T := T)
  : Meet_and_join_intf.S
      with module T := T
      with type of_kind_foo = T.of_kind_value
