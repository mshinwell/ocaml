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

(** Functions over which meet and join code is parameterised. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module For_meet (T : Typing_world.S)
  : Either_meet_or_join_intf.S
      with module Typing_env := T.Typing_env
      with module Typing_env_extension := T.Typing_env_extension
      with module Join_env := T.Join_env

module For_join (T : Typing_world.S)
  : Either_meet_or_join_intf.S
      with module Typing_env := T.Typing_env
      with module Typing_env_extension := T.Typing_env_extension
      with module Join_env := T.Join_env
