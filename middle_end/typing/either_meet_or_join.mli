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

module For_meet (W : Typing_world.S)
  : Either_meet_or_join_intf.S
      with module Join_env := W.Join_env
      with module Typing_env := W.Typing_env
      with module Typing_env_extension := W.Typing_env_extension

module For_join (W : Typing_world.S)
  : Either_meet_or_join_intf.S
      with module Join_env := W.Join_env
      with module Typing_env := W.Typing_env
      with module Typing_env_extension := W.Typing_env_extension
