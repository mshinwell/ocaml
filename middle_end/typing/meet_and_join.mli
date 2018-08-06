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

(** Meet (greatest lower bound) and join (least upper bound) operations on
    types. *)

(* CR mshinwell: This should be called [Meet_or_join]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (W : Typing_world.S) :
  Meet_and_join_intf.S
    with module Flambda_types = W.Flambda_types
    with module Join_env = W.Join_env
    with module Meet_env = W.Meet_env
    with module Typing_env = W.Typing_env
    with module Typing_env_extension = W.Typing_env_extension
