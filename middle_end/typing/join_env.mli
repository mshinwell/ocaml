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

(** "Join environments": structures which keep track of the various
    typing environments and environment extensions that are required whilst
    carrying out join operations (on types, etc). *)

module Make_types (W : Typing_world.Types)
    : Join_env_intf.S_types

module type World = sig
  module rec Types : Typing_world.Types
    with module Join_env = Make_t (Types)
  include Typing_world.S with module Types := Types
end

module Make (W : World)
  : Join_env_intf.S
      with module Meet_env := W.Meet_env
      with module Typing_env := W.Typing_env
      with module Typing_env_extension := W.Typing_env_extension
      with type t := Make_types (W).t
