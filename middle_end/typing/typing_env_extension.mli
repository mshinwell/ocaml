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

(** Typing environment extensions. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make_types (W : Typing_world.Types) = struct
  type t
end

module type World = sig
  module rec Types : Typing_world.Types
    with module Typing_env_extension = Make_t (Types)
  include Typing_world.S with module Types := Types
end

module Make (W : World) :
  Typing_env_extension_intf.S
    with module Flambda_types := W.Flambda_types
    with module Typing_env := W.Typing_env
    with module Join_env := W.Join_env
