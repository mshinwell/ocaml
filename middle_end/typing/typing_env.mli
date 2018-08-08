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

(** Typing environments. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make_types (W : Typing_world.Types)
  : Typing_env_intf.S_types
      with module Flambda_types := W.Flambda_types

module type World = sig
  module rec Types : Typing_world.Types
    with module Typing_env = Make_types (Types)
  include Typing_world.S with module Types := Types
end

module Make (W : World)
  : Typing_env_intf.S
      with module Flambda_type0_core := W.Flambda_type0_core
      with module Join_env := W.Join_env
      with module Typing_env := W.Typing_env
      with module Typing_env_extension := W.Typing_env_extension
      with type binding_type := Make_types (W.Types).binding_type
      with type typing_environment_entry0 :=
        Make_types (W.Types).typing_environment_entry0
      with type typing_environment_entry :=
        Make_types (W.Types).typing_environment_entry
      with type levels_to_entries := Make_types (W.Types).levels_to_entries
