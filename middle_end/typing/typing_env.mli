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

module Make_types (W : Typing_world.Types) = struct
  type binding_type = Normal | Was_existential

  type typing_environment_entry0 =
    | Definition of Flambda_type.t
    | Equation of Flambda_type.t

  type typing_environment_entry = private
    | Definition of Flambda_type.t
    | Equation of Flambda_type.t
    | CSE of Flambda_primitive.With_fixed_value.t

  type levels_to_entries =
    (Name.t * typing_environment_entry)
      Scope_level.Sublevel.Map.t Scope_level.Map.t
end

module type World = sig
  module rec Types : Typing_world.Types
    with module Typing_env = Make_t (Types)
  include Typing_world.S with module Types := Types
end

module Make (W : World)
  : Typing_env_intf.S
      with module Flambda_type0_core := W.Flambda_type0_core
      with module Join_env := W.Join_env
      with module Typing_env := W.Typing_env
      with module Typing_env_extension := W.Typing_env_extension
