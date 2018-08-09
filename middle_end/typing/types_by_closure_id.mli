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

(** Relational product, indexed by individual closure IDs, that (via
    logical variables) describes the makeup of a set of closures. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make_types
  (T : Typing_world_abstract.S)
  (Functor_T : Typing_world_abstract.Functor_S)
  : Types_by_closure_id_intf.S_types
      with module T := T
      and module Functor_T := Functor_T

module type Strengthened_world = sig
  module Recursive_world : sig
    module rec Types : (Typing_world_types.Types_nonrec
      with module Abstract_types := Types
      and module Abstract_functor_types := Functor_types
      with module Types_by_closure_id = Make_types (Types) (Functor_types))
    and Functor_types : Typing_world_types.Functor_types_nonrec
      with module Abstract_types := Types
  end
  include Typing_world.S with module Typing_world := Recursive_world
end

module Make (W : Strengthened_world) (F : Typing_world.Functor_S)
  : Types_by_closure_id_intf.S
      with module T := W.Recursive_world.Types
      and module Functor_T := W.Recursive_world.Functor_types
