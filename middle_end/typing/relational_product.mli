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

(** A "relational product" represents a list of indexed products.  Each
    indexed product binds a set of components, thus:

       ------
        |  |
        |  |     (component_i : Component)
      i : Index

    and additionally holds relational information between the components
    expressed as a typing environment extension.

    Any indexed product in a relational product may depend on components'
    names bound by an earlier indexed product.  The overall structure is thus:

       ------    ------
        |  |      |  |
        |  |      |  |     (component_i_n : Component)
       n : int  i_n : Index

    where the outer (dependent) product corresponds to the list structure.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make_types
  (T : Typing_world_abstract.S)
  : Relational_product_intf.S_types
      with module T := T

module type Strengthened_world = sig
  module Recursive_world : sig
    module rec Types : (Typing_world_types.Types_nonrec
      with module Abstract_types := Types
      and module Abstract_functor_types := Functor_types
      with module Relational_product = Make_types (Types))
    and Functor_types : Typing_world_types.Functor_types_nonrec
      with module Abstract_types := Types
  end
  include Typing_world.S with module Types := Recursive_world
end

module Make (W : Strengthened_world)
  : Relational_product_intf.S
      with module T := W.Recursive_world.Types
