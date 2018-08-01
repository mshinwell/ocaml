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

module Make
    (Index : Relational_product_intf.Name_like)
    (Component : sig
      include Relational_product_intf.Name_like

      val create : unit -> t
    end)
    (T : Typing_world.S) :
  Relational_product_intf.S
    with module Typing_env := T.Typing_env
    with module Typing_env_extension := T.Typing_env_extension
    with module Join_env := T.Join_env
