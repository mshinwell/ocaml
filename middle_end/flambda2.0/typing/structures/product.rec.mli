(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Straightforward ordered product, e.g. for representing types of block fields
    without tracking any relations between them.

    Projections from products are achieved using [meet] operations.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Create a product given the kind of its elements and the types of its
    elements.  The kinds of all types in the list must be the same and equal
    to the supplied kind. *)
val create : Flambda_types.t list -> Flambda_kind.t -> t

(** Create a product with the given number of fields of the given kind.
    The types of the fields will be set to bottom. *)
val create_bottom : arity:int -> Flambda_kind.t -> t

(** Widen the product by adding as many fields, after any existing fields,
    so that the product has the same number of fields as [to_match].  If the
    supplied product already has at least that many fields then it is
    returned unchanged. *)
val widen : t -> to_match:t -> t

include Type_structure_intf.S
  with type t := t
  with type join_env := Join_env.t
  with type meet_env := Meet_env.t
  with type type_equality_env := Type_equality_env.t
  with type type_equality_result := Type_equality_result.t
  with type typing_env_extension := Typing_env_extension.t
