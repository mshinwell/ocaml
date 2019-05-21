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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (Index : Identifiable.S) : sig
  type t

  (** Create a product value given the indexes with associated components. *)
  val create : Flambda_types.t Index.Map.t -> t

  (** Widen the product by adding as many fields, after any existing fields,
      so that the product has the same number of fields as [to_match].  If the
      supplied product already has at least that many fields then it is
      returned unchanged. *)
  val widen : t -> to_match:t -> t

  include Type_structure_intf.S
    with type t := t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type type_equality_env := Type_equality_env.t
    with type typing_env_extension := Typing_env_extension.t
end
