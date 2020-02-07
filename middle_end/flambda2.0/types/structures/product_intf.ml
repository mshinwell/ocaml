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

module type S = sig
  type t

  type flambda_type
  type typing_env
  type meet_env
  type typing_env_extension

  module Index : Identifiable.S

  (** Create a product value given the indexes with associated components. *)
  val create : Flambda_kind.t -> flambda_type Index.Map.t -> t

  val create_bottom : unit -> t

  val is_bottom : t -> bool

  val width : t -> Targetint.OCaml.t

  val components : t -> flambda_type list

  val to_map : t -> flambda_type Index.Map.t

  val map_types
     : t
    -> f:(flambda_type -> flambda_type Or_bottom.t)
    -> t Or_bottom.t

  include Type_structure_intf.S
    with type t := t
    with type flambda_type := flambda_type
    with type typing_env := typing_env
    with type meet_env := meet_env
    with type typing_env_extension := typing_env_extension
end
