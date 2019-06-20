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

(** Union-find-like structure for keeping track of equivalence classes,
    used for alias resolution in the typing environment. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (E : sig
  type t
  type elt = t
  include Identifiable.S with type t := t

  val defined_earlier : t -> than:t -> bool
  val implicitly_bound_and_canonical : t -> bool

  module Order_for_canonicals : sig
    type t
    include Identifiable.S with type t := t
  end

  val order_for_canonicals : t -> Order_for_canonicals.t
end) : sig
  type t

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : t

  val add_canonical_element : t -> E.t -> t

  type add_result = private {
    canonical_element : E.t;
    alias_of : E.t;
  }

  val add
     : t
    -> E.t
    -> E.t
    -> add_result option * t

  val get_canonical_element
     : t
    -> E.t
    -> min_order_for_canonicals:E.Order_for_canonicals.t
    -> E.t option

  val get_normal_canonical_element : t -> E.t -> E.t option

  val get_aliases : t -> E.t -> E.Set.t

  val canonical_elements : t -> E.Set.t
end
