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
    used for alias resolution in the typing environment, with support for
    associating orderings to aliases of canonical elements. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (E : sig
  (* CR mshinwell: Add _intf.ml file. *)
  type t
  type elt = t
  include Identifiable.S with type t := t

  val defined_earlier : t -> than:t -> bool

  module Order_within_equiv_class : sig
    type t
    include Identifiable.S with type t := t

    (** The total order used by [Set], [Map] etc. must be a linear
        extension of this partial order.  This enables [find_first] to be
        used; see the .ml file. *)
    val compare_partial_order : t -> t -> int option

    val compare : t -> t -> [ `Be_explicit_about_total_or_partial_ordering ]
  end

  val order_within_equiv_class : t -> Order_within_equiv_class.t
end) : sig
  type t

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : t

  type add_result = private {
    t : t;
    canonical_element : E.t;
    alias_of : E.t;
  }

  val add
     : t
    -> E.t
    -> E.t
    -> add_result

  (** [get_canonical_element] returns [None] only when the
      [min_order_within_equiv_class] cannot be satisfied. *)
  val get_canonical_element_exn
     : t
    -> E.t
    -> min_order_within_equiv_class:E.Order_within_equiv_class.t
    -> E.t

  (** [get_aliases] always returns the supplied element in the result set. *)
  val get_aliases : t -> E.t -> E.Set.t
end
