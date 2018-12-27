(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module L = Linearize

module type S_subrange_state = sig
  type t

  val create : unit -> t
  val advance_over_instruction : t -> L.instruction -> t
end

module type S_subrange_info = sig
  type t
  type key
  type subrange_state

  val create : key -> subrange_state -> t
end

module type S_range_info = sig
  type t
  type key
  type index

  val create
     : L.fundecl
    -> key
    -> start_insn:L.instruction
    -> (index * t) option
end

module type S_functor = sig
  module Index : Identifiable.S

  module Key : sig
    type t
    module Set : Set.S with type elt = t
    module Map : Map.S with type key = t

    val all_parents : t -> t list
  end

  module Subrange_state : S_subrange_state

  module Subrange_info : S_subrange_info
    with type key := Key.t
    with type subrange_state := Subrange_state.t

  module Range_info : S_range_info
    with type key := Key.t
    with type index := Index.t

  val available_before : L.instruction -> Key.Set.t

  val available_across : L.instruction -> Key.Set.t

  val must_restart_ranges_upon_any_change : unit -> bool
end

module type S = sig
  module Index : Identifiable.S

  module Key : sig
    type t
    module Set : Set.S with type elt = t
    module Map : Map.S with type key = t
  end

  module Subrange_state : S_subrange_state

  module Subrange_info : S_subrange_info
    with type key := Key.t
    with type subrange_state := Subrange_state.t

  module Range_info : S_range_info
    with type key := Key.t
    with type index := Index.t

  module Subrange : sig
    type t

    val info : t -> Subrange_info.t

    val start_pos : t -> Linearize.label
    val end_pos : t -> Linearize.label
    val end_pos_offset : t -> int
  end

  module Range : sig
    type t

    val info : t -> Range_info.t

    val extremities : t -> Linearize.label * Linearize.label

    val lowest_address : t -> Linearize.label option

    val fold
       : t
      -> init:'a
      -> f:('a -> Subrange.t -> 'a)
      -> 'a
  end

  type t

  val create : Linearize.fundecl -> t * Linearize.fundecl

  val range_covering_whole_function
     : t
    -> end_of_function_label:Linearize.label
    -> Range_info.t
    -> Subrange_info.t
    -> Range.t

  val iter : t -> f:(Index.t -> Range.t -> unit) -> unit

  val fold : t -> init:'a -> f:('a -> Index.t -> Range.t -> 'a) -> 'a

  val find : t -> Index.t -> Range.t

  val all_indexes : t -> Index.Set.t

  (** The [env] should come from [Coalesce_labels.fundecl]. *)
  val rewrite_labels : t -> env:int Numbers.Int.Map.t -> t
end
