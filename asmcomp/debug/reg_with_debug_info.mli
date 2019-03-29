(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Registers optionally equipped with extra data about their contents that
    is used for generating debugging information. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** What a particular register holds. *)
module Holds_value_of : sig
  type t =
    | Var of Backend_var.t
      (** The value of the given variable. *)
    | Const_int of Targetint.t
      (** The given integer constant. *)
    | Const_naked_float of Int64.t
      (** The floating-point constant with the given bit pattern. *)
    | Const_symbol of string
      (** The given statically-allocated constant. *)

  include Identifiable.S with type t := t
end

module Debug_info : sig
  (** The type of debugging information attached to a register. *)
  type t

  (** Total order on values of type [t]. *)
  val compare : t -> t -> int

  (** The identifier or constant that the register holds (part of) the
      value of. *)
  val holds_value_of : t -> Holds_value_of.t

  (** If the register holds only one part of a value (for example half of a
      64-bit constant), then this function returns how many parts there are,
      or unity otherwise. *)
  val num_parts_of_value : t -> int

  (** If [num_parts_of_value] is greater than zero, then this function returns
      the zero-based index of which part of the whole value is contained within
      the register; otherwise it returns zero. *)
  val part_of_value : t -> int

  (* CR-soon mshinwell: This doesn't seem quite right, because the provenance
     is only relevant for the [Var] case.  Likewise [is_parameter].  We should
     try to improve this after the initial merge. *)

  (** A description as to whether the register holds the value of a function
      parameter or local variable.  (Anonymous constants will count as
      local variables.) *)
  val is_parameter : t -> Is_parameter.t

  (** Any provenance information, which can be used for linking back to
      .cmt files, associated with the register.  This will be [None] except
      in the [Var] case. *)
  val provenance : t -> Backend_var.Provenance.t option
end

(** The type of a register with associated debugging information. Each value
    of type [t] holds an "underlying" [Reg.t] value. By the time this module
    is used, all such [Reg.t] values will describe hard registers or stack
    slots, and not unallocated pseudos. *)
type t

type reg_with_debug_info = t

(** Print a value of type [t] to a formatter. *)
val print
   : ?print_reg:(Format.formatter -> Reg.t -> unit)
  -> Format.formatter
  -> t
  -> unit

(** Create a value of type [t] giving full details of the associated
    debugging information. *)
val create
   : reg:Reg.t
  -> holds_value_of:Holds_value_of.t
  -> part_of_value:int
  -> num_parts_of_value:int
  -> Is_parameter.t
  -> provenance:Backend_var.Provenance.t option
  -> t

(** Create a value of type [t] describing the given [reg] with the given
    [debug_info] structure. *)
val create_with_debug_info : reg:Reg.t -> debug_info:Debug_info.t option -> t

(** Create a value of type [t] that has no debug info but is associated with
    the given hard register. *)
val create_without_debug_info : reg:Reg.t -> t

(** Create a value of type [t] whose debug info is that from [debug_info_from]
    but whose associated [Reg.t] is that specified by [reg]. *)
val create_copying_debug_info : reg:Reg.t -> debug_info_from:t -> t

(** Return the normal [Reg.t] value described by the given register with
    debug info. *)
val reg : t -> Reg.t

(** Where the register is located (a hard register, the stack, etc). *)
val location : t -> Reg.location

(** The debugging information associated with the register. *)
val debug_info : t -> Debug_info.t option

(** [at_same_location t reg] holds iff the register [t] corresponds to
    the same (physical or pseudoregister) location as the register [reg],
    which is not equipped with debugging information.
    [register_class] should be [Proc.register_class].
*)
val at_same_location : t -> Reg.t -> register_class:(Reg.t -> int) -> bool

(** Whether the register may hold a pointer value. *)
val maybe_holds_pointer : t -> bool

(** Whether the register always holds an immediate value. *)
val always_holds_non_pointer : t -> bool

(** [assigned_to_stack t] holds iff the location of [t] is a hard stack
    slot. *)
val assigned_to_stack : t -> bool

(** Return a new register that has empty debugging information but is
    otherwise like the supplied register with debug info. *)
val clear_debug_info : t -> t

(** Shared interfaces for non-canonical and canonical register sets
    (see below). *)
module type Set_intf = sig
  type t

  type reg_with_debug_info

  (** Print a value of type [t] to a formatter. *)
  val print
     : ?print_reg:(Format.formatter -> Reg.t -> unit)
    -> Format.formatter
    -> t
    -> unit

  (** Test for equality. *)
  val equal : t -> t -> bool

  (** The empty set. *)
  val empty : t

  (** Returns [true] iff the given set is empty. *)
  val is_empty : t -> bool

  (** Create a set from the given list of elements.  The list does not have
      to satisfy any canonical form criterion (see documentation for
      [Canonical_set], below). *)
  val of_list : reg_with_debug_info list -> t

  (** Like [of_list], but for arrays. *)
  val of_array : reg_with_debug_info array -> t

  (** Create a set of registers with debug info by taking the given set of
      [Reg.t] values and equipping them with empty debugging information. *)
  val without_debug_info : Reg.Set.t -> t

  (** Set intersection. *)
  val inter : t -> t -> t

  (** Set difference. *)
  val diff : t -> t -> t

  (** Retain only those elements of the given set for which the supplied
      predicate returns [true]. *)
  val filter : t -> f:(reg_with_debug_info -> bool) -> t

  (** Fold over all elements of the given set. *)
  val fold : t -> init:'a -> f:(reg_with_debug_info -> 'a -> 'a) -> 'a

  (** Test membership of the set given the underlying [Reg.t]. *)
  val mem_reg : t -> Reg.t -> bool

  (** Find an element of the set given the underlying [Reg.t]. *)
  val find_reg : t -> Reg.t -> reg_with_debug_info option

  (** Preserve only those members of the given set whose underlying [Reg.t]
      is that supplied. *)
  val filter_reg : t -> Reg.t -> t

  (** Return a set of the underlying [Reg.t] values. *)
  val forget_debug_info : t -> Reg.Set.t

  (** [made_unavailable_by_clobber t ~regs_clobbered ~register_class] returns
      the largest subset of [t] whose locations do not overlap with any
      registers in [regs_clobbered].  (Think of [t] as a set of available
      registers.)
      [register_class] should always be [Proc.register_class]. *)
  val made_unavailable_by_clobber
     : t
    -> regs_clobbered:Reg.t array
    -> register_class:(Reg.t -> int)
    -> t
end

(** Sets of registers with debug info.  Unlike [Canonical_set], below, there
    is no canonicalisation.

    Some functions in [Set] are not provided in [Canonical_set]---see the
    [Canonical_set] documentation below for why.

    The comparison function for the sets is [compare], from above.
*)
module Set : sig
  include Set_intf with type reg_with_debug_info := t

  (** The set containing only the given element. *)
  val singleton : reg_with_debug_info -> t

  (** Add a register to the given set. *)
  val add : t -> reg_with_debug_info -> t

  (** Set union. *)
  val union : t -> t -> t

  (** Map over elements of the given set. *)
  val map : t -> f:(reg_with_debug_info -> reg_with_debug_info) -> t

  (** Non-strict subset inclusion.  (This could be provided for [Canonical_set]
      but it seems like it would be a mistake to use it on such a set.) *)
  val subset : t -> t -> bool
end

(** Sets of registers with debug info, kept in a canonical form, guaranteeing
    for each set that it:
    (a) contains only registers that are associated with debug info; and
    (b) contains at most one register that holds the value of any given
        variable.

    Registers assigned to the stack are preferred if a choice has to be
    made to satisfy (b).

    Operations provided on these sets preserve the canonical form.
    In particular, there is no [union], since this might not.

    The comparison function for the sets is [compare], from above.
*)
module Canonical_set : sig
  include Set_intf with type reg_with_debug_info := t

  (** Canonicalise an existing set. *)
  val of_set : Set.t -> t

  (** Find the element of the set that holds the value of the given variable,
      if such exists, otherwise returning [None].  (Note that by virtue of the
      canonical form criterion, there can never be more than one variable
      eligible to be returned from any one call to this function.) *)
  val find_holding_value_of_variable
     : t
    -> Backend_var.t
    -> reg_with_debug_info option
end
