(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Pseudo-registers *)

type t =
  { mutable name: string;               (* Name (for printing) *)
    mutability : Cmm.mutability;        (* Whether contents are mutable *)
    stamp: int;                         (* Unique stamp *)
    mutable typ: Cmm.machtype_component;(* Type of contents *)
    mutable loc: location;              (* Actual location *)
    mutable spill: bool;                (* "true" to force stack allocation  *)
    mutable interf: shared list;         (* Other regs live simultaneously *)
    mutable prefer: (shared * int) list; (* Preferences for other regs *)
    mutable degree: int;                 (* Number of other regs live sim. *)
    mutable spill_cost: int;             (* Estimate of spilling cost *)
    mutable visited: bool }              (* For graph walks *)

and location =
    Unknown
  | Reg of int
  | Stack of stack_location

and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int

type reg = t

val dummy: t
val create: ?mutability:Cmm.mutability -> Cmm.machtype_component -> t
val createv: Cmm.machtype -> t array
val createv_like: ?mutability:Cmm.mutability -> t array -> t array

(** Create a fresh register with the following properties taken from the
    given register: [typ], [mutability] and [name]. *)
val clone: t -> t

val at_location: Cmm.machtype_component -> location -> t

(** Whether the register might hold the value of a mutable Cmm variable. *)
val immutable : t -> bool

(* Name for printing *)
val name : t -> string

module Set: Set.S with type elt = t
module Map: Map.S with type key = t

val add_set_array: Set.t -> t array -> Set.t
val diff_set_array: Set.t -> t array -> Set.t
val inter_set_array: Set.t -> t array -> Set.t
val disjoint_set_array: Set.t -> t array -> bool
val set_of_array: t array -> Set.t

val reset: unit -> unit
val all_registers: unit -> shared list
val num_registers: unit -> int
val reinit: unit -> unit

module With_debug_info : sig
  type t
  (** A register equipped with information used for generating debugging
      information. *)

  val create
     : reg:Reg.t
    -> holds_value_of:Ident.t
    -> part_of_value:int
    -> num_parts_of_value:int
    -> which_parameter:int option
    -> t

  val reg : t -> Reg.t
  val location : t -> location

  val holds_value_of : Ident.t -> t
  (** The identifier that the register holds (part of) the value of. *)

  val part_of_value : t -> int
  val num_parts_of_value : t -> int

  val which_parameter : t -> int option
  (** If the register corresponds to a function parameter, the value returned
      is the zero-based index of said parameter; otherwise it is [None]. *)

  val at_same_location : t -> reg -> bool
  (** [at_same_location t reg] holds iff the register [t] corresponds to
      the same (physical or pseudoregister) location as the register [reg],
      which is not equipped with debugging information. *)

  val holds_pointer : t -> bool
  val holds_non_pointer : t -> bool

  val assigned_to_stack : t -> bool
  (** [assigned_to_stack t] holds iff the location of [t] is a hard stack
      slot. *)

  val clear_debug_info : t -> t

  module Set : sig
    include Set.S with type elt = t

    val mem_reg : t -> reg -> bool

    val forget_debug_info : t -> Set.t

    val without_debug_info : Set.t -> t

    val made_unavailable_by_clobber : t -> regs_clobbered:Set.t -> t
    (** [made_unavailable_by_clobber t ~regs_clobbered] returns the largest
        subset of [t] whose locations do not overlap with any registers in
        [regs_clobbered].  (Think of [t] as a set of available registers.) *)
  end
end
