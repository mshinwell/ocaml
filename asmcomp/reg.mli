(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Hard and pseudo-registers *)

module Raw_name : sig
  (* A value of type [t] is a description, for informational and
     debug info generation purposes, of the contents of a register. *)
  type t

  val create_from_ident : Ident.t -> t
  val create_from_symbol : string -> t
  val create_from_blockheader : nativeint -> t
  val create_pointer_to_uninitialized_block : unit -> t

  (* [augmented_with_displacement t ~words] is an appropriate name for
     a register containing the result of a memory load where the base
     address was the contents of a register with name [t] and the
     displacement applied to said address was [words]. *)
  val augmented_with_displacement : t -> words:int -> t

  (* [do_not_propagate t] being [true] indicates that a move from a
     register [r] with name [t] into a register [r'] should not update
     the name of [r'].  This is used to ensure that "uninitialized block"
     register names do not appear after the block has been initialized. *)
  val do_not_propagate : t -> bool
end

(* Values of type [t] represent either hard registers or stack slots (in
   the case where [loc] is not [Unknown]) or pseudoregisters yet to be
   assigned.  There may be multiple values of type [t] with the same stamp
   but different [raw_name]s (for example, hard registers used for function
   arguments and results---see selectgen.ml). *)
type t =
  { mutable raw_name: Raw_name.t;       (* Name *)
    stamp: int;                         (* Unique stamp *)
    typ: Cmm.machtype_component;        (* Type of contents *)
    mutable loc: location;              (* Actual location *)
    mutable spill: bool;                (* "true" to force stack allocation  *)
    mutable part: int option;           (* Zero-based index of part of value *)
    mutable interf: t list;             (* Other regs live simultaneously *)
    mutable prefer: (t * int) list;     (* Preferences for other regs *)
    mutable degree: int;                (* Number of other regs live sim. *)
    mutable spill_cost: int;            (* Estimate of spilling cost *)
    mutable visited: bool }             (* For graph walks *)

and location =
    Unknown
  | Reg of int
  | Stack of stack_location

and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int

val dummy: t
val create: Cmm.machtype_component -> t
val createv: Cmm.machtype -> t array
val createv_like: t array -> t array
val clone: t -> t
(* [identical_except_in_name t ~from] returns a copy of [t] that is the same in
   all respects, including the stamp, as [t]---but has the raw name of
   [from]. *)
val identical_except_in_name : t -> from:t -> t
val identical_except_in_namev : t array -> from:t array -> t array
val at_location: Cmm.machtype_component -> location -> t

(* If [immutable t] is [false] then the register [t] might hold a value that
   can be mutated (using [Cassign], for example a [for] loop counter). *)
val immutable : t -> bool

(* Name for printing *)
val name : t -> string

module Set: Set.S with type elt = t
module Map: Map.S with type key = t

val add_set_array: Set.t -> t array -> Set.t
val diff_set_array: Set.t -> t array -> Set.t
val inter_set_array: Set.t -> t array -> Set.t
val set_of_array: t array -> Set.t

val reset: unit -> unit
val all_registers: unit -> t list
val num_registers: unit -> int
val reinit: unit -> unit
