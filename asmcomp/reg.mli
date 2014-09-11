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

(* Pseudo-registers *)

module Raw_name : sig
  type t
  val create_ident : Ident.t -> t
  val is_ident : t -> bool
  val to_ident : t -> Ident.t option
  val (=) : t -> t -> bool
end

type t =
  { mutable raw_name: Raw_name.t;       (* Name *)
    stamp: int;                         (* Unique stamp *)
    typ: Cmm.machtype_component;        (* Type of contents *)
    mutable loc: location;              (* Actual location *)
    mutable spill: bool;                (* "true" to force stack allocation  *)
    mutable part: int option;           (* Zero-based index of part of value *)
    mutable is_parameter: bool;         (* Whether it holds a function arg. *)
    mutable interf: t list;             (* Other regs live simultaneously *)
    mutable prefer: (t * int) list;     (* Preferences for other regs *)
    mutable degree: int;                (* Number of other regs live sim. *)
    mutable spill_cost: int;            (* Estimate of spilling cost *)
    mutable visited: bool;              (* For graph walks *)
  }

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
val create_procedure_call_convention: Cmm.machtype_component -> location -> t

(* [identical_except_in_namev t ~from] takes registers and names elementwise
   from [t] and [from] respectively and returns a new array containing copies
   of the registers with the new names.  The registers' stamps are not changed.
   This is used for naming values of type [t] representing the hard registers
   and stack slots of procedure call conventions; see selectgen.ml. *)
val identical_except_in_namev : t array -> from:t array -> t array

(* Like [identical_except_in_namev] except just for one register. *)
val identical_except_in_name : t -> from:t -> t

(* A register being "temporary" means that it is not mapped in the environment
   as used during instruction selection.  Another way of thinking of this is
   that it is never the canonical location of the value of a given
   identifier. *)
val is_temporary : t -> bool

val is_procedure_call_convention : t -> bool

val holds_pointer : t -> bool
val holds_non_pointer : t -> bool

(* Name for printing *)
val name : t -> string

val compare_stamps : t -> t -> int
module Set: Set.S with type elt = t
module Map: Map.S with type key = t

val add_set_array: Set.t -> t array -> Set.t
val diff_set_array: Set.t -> t array -> Set.t
val inter_set_array: Set.t -> t array -> Set.t
val disjoint_set_array: Set.t -> t array -> bool
val set_of_array: t array -> Set.t

val reset: unit -> unit
val all_registers: unit -> t list
val num_registers: unit -> int
val reinit: unit -> unit
