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
  (* A value of type [t] forms the core of a description, for informational
     and debug info generation purposes, of the contents of a register.
     (Certain extra information, such as whether a register contains a
     spilled value or only part of a value, is stored within the value of
     type [Reg.t] instead.) *)
  type t

  val create_anon : unit -> t
  val create_from_ident : Ident.t -> t

  val augment : t -> new_name:t -> t

  (* CR mshinwell: proper (de)serializers for names, which we can use on
     the debugger side *)
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
val create_procedure_call_convention: Cmm.machtype_component -> location -> t
val createv: Cmm.machtype -> t array
val createv_like: t array -> t array
val clone: t -> t
(* [identical_except_in_name t ~from] returns a copy of [t] that is the same in
   all respects, including the stamp, as [t]---but has the raw name of
   [from]. *)
val identical_except_in_name : t -> from:t -> t
val identical_except_in_namev : t array -> from:t array -> t array

(* If [immutable t] is [false] then the register [t] might hold a value that
   can be mutated (using [Cassign], for example a [for] loop counter). *)
val immutable : t -> bool

(* [immutable_and_anonymous t] being [true] tells us that the register [t] is
   immutable (as above) and that we don't yet have any naming information as to
   which particular value is stored within it.

   The "anonymity" part of this should not be used for making code
   generation decisions that affect semantics (whereas "immutability", above,
   may be).
*)
val immutable_and_anonymous : t -> bool

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
