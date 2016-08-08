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
    mutable interf: t list;              (* Other regs live simultaneously *)
    mutable prefer: (t * int) list;      (* Preferences for other regs *)
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

val all_immutable : t list -> bool

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
val all_registers: unit -> t list
val num_registers: unit -> int
val reinit: unit -> unit
