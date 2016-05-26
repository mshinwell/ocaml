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

type shared =
  { mutability : Cmm.mutability;        (* Whether contents are mutable *)
    stamp: int;                         (* Unique stamp *)
    mutable typ: Cmm.machtype_component;(* Type of contents *)
    mutable loc: location;              (* Actual location *)
    mutable spill: bool;                (* "true" to force stack allocation  *)
    mutable part: int option;           (* Zero-based index of part of value *)
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

type t = {
  mutable name : Ident.t option;
  (** [name] specifies any identifier corresponding to this register in the
      source code of the program being compiled. *)
  shared : shared;
  (** Multiple values of type [t] may share the same underlying data, with
      the exception of the name.  This enables us to have fast update of
      registers' properties (such as locations by the register allocator)
      and also fine-grained tracking of names. *)
}

val dummy: t
val create: ?mutability:Cmm.mutability -> Cmm.machtype_component -> t
val createv: Cmm.machtype -> t array
val createv_like: ?mutability:Cmm.mutability -> t array -> t array
val clone: t -> t
val at_location: Cmm.machtype_component -> location -> t

(** Whether the register might hold the value of a mutable Cmm variable. *)
val immutable : t -> bool

(** Whether we have a name for the register that corresponds to a name in
    the source text of the program being compiled.

    Beware: In previous versions of the compiler, [anonymous] was instead a
    property relating to whether the register held the value of a mutable
    Cmm variable.
*)
val anonymous : t -> bool

(** [anonymise t] creates a new register with no name that shares the
    underlying properties of [t].  When [t] changes (e.g. the register
    allocator assigns it a location), so does the new register. *)
val anonymise : t -> t

(** [identical_except_in_namev t ~take_names_from] takes registers and names
    elementwise from [t] and [take_names_from] respectively and returns a new
    array containing copies of the registers with the new names (save that if
    a particular register in [take_names_from] is anonymous, its name is not
    transferred).  The new registers share the underlying properties of
    the input registers. *)
val identical_except_in_namev : t array -> take_names_from:t array -> t array

(* Like [identical_except_in_namev] except just for one register. *)
val identical_except_in_name : t -> take_name_from:t -> t

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
