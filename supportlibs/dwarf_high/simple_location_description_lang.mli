(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Higher-level representation of simple location descriptions than that
    provided by the DWARF standard.  This representation is compiled down
    to [Simple_location_description.t] values.

    Functions in this interface are used to build descriptions that enable
    the debugger to know what value a particular variable has at runtime.
    This is done by either giving the value itself ("rvalue" semantics) or
    explaining how to compute the address in the target's memory where the
    value may be found ("lvalue" semantics).  Which of these semantics is
    appropriate depends on the context in which the simple location
    description is being used.  (For example see the DWARF-4 standard
    section 2.6.1.1.3 bullet point 2.)

    The descriptions of the functions are written in terms of some fictional
    value V and time T.  The time is when the simple location description is
    being evaluated in the debugger to inspect V.
*)

type t

(** "A piece or all of an object that is present in the source but not in
    the object code" (DWARF-4 standard 2.6.1.1.4). *)
val empty : t

module Lvalue : sig
  (** V will be in the given register at time T. *)
  val in_register : Dwarf_register_number.t -> t

  (** V will be in the given stack slot at time T. *)
  val in_stack_slot : offset_in_words:int -> t

  (** V will be in the given field of the given symbol at time T. *)
  val read_symbol_field : symbol:Symbol.t -> field:int -> t
end

module Rvalue : sig
  (** V is the OCaml tagged encoding of the given constant integer. *)
  val const_int : Int64.t -> t

  (** V is the given constant integer. *)
  val const_int_not_ocaml_encoded : Int64.t -> t

  (** V is the address of the given symbol. *)
  val const_symbol : Symbol.t -> t

  (** V will be in the given register at time T. *)
  val in_register : Dwarf_register_number.t -> t

  (** V will be in the given stack slot at time T. *)
  val in_stack_slot : offset_in_words:int -> t

  (** V will be in the given field of the block given by the provided
      simple location description at time T. *)
  val read_field : t -> field:int -> t

  (** V will be in the given field of the given symbol at time T. *)
  val read_symbol_field : symbol:Symbol.t -> field:int -> t

  (** V will be the given offset added to the value of the given simple
      location description at time T. *)
  val offset_pointer : t -> offset_in_words:int -> t
end

(** The value or location of V is to be found by using the instructions in
    the DIE at the given [die_label]. *)
val location_from_another_die
   : die_label:Cmm.label
  -> compilation_unit_header_label:Linearize.label
  -> t

(** V does not exist in the target's memory but behaves as a pointer to a
    value that can be reconstructed using the instructions in the DIE found
    at the given [die_label]. *)
val implicit_pointer
   : offset_in_bytes:int
  -> die_label:Cmm.label
  -> dwarf_version:Dwarf_version.t
  -> t

(** Transform the high-level location description into a stream of DWARF
    operators forming a DWARF simple location description. *)
val compile : t -> Simple_location_description.t
