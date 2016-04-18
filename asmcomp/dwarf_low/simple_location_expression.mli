(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR-soon mshinwell: Shouldn't this module be called
   [Simple_location_description]?  (DWARF-4 spec 2.6.1) *)

type t

include Emittable.S with type t := t

(* The address of [symbol + offset_in_bytes] relative to the symbol [base]. *)
(* CR mshinwell: delete "base" argument *)
val at_offset_from_symbol
   : base:string
  -> symbol:Symbol.t
  -> offset_in_bytes:Target_addr.t
  -> t

val at_symbol : Symbol.t -> t

(* CR-soon mshinwell: consider a new type to identify whose register
   numbering is in use here *)
val in_register : reg_number:int -> t

val register_based_addressing
   : reg_number:int
  -> offset_in_bytes:Target_addr.t
  -> t

val frame_base_register : offset_in_bytes:Target_addr.t -> t
