(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module SLE = Simple_location_expression

type t =
  | Immediate of SLE.t  (* will do for the moment *)
  | Computed of (unit -> SLE.t)

let in_register ~reg =
  let reg_number = Proc.dwarf_register_number reg in
  let sle = SLE.in_register ~reg_number in
  Immediate sle

let at_offset_from_register ~reg ~offset_in_bytes =
  let reg_number = Proc.dwarf_register_number reg in
  let sle =
    SLE.register_based_addressing ~reg_number ~offset_in_bytes
  in
  Immediate sle

let at_offset_from_frame_pointer ~offset_in_bytes =
  Immediate (SLE.frame_base_register ~offset_in_bytes)

let at_offset_from_stack_pointer ~offset_in_bytes =
  let sle =
    SLE.register_based_addressing
      ~reg_number:Proc.stack_ptr_dwarf_register_number
      ~offset_in_bytes
  in
  Immediate sle

let at_computed_offset_from_stack_pointer ~offset_in_bytes =
  let sle () =
    SLE.register_based_addressing
      ~reg_number:Proc.stack_ptr_dwarf_register_number
      ~offset_in_bytes:(offset_in_bytes ())
  in
  Computed sle

let at_symbol symbol = Immediate (SLE.at_symbol symbol)

let implicit imp = Immediate (SLE.implicit imp)

let sle t =
  match t with
  | Immediate sle -> sle
  | Computed sle -> sle ()

let size t =
  SLE.size (sle t)

let emit t asm =
  SLE.emit (sle t) asm
