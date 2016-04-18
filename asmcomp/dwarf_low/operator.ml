(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | DW_op_addr of Dwarf_value.t
  | DW_op_regx of Dwarf_value.t
  | DW_op_fbreg of Dwarf_value.t
  | DW_op_bregx of
      { register : Dwarf_value.t; offset_in_bytes : Dwarf_value.t; }

let at_offset_from_symbol ~base:_ ~symbol ~offset_in_bytes =
  let value =
    Dwarf_value.Code_address_from_symbol_plus_bytes (symbol, offset_in_bytes)
  in
  DW_op_addr value

let at_symbol symbol =
  DW_op_addr (Dwarf_value.Code_address_from_symbol symbol)

let register ~reg_number =
  let reg_number = Dwarf_value.Uleb128 (Int64.of_int reg_number) in
  DW_op_regx reg_number

let register_based_addressing ~reg_number ~offset_in_bytes =
  let register = Dwarf_value.Uleb128 (Int64.of_int reg_number) in
  let offset_in_bytes =
    Dwarf_value.Sleb128 (Target_addr.to_int64 offset_in_bytes)
  in
  DW_op_bregx { register; offset_in_bytes; }

let frame_base_register ~offset_in_bytes =
  let offset_in_bytes =
    Dwarf_value.Sleb128 (Target_addr.to_int64 offset_in_bytes)
  in
  DW_op_fbreg offset_in_bytes

(* DWARF-4 spec section 7.7.1. *)
let opcode = function
  | DW_op_addr _ -> 0x03
  | DW_op_regx _ -> 0x90
  | DW_op_fbreg _ -> 0x91
  | DW_op_bregx _ -> 0x92

let size t =
  let opcode_size = Int64.of_int 1 in
  let args_size =
    match t with
    | DW_op_addr addr -> Dwarf_value.size addr
    | DW_op_regx reg_number -> Dwarf_value.size reg_number
    | DW_op_fbreg offset -> Dwarf_value.size offset
    | DW_op_bregx { register; offset_in_bytes; } ->
      Int64.add (Dwarf_value.size register)
        (Dwarf_value.size offset_in_bytes)
  in
  Int64.add opcode_size args_size

let emit t asm =
  Dwarf_value.emit (Dwarf_value.Int8 (Int8.of_int_exn (opcode t))) asm;
  match t with
  | DW_op_addr addr -> Dwarf_value.emit addr asm
  | DW_op_regx reg_number -> Dwarf_value.emit reg_number asm
  | DW_op_fbreg offset -> Dwarf_value.emit offset asm
  | DW_op_bregx { register; offset_in_bytes; } ->
    Dwarf_value.emit register asm;
    Dwarf_value.emit offset_in_bytes asm
