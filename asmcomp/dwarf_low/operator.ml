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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type implicit_value =
  | Int of int
  | Symbol of { symbol : Symbol.t; offset_in_bytes : Target_addr.t; }

type t =
  | DW_op_addr of Dwarf_value.t
  | DW_op_regx of Dwarf_value.t
  | DW_op_fbreg of Dwarf_value.t
  | DW_op_bregx of
      { register : Dwarf_value.t; offset_in_bytes : Dwarf_value.t; }
  | DW_op_implicit_value of implicit_value

let _ = DW_op_addr Dwarf_value.Flag_true  (* silence compiler warning *)

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

external caml_string_set32 : bytes -> index:int -> Int32.t -> unit
  = "%caml_string_set32"

external caml_string_set64 : bytes -> index:int -> Int64.t -> unit
  = "%caml_string_set64"

let implicit imp = DW_op_implicit_value imp

let at_offset_from_symbol ~base:_ ~symbol ~offset_in_bytes =
  DW_op_implicit_value (Symbol { symbol; offset_in_bytes; })

let at_symbol symbol =
  DW_op_implicit_value (Symbol { symbol; offset_in_bytes = Target_addr.zero; })

(* DWARF-4 spec section 7.7.1. *)
let opcode = function
  | DW_op_addr _ -> 0x03
  | DW_op_regx _ -> 0x90
  | DW_op_fbreg _ -> 0x91
  | DW_op_bregx _ -> 0x92
  | DW_op_implicit_value _ -> 0x9e

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
    | DW_op_implicit_value (Int _) ->
      let size_int = Int64.of_int Arch.size_int in
      Int64.add (Dwarf_value.size (Sleb128 size_int)) size_int
    | DW_op_implicit_value (Symbol _) ->
      let size_addr = Int64.of_int Arch.size_addr in
      Int64.add (Dwarf_value.size (Sleb128 size_addr)) size_addr
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
  | DW_op_implicit_value (Int i) ->
    (* The buffer must contain the integer as an OCaml value. *)
    let i = (i lsl 1) lor 1 in
    let buf =
      match Arch.size_int with
      | 4 ->
        let buf = Bytes.create 4 in
        caml_string_set32 buf ~index:0 (Int32.of_int i);
        buf
      | 8 ->
        let buf = Bytes.create 8 in
        caml_string_set64 buf ~index:0 (Int64.of_int i);
        buf
      | n ->
        Misc.fatal_errorf "Dwarf_low.Operator: bad Arch.size_int = %d" n
    in
    Dwarf_value.emit (Sleb128 (Int64.of_int (Bytes.length buf))) asm;
    Dwarf_value.emit (String (Bytes.to_string buf)) asm
  | DW_op_implicit_value (Symbol { symbol; offset_in_bytes; }) ->
    Dwarf_value.emit (Sleb128 (Int64.of_int Arch.size_addr)) asm;
    Dwarf_value.emit
      (Code_address_from_symbol_plus_bytes (symbol, offset_in_bytes))
      asm
