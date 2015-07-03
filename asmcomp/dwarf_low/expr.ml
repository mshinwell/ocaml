(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Group, LLC                             *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

type register_number = int

type t =
  | DW_OP_addr of Target_addr.t
  | DW_OP_deref
  | DW_OP_const1u of Int8.t
  | DW_OP_const1s of Int8.t
  | DW_OP_const2u of Int16.t
  | DW_OP_const2s of Int16.t
  | DW_OP_const4u of Int32.t
  | DW_OP_const4s of Int32.t
  | DW_OP_const8u of Int64.t
  | DW_OP_const8s of Int64.t
  | DW_OP_constu of Int64.t
  | DW_OP_consts of Int64.t
  | DW_OP_du
  | DW_OP_drop
  | DW_OP_over
  | DW_OP_pick of Int8.t
  | DW_OP_swap
  | DW_OP_rot
  | DW_OP_xderef
  | DW_OP_abs
  | DW_OP_and
  | DW_OP_div
  | DW_OP_minus
  | DW_OP_mod
  | DW_OP_mul
  | DW_OP_neg
  | DW_OP_not
  | DW_OP_or
  | DW_OP_plus
  | DW_OP_plus_uconst of Int64.t
  | DW_OP_shl
  | DW_OP_shr
  | DW_OP_shra
  | DW_OP_xor
  | DW_OP_skip of Int16.t
  | DW_OP_bra of Int16.t
  | DW_OP_eq
  | DW_OP_ge
  | DW_OP_gt
  | DW_OP_le
  | DW_OP_lt
  | DW_OP_ne
  | DW_OP_lit0
  | DW_OP_lit1
  | DW_OP_lit2
  | DW_OP_lit3
  | DW_OP_lit4
  | DW_OP_lit5
  | DW_OP_lit6
  | DW_OP_lit7
  | DW_OP_lit8
  | DW_OP_lit9
  | DW_OP_lit10
  | DW_OP_lit11
  | DW_OP_lit12
  | DW_OP_lit13
  | DW_OP_lit14
  | DW_OP_lit15
  | DW_OP_lit16
  | DW_OP_lit17
  | DW_OP_lit18
  | DW_OP_lit19
  | DW_OP_lit20
  | DW_OP_lit21
  | DW_OP_lit22
  | DW_OP_lit23
  | DW_OP_lit24
  | DW_OP_lit25
  | DW_OP_lit26
  | DW_OP_lit27
  | DW_OP_lit28
  | DW_OP_lit29
  | DW_OP_lit30
  | DW_OP_lit31
  | DW_OP_reg0
  | DW_OP_reg1
  | DW_OP_reg2
  | DW_OP_reg3
  | DW_OP_reg4
  | DW_OP_reg5
  | DW_OP_reg6
  | DW_OP_reg7
  | DW_OP_reg8
  | DW_OP_reg9
  | DW_OP_reg10
  | DW_OP_reg11
  | DW_OP_reg12
  | DW_OP_reg13
  | DW_OP_reg14
  | DW_OP_reg15
  | DW_OP_reg16
  | DW_OP_reg17
  | DW_OP_reg18
  | DW_OP_reg19
  | DW_OP_reg20
  | DW_OP_reg21
  | DW_OP_reg22
  | DW_OP_reg23
  | DW_OP_reg24
  | DW_OP_reg25
  | DW_OP_reg26
  | DW_OP_reg27
  | DW_OP_reg28
  | DW_OP_reg29
  | DW_OP_reg30
  | DW_OP_reg31
  | DW_OP_breg0 of Target_addr.t
  | DW_OP_breg1 of Target_addr.t
  | DW_OP_breg2 of Target_addr.t
  | DW_OP_breg3 of Target_addr.t
  | DW_OP_breg4 of Target_addr.t
  | DW_OP_breg5 of Target_addr.t
  | DW_OP_breg6 of Target_addr.t
  | DW_OP_breg7 of Target_addr.t
  | DW_OP_breg8 of Target_addr.t
  | DW_OP_breg9 of Target_addr.t
  | DW_OP_breg10 of Target_addr.t
  | DW_OP_breg11 of Target_addr.t
  | DW_OP_breg12 of Target_addr.t
  | DW_OP_breg13 of Target_addr.t
  | DW_OP_breg14 of Target_addr.t
  | DW_OP_breg15 of Target_addr.t
  | DW_OP_breg16 of Target_addr.t
  | DW_OP_breg17 of Target_addr.t
  | DW_OP_breg18 of Target_addr.t
  | DW_OP_breg19 of Target_addr.t
  | DW_OP_breg20 of Target_addr.t
  | DW_OP_breg21 of Target_addr.t
  | DW_OP_breg22 of Target_addr.t
  | DW_OP_breg23 of Target_addr.t
  | DW_OP_breg24 of Target_addr.t
  | DW_OP_breg25 of Target_addr.t
  | DW_OP_breg26 of Target_addr.t
  | DW_OP_breg27 of Target_addr.t
  | DW_OP_breg28 of Target_addr.t
  | DW_OP_breg29 of Target_addr.t
  | DW_OP_breg30 of Target_addr.t
  | DW_OP_breg31 of Target_addr.t
  | DW_OP_regx of register_number
  | DW_OP_fbreg of Target_addr.t
  | DW_OP_bregx of register_number * Target_addr.t
  | DW_OP_piece of Int64.t
  | DW_OP_deref_size of Int8.t
  | DW_OP_xderef_size of Int8.t
  | DW_OP_nop
  | DW_OP_push_object_address
  (* DW_OP_call* take the offset of a DIE within the current compilation
     unit. *)
  | DW_OP_call2 of Int16.t
  | DW_OP_call4 of Int32.t
  | DW_OP_call_ref of ??? (4 or 8 byte offset)
  | DW_OP_form_tls_address
  | DW_OP_call_frame_cfa
  | DW_OP_bit_piece of Int64.t * Int64.t  (* size, offset *)
  | DW_OP_implicit_value of Int64.t * Block.t
  | DW_OP_stack_value

let dw_op_lo_user = 0xe0
let dw_op_hi_user = 0xff

let emit t ~emitter =
  match t with
  | Addr addr ->
    Emitter.emit_byte_exn emitter 0x03;
    Emitter.emit_native_int emitter addr
  | Deref -> Emitter.emit_byte_exn emitter 0x06
  | Const1u c ->
    Emitter.emit_byte_exn emitter 0x08;
    Emitter.emit_byte_exn emitter c
  | Const1s c ->
    Emitter.emit_byte_exn emitter 0x09;
    Emitter.emit_byte_exn emitter c
  | Const2u c ->
    Emitter.emit_byte_exn emitter 0x0a;
    Emitter.emit_two_byte_int emitter c
  | Const2s c ->
    Emitter.emit_byte_exn emitter 0x0b;
    Emitter.emit_two_byte_int emitter c
  | Const4u c ->
    Emitter.emit_byte_exn emitter 0x0c;
    Emitter.emit_four_byte_int emitter c
  | Const4s c ->
    Emitter.emit_byte_exn emitter 0x0d;
    Emitter.emit_four_byte_int emitter c
  | Const8u c ->
    Emitter.emit_byte_exn emitter 0x0e;
    Stream.write_int64 stream c
  | Const8s c ->
    Emitter.emit_byte_exn emitter 0x0f;
    Stream.write_int64 stream c
  | Constu c ->
    Emitter.emit_byte_exn emitter 0x10;
    Emitter.emit_uleb128_from_int64 emitter c
  | Consts c ->
    Emitter.emit_byte_exn emitter 0x11;
    Emitter.emit_leb128_from_int64 emitter c
  | Dup -> Emitter.emit_byte_exn emitter 0x12
  | Drop -> Emitter.emit_byte_exn emitter 0x13
  | Over -> Emitter.emit_byte_exn emitter 0x14
  | Pick -> Emitter.write_byte emitter 0x15 (* check 0x15 *)
  | Swap -> Emitter.emit_byte_exn emitter 0x16
  | Rot -> Emitter.emit_byte_exn emitter 0x17
  | Xderef -> Emitter.emit_byte_exn emitter 0x18
  | Abs -> Emitter.emit_byte_exn emitter 0x19
  | And -> Emitter.emit_byte_exn emitter 0x1a
  | Div -> Emitter.emit_byte_exn emitter 0x1b
  | Minus -> Emitter.emit_byte_exn emitter 0x1c
  | Mod -> Emitter.emit_byte_exn emitter 0x1d
  | Mul -> Emitter.emit_byte_exn emitter 0x1e
  | Neg -> Emitter.emit_byte_exn emitter 0x1f
  | Not -> Emitter.emit_byte_exn emitter 0x20
  | Or -> Emitter.emit_byte_exn emitter 0x21
  | Plus -> Emitter.emit_byte_exn emitter 0x22
  | Plus_uconst addend ->
    Emitter.emit_byte_exn emitter 0x23;
    Emitter.emit_uleb128_from_int64 emitter addend
  | Shl -> Emitter.emit_byte_exn emitter 0x24
  | Shr -> Emitter.emit_byte_exn emitter 0x25
  | Shra -> Emitter.emit_byte_exn emitter 0x26
  | Xor -> Emitter.emit_byte_exn emitter 0x27
  | Skip c ->
    Emitter.emit_byte_exn emitter 0x2f;
    Emitter.emit_two_byte_int emitter c
  | Bra ->
    Emitter.emit_byte_exn emitter 0x28;
    Emitter.emit_two_byte_int emitter c
  | Eq -> Emitter.emit_byte_exn emitter 0x29
  | Ge -> Emitter.emit_byte_exn emitter 0x2a
  | Gt -> Emitter.emit_byte_exn emitter 0x2b
  | Le -> Emitter.emit_byte_exn emitter 0x2c
  | Lt -> Emitter.emit_byte_exn emitter 0x2d
  | Ne -> Emitter.emit_byte_exn emitter 0x2e
  | Lit0 -> Emitter.emit_byte_exn emitter 0x30
  | Lit1 -> Emitter.emit_byte_exn emitter 0x31
  | Lit2 -> Emitter.emit_byte_exn emitter 0x32
  | Lit3 -> Emitter.emit_byte_exn emitter 0x33
  | Lit4 -> Emitter.emit_byte_exn emitter 0x34
  | Lit5 -> Emitter.emit_byte_exn emitter 0x35
  | Lit6 -> Emitter.emit_byte_exn emitter 0x36
  | Lit7 -> Emitter.emit_byte_exn emitter 0x37
  | Lit8 -> Emitter.emit_byte_exn emitter 0x38
  | Lit9 -> Emitter.emit_byte_exn emitter 0x39
  | Lit10 -> Emitter.emit_byte_exn emitter 0x3a
  | Lit11 -> Emitter.emit_byte_exn emitter 0x3b
  | Lit12 -> Emitter.emit_byte_exn emitter 0x3c
  | Lit13 -> Emitter.emit_byte_exn emitter 0x3d
  | Lit14 -> Emitter.emit_byte_exn emitter 0x3e
  | Lit15 -> Emitter.emit_byte_exn emitter 0x3f
  | Lit16 -> Emitter.emit_byte_exn emitter 0x40
  | Lit17 -> Emitter.emit_byte_exn emitter 0x41
  | Lit18 -> Emitter.emit_byte_exn emitter 0x42
  | Lit19 -> Emitter.emit_byte_exn emitter 0x43
  | Lit20 -> Emitter.emit_byte_exn emitter 0x44
  | Lit21 -> Emitter.emit_byte_exn emitter 0x45
  | Lit22 -> Emitter.emit_byte_exn emitter 0x46
  | Lit23 -> Emitter.emit_byte_exn emitter 0x47
  | Lit24 -> Emitter.emit_byte_exn emitter 0x48
  | Lit25 -> Emitter.emit_byte_exn emitter 0x49
  | Lit26 -> Emitter.emit_byte_exn emitter 0x4a
  | Lit27 -> Emitter.emit_byte_exn emitter 0x4b
  | Lit28 -> Emitter.emit_byte_exn emitter 0x4c
  | Lit29 -> Emitter.emit_byte_exn emitter 0x4d
  | Lit30 -> Emitter.emit_byte_exn emitter 0x4e
  | Lit31 -> Emitter.emit_byte_exn emitter 0x4f
  | Reg0 -> Emitter.emit_byte_exn emitter 0x50
  | Reg1 -> Emitter.emit_byte_exn emitter 0x51
  | Reg2 -> Emitter.emit_byte_exn emitter 0x52
  | Reg3 -> Emitter.emit_byte_exn emitter 0x53
  | Reg4 -> Emitter.emit_byte_exn emitter 0x54
  | Reg5 -> Emitter.emit_byte_exn emitter 0x55
  | Reg6 -> Emitter.emit_byte_exn emitter 0x56
  | Reg7 -> Emitter.emit_byte_exn emitter 0x57
  | Reg8 -> Emitter.emit_byte_exn emitter 0x58
  | Reg9 -> Emitter.emit_byte_exn emitter 0x59
  | Reg10 -> Emitter.emit_byte_exn emitter 0x5a
  | Reg11 -> Emitter.emit_byte_exn emitter 0x5b
  | Reg12 -> Emitter.emit_byte_exn emitter 0x5c
  | Reg13 -> Emitter.emit_byte_exn emitter 0x5d
  | Reg14 -> Emitter.emit_byte_exn emitter 0x5e
  | Reg15 -> Emitter.emit_byte_exn emitter 0x5f
  | Reg16 -> Emitter.emit_byte_exn emitter 0x60
  | Reg17 -> Emitter.emit_byte_exn emitter 0x61
  | Reg18 -> Emitter.emit_byte_exn emitter 0x62
  | Reg19 -> Emitter.emit_byte_exn emitter 0x63
  | Reg20 -> Emitter.emit_byte_exn emitter 0x64
  | Reg21 -> Emitter.emit_byte_exn emitter 0x65
  | Reg22 -> Emitter.emit_byte_exn emitter 0x66
  | Reg23 -> Emitter.emit_byte_exn emitter 0x67
  | Reg24 -> Emitter.emit_byte_exn emitter 0x68
  | Reg25 -> Emitter.emit_byte_exn emitter 0x69
  | Reg26 -> Emitter.emit_byte_exn emitter 0x6a
  | Reg27 -> Emitter.emit_byte_exn emitter 0x6b
  | Reg28 -> Emitter.emit_byte_exn emitter 0x6c
  | Reg29 -> Emitter.emit_byte_exn emitter 0x6d
  | Reg30 -> Emitter.emit_byte_exn emitter 0x6e
  | Reg31 -> Emitter.emit_byte_exn emitter 0x6f
  | Breg0 offset ->
    Emitter.emit_byte_exn emitter 0x70;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg1 offset ->
    Emitter.emit_byte_exn emitter 0x71;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg2 offset ->
    Emitter.emit_byte_exn emitter 0x72;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg3 offset ->
    Emitter.emit_byte_exn emitter 0x73;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg4 offset ->
    Emitter.emit_byte_exn emitter 0x74;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg5 offset ->
    Emitter.emit_byte_exn emitter 0x75;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg6 offset ->
    Emitter.emit_byte_exn emitter 0x76;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg7 offset ->
    Emitter.emit_byte_exn emitter 0x77;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg8 offset ->
    Emitter.emit_byte_exn emitter 0x78;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg9 offset ->
    Emitter.emit_byte_exn emitter 0x79;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg10 offset ->
    Emitter.emit_byte_exn emitter 0x7a;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg11 offset ->
    Emitter.emit_byte_exn emitter 0x7b;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg12 offset ->
    Emitter.emit_byte_exn emitter 0x7c;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg13 offset ->
    Emitter.emit_byte_exn emitter 0x7d;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg14 offset ->
    Emitter.emit_byte_exn emitter 0x7e;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg15 offset ->
    Emitter.emit_byte_exn emitter 0x7f;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg16 offset ->
    Emitter.emit_byte_exn emitter 0x80;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg17 offset ->
    Emitter.emit_byte_exn emitter 0x81;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg18 offset ->
    Emitter.emit_byte_exn emitter 0x82;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg19 offset ->
    Emitter.emit_byte_exn emitter 0x83;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg20 offset ->
    Emitter.emit_byte_exn emitter 0x84;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg21 offset ->
    Emitter.emit_byte_exn emitter 0x85;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg22 offset ->
    Emitter.emit_byte_exn emitter 0x86;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg23 offset ->
    Emitter.emit_byte_exn emitter 0x87;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg24 offset ->
    Emitter.emit_byte_exn emitter 0x88;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg25 offset ->
    Emitter.emit_byte_exn emitter 0x89;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg26 offset ->
    Emitter.emit_byte_exn emitter 0x8a;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg27 offset ->
    Emitter.emit_byte_exn emitter 0x8b;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg28 offset ->
    Emitter.emit_byte_exn emitter 0x8c;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg29 offset ->
    Emitter.emit_byte_exn emitter 0x8d;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg30 offset ->
    Emitter.emit_byte_exn emitter 0x8e;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Breg31 offset ->
    Emitter.emit_byte_exn emitter 0x8f;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Regx reg ->
    Emitter.emit_byte_exn emitter 0x90;
    Emitter.emit_uleb128 emitter reg
  | Fbreg offset ->
    Emitter.emit_byte_exn emitter 0x91;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Bregx (reg, offset) ->
    Emitter.emit_byte_exn emitter 0x92;
    Emitter.emit_uleb128 emitter reg;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | Piece size ->
    Emitter.emit_byte_exn emitter 0x93;
    Emitter.emit_uleb128_from_int64 emitter size
  | Deref_size size ->
    Emitter.emit_byte_exn emitter 0x94;
    Emitter.emit_byte_exn emitter size
  | Xderef_size ->
    Emitter.emit_byte_exn emitter 0x95;
    Emitter.emit_byte_exn emitter size
  | Nop -> Emitter.emit_byte_exn emitter 0x96
  | Push_object_address -> Emitter.emit_byte_exn emitter 0x97
  | Call2 offset_of_die ->
    Emitter.emit_byte_exn emitter 0x98;
    Emitter.emit_two_byte_int emitter offset_of_die
  | Call4 offset_of_die ->
    Emitter.emit_byte_exn emitter 0x99;
    Emitter.emit_four_byte_int emitter offset_of_die
  | Call_ref offset_of_die ->
    Emitter.emit_byte_exn emitter 0x9a;
    Stream.write_???(32 or 64) stream offset_of_die
  | Form_tls_address -> Emitter.emit_byte_exn emitter 0x9b
  | Call_frame_cfa -> Emitter.emit_byte_exn emitter 0x9c
  | Bit_piece (size, offset) ->
    Emitter.emit_byte_exn emitter 0x9d;
    Emitter.emit_uleb128_from_int64 emitter size;
    Emitter.emit_uleb128_from_int64 emitter offset
  | Implicit_value (size, block) ->
    Emitter.emit_byte_exn emitter 0x9e;
    Emitter.emit_uleb128_from_int64 emitter size;
    Block.write block ~stream
  | Stack_value -> Emitter.emit_byte_exn emitter 0x9f

let literal_for_int64 =
  let table = [|
    Lit0; Lit1; Lit2; Lit3; Lit4;
    Lit5; Lit6; Lit7; Lit8; Lit9;
    Lit10; Lit11; Lit12; Lit13; Lit14;
    Lit15; Lit16; Lit17; Lit18; Lit19;
    Lit20; Lit21; Lit22; Lit23; Lit24;
    Lit25; Lit26; Lit27; Lit28; Lit29;
    Lit30; Lit31;
  |] in
  fun c ->
    if Int64.compare c 0L >= 0 && Int64.compare c 31L <= 0 then begin
      Some table.(Int64.to_int c)
    end else begin
      None
    end
