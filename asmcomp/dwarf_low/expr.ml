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
  | DW_OP_addr addr ->
    Emitter.emit_byte_exn emitter 0x03;
    Emitter.emit_native_int emitter addr
  | DW_OP_deref -> Emitter.emit_byte_exn emitter 0x06
  | DW_OP_const1u c ->
    Emitter.emit_byte_exn emitter 0x08;
    Emitter.emit_byte_exn emitter c
  | DW_OP_const1s c ->
    Emitter.emit_byte_exn emitter 0x09;
    Emitter.emit_byte_exn emitter c
  | DW_OP_const2u c ->
    Emitter.emit_byte_exn emitter 0x0a;
    Emitter.emit_two_byte_int emitter c
  | DW_OP_const2s c ->
    Emitter.emit_byte_exn emitter 0x0b;
    Emitter.emit_two_byte_int emitter c
  | DW_OP_const4u c ->
    Emitter.emit_byte_exn emitter 0x0c;
    Emitter.emit_four_byte_int emitter c
  | DW_OP_const4s c ->
    Emitter.emit_byte_exn emitter 0x0d;
    Emitter.emit_four_byte_int emitter c
  | DW_OP_const8u c ->
    Emitter.emit_byte_exn emitter 0x0e;
    Stream.write_int64 stream c
  | DW_OP_const8s c ->
    Emitter.emit_byte_exn emitter 0x0f;
    Stream.write_int64 stream c
  | DW_OP_constu c ->
    Emitter.emit_byte_exn emitter 0x10;
    Emitter.emit_uleb128_from_int64 emitter c
  | DW_OP_consts c ->
    Emitter.emit_byte_exn emitter 0x11;
    Emitter.emit_leb128_from_int64 emitter c
  | DW_OP_dup -> Emitter.emit_byte_exn emitter 0x12
  | DW_OP_drop -> Emitter.emit_byte_exn emitter 0x13
  | DW_OP_over -> Emitter.emit_byte_exn emitter 0x14
  | DW_OP_pick -> Emitter.write_byte emitter 0x15 (* check 0x15 *)
  | DW_OP_swap -> Emitter.emit_byte_exn emitter 0x16
  | DW_OP_rot -> Emitter.emit_byte_exn emitter 0x17
  | DW_OP_xderef -> Emitter.emit_byte_exn emitter 0x18
  | DW_OP_abs -> Emitter.emit_byte_exn emitter 0x19
  | DW_OP_and -> Emitter.emit_byte_exn emitter 0x1a
  | DW_OP_div -> Emitter.emit_byte_exn emitter 0x1b
  | DW_OP_minus -> Emitter.emit_byte_exn emitter 0x1c
  | DW_OP_mod -> Emitter.emit_byte_exn emitter 0x1d
  | DW_OP_mul -> Emitter.emit_byte_exn emitter 0x1e
  | DW_OP_neg -> Emitter.emit_byte_exn emitter 0x1f
  | DW_OP_not -> Emitter.emit_byte_exn emitter 0x20
  | DW_OP_or -> Emitter.emit_byte_exn emitter 0x21
  | DW_OP_plus -> Emitter.emit_byte_exn emitter 0x22
  | DW_OP_plus_uconst addend ->
    Emitter.emit_byte_exn emitter 0x23;
    Emitter.emit_uleb128_from_int64 emitter addend
  | DW_OP_shl -> Emitter.emit_byte_exn emitter 0x24
  | DW_OP_shr -> Emitter.emit_byte_exn emitter 0x25
  | DW_OP_shra -> Emitter.emit_byte_exn emitter 0x26
  | DW_OP_xor -> Emitter.emit_byte_exn emitter 0x27
  | DW_OP_skip c ->
    Emitter.emit_byte_exn emitter 0x2f;
    Emitter.emit_two_byte_int emitter c
  | DW_OP_bra ->
    Emitter.emit_byte_exn emitter 0x28;
    Emitter.emit_two_byte_int emitter c
  | DW_OP_eq -> Emitter.emit_byte_exn emitter 0x29
  | DW_OP_ge -> Emitter.emit_byte_exn emitter 0x2a
  | DW_OP_gt -> Emitter.emit_byte_exn emitter 0x2b
  | DW_OP_le -> Emitter.emit_byte_exn emitter 0x2c
  | DW_OP_lt -> Emitter.emit_byte_exn emitter 0x2d
  | DW_OP_ne -> Emitter.emit_byte_exn emitter 0x2e
  | DW_OP_lit0 -> Emitter.emit_byte_exn emitter 0x30
  | DW_OP_lit1 -> Emitter.emit_byte_exn emitter 0x31
  | DW_OP_lit2 -> Emitter.emit_byte_exn emitter 0x32
  | DW_OP_lit3 -> Emitter.emit_byte_exn emitter 0x33
  | DW_OP_lit4 -> Emitter.emit_byte_exn emitter 0x34
  | DW_OP_lit5 -> Emitter.emit_byte_exn emitter 0x35
  | DW_OP_lit6 -> Emitter.emit_byte_exn emitter 0x36
  | DW_OP_lit7 -> Emitter.emit_byte_exn emitter 0x37
  | DW_OP_lit8 -> Emitter.emit_byte_exn emitter 0x38
  | DW_OP_lit9 -> Emitter.emit_byte_exn emitter 0x39
  | DW_OP_lit10 -> Emitter.emit_byte_exn emitter 0x3a
  | DW_OP_lit11 -> Emitter.emit_byte_exn emitter 0x3b
  | DW_OP_lit12 -> Emitter.emit_byte_exn emitter 0x3c
  | DW_OP_lit13 -> Emitter.emit_byte_exn emitter 0x3d
  | DW_OP_lit14 -> Emitter.emit_byte_exn emitter 0x3e
  | DW_OP_lit15 -> Emitter.emit_byte_exn emitter 0x3f
  | DW_OP_lit16 -> Emitter.emit_byte_exn emitter 0x40
  | DW_OP_lit17 -> Emitter.emit_byte_exn emitter 0x41
  | DW_OP_lit18 -> Emitter.emit_byte_exn emitter 0x42
  | DW_OP_lit19 -> Emitter.emit_byte_exn emitter 0x43
  | DW_OP_lit20 -> Emitter.emit_byte_exn emitter 0x44
  | DW_OP_lit21 -> Emitter.emit_byte_exn emitter 0x45
  | DW_OP_lit22 -> Emitter.emit_byte_exn emitter 0x46
  | DW_OP_lit23 -> Emitter.emit_byte_exn emitter 0x47
  | DW_OP_lit24 -> Emitter.emit_byte_exn emitter 0x48
  | DW_OP_lit25 -> Emitter.emit_byte_exn emitter 0x49
  | DW_OP_lit26 -> Emitter.emit_byte_exn emitter 0x4a
  | DW_OP_lit27 -> Emitter.emit_byte_exn emitter 0x4b
  | DW_OP_lit28 -> Emitter.emit_byte_exn emitter 0x4c
  | DW_OP_lit29 -> Emitter.emit_byte_exn emitter 0x4d
  | DW_OP_lit30 -> Emitter.emit_byte_exn emitter 0x4e
  | DW_OP_lit31 -> Emitter.emit_byte_exn emitter 0x4f
  | DW_OP_reg0 -> Emitter.emit_byte_exn emitter 0x50
  | DW_OP_reg1 -> Emitter.emit_byte_exn emitter 0x51
  | DW_OP_reg2 -> Emitter.emit_byte_exn emitter 0x52
  | DW_OP_reg3 -> Emitter.emit_byte_exn emitter 0x53
  | DW_OP_reg4 -> Emitter.emit_byte_exn emitter 0x54
  | DW_OP_reg5 -> Emitter.emit_byte_exn emitter 0x55
  | DW_OP_reg6 -> Emitter.emit_byte_exn emitter 0x56
  | DW_OP_reg7 -> Emitter.emit_byte_exn emitter 0x57
  | DW_OP_reg8 -> Emitter.emit_byte_exn emitter 0x58
  | DW_OP_reg9 -> Emitter.emit_byte_exn emitter 0x59
  | DW_OP_reg10 -> Emitter.emit_byte_exn emitter 0x5a
  | DW_OP_reg11 -> Emitter.emit_byte_exn emitter 0x5b
  | DW_OP_reg12 -> Emitter.emit_byte_exn emitter 0x5c
  | DW_OP_reg13 -> Emitter.emit_byte_exn emitter 0x5d
  | DW_OP_reg14 -> Emitter.emit_byte_exn emitter 0x5e
  | DW_OP_reg15 -> Emitter.emit_byte_exn emitter 0x5f
  | DW_OP_reg16 -> Emitter.emit_byte_exn emitter 0x60
  | DW_OP_reg17 -> Emitter.emit_byte_exn emitter 0x61
  | DW_OP_reg18 -> Emitter.emit_byte_exn emitter 0x62
  | DW_OP_reg19 -> Emitter.emit_byte_exn emitter 0x63
  | DW_OP_reg20 -> Emitter.emit_byte_exn emitter 0x64
  | DW_OP_reg21 -> Emitter.emit_byte_exn emitter 0x65
  | DW_OP_reg22 -> Emitter.emit_byte_exn emitter 0x66
  | DW_OP_reg23 -> Emitter.emit_byte_exn emitter 0x67
  | DW_OP_reg24 -> Emitter.emit_byte_exn emitter 0x68
  | DW_OP_reg25 -> Emitter.emit_byte_exn emitter 0x69
  | DW_OP_reg26 -> Emitter.emit_byte_exn emitter 0x6a
  | DW_OP_reg27 -> Emitter.emit_byte_exn emitter 0x6b
  | DW_OP_reg28 -> Emitter.emit_byte_exn emitter 0x6c
  | DW_OP_reg29 -> Emitter.emit_byte_exn emitter 0x6d
  | DW_OP_reg30 -> Emitter.emit_byte_exn emitter 0x6e
  | DW_OP_reg31 -> Emitter.emit_byte_exn emitter 0x6f
  | DW_OP_breg0 offset ->
    Emitter.emit_byte_exn emitter 0x70;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg1 offset ->
    Emitter.emit_byte_exn emitter 0x71;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg2 offset ->
    Emitter.emit_byte_exn emitter 0x72;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg3 offset ->
    Emitter.emit_byte_exn emitter 0x73;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg4 offset ->
    Emitter.emit_byte_exn emitter 0x74;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg5 offset ->
    Emitter.emit_byte_exn emitter 0x75;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg6 offset ->
    Emitter.emit_byte_exn emitter 0x76;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg7 offset ->
    Emitter.emit_byte_exn emitter 0x77;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg8 offset ->
    Emitter.emit_byte_exn emitter 0x78;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg9 offset ->
    Emitter.emit_byte_exn emitter 0x79;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg10 offset ->
    Emitter.emit_byte_exn emitter 0x7a;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg11 offset ->
    Emitter.emit_byte_exn emitter 0x7b;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg12 offset ->
    Emitter.emit_byte_exn emitter 0x7c;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg13 offset ->
    Emitter.emit_byte_exn emitter 0x7d;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg14 offset ->
    Emitter.emit_byte_exn emitter 0x7e;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg15 offset ->
    Emitter.emit_byte_exn emitter 0x7f;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg16 offset ->
    Emitter.emit_byte_exn emitter 0x80;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg17 offset ->
    Emitter.emit_byte_exn emitter 0x81;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg18 offset ->
    Emitter.emit_byte_exn emitter 0x82;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg19 offset ->
    Emitter.emit_byte_exn emitter 0x83;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg20 offset ->
    Emitter.emit_byte_exn emitter 0x84;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg21 offset ->
    Emitter.emit_byte_exn emitter 0x85;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg22 offset ->
    Emitter.emit_byte_exn emitter 0x86;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg23 offset ->
    Emitter.emit_byte_exn emitter 0x87;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg24 offset ->
    Emitter.emit_byte_exn emitter 0x88;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg25 offset ->
    Emitter.emit_byte_exn emitter 0x89;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg26 offset ->
    Emitter.emit_byte_exn emitter 0x8a;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg27 offset ->
    Emitter.emit_byte_exn emitter 0x8b;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg28 offset ->
    Emitter.emit_byte_exn emitter 0x8c;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg29 offset ->
    Emitter.emit_byte_exn emitter 0x8d;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg30 offset ->
    Emitter.emit_byte_exn emitter 0x8e;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_breg31 offset ->
    Emitter.emit_byte_exn emitter 0x8f;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_regx reg ->
    Emitter.emit_byte_exn emitter 0x90;
    Emitter.emit_uleb128 emitter reg
  | DW_OP_fbreg offset ->
    Emitter.emit_byte_exn emitter 0x91;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_bregx (reg, offset) ->
    Emitter.emit_byte_exn emitter 0x92;
    Emitter.emit_uleb128 emitter reg;
    Emitter.emit_sleb128_from_int64 emitter (Target_addr.to_int64 offset)
  | DW_OP_piece size ->
    Emitter.emit_byte_exn emitter 0x93;
    Emitter.emit_uleb128_from_int64 emitter size
  | DW_OP_deref_size size ->
    Emitter.emit_byte_exn emitter 0x94;
    Emitter.emit_byte_exn emitter size
  | DW_OP_xderef_size ->
    Emitter.emit_byte_exn emitter 0x95;
    Emitter.emit_byte_exn emitter size
  | DW_OP_nop -> Emitter.emit_byte_exn emitter 0x96
  | DW_OP_push_object_address -> Emitter.emit_byte_exn emitter 0x97
  | DW_OP_call2 offset_of_die ->
    Emitter.emit_byte_exn emitter 0x98;
    Emitter.emit_two_byte_int emitter offset_of_die
  | DW_OP_call4 offset_of_die ->
    Emitter.emit_byte_exn emitter 0x99;
    Emitter.emit_four_byte_int emitter offset_of_die
  | DW_OP_call_ref offset_of_die ->
    Emitter.emit_byte_exn emitter 0x9a;
    Stream.write_???(32 or 64) stream offset_of_die
  | DW_OP_form_tls_address -> Emitter.emit_byte_exn emitter 0x9b
  | DW_OP_call_frame_cfa -> Emitter.emit_byte_exn emitter 0x9c
  | DW_OP_bit_piece (size, offset) ->
    Emitter.emit_byte_exn emitter 0x9d;
    Emitter.emit_uleb128_from_int64 emitter size;
    Emitter.emit_uleb128_from_int64 emitter offset
  | DW_OP_implicit_value (size, block) ->
    Emitter.emit_byte_exn emitter 0x9e;
    Emitter.emit_uleb128_from_int64 emitter size;
    Block.write block ~stream
  | DW_OP_stack_value -> Emitter.emit_byte_exn emitter 0x9f

let literal_for_int64 =
  let table = [|
    DW_OP_lit0; DW_OP_lit1; DW_OP_lit2; DW_OP_lit3; DW_OP_lit4;
    DW_OP_lit5; DW_OP_lit6; DW_OP_lit7; DW_OP_lit8; DW_OP_lit9;
    DW_OP_lit10; DW_OP_lit11; DW_OP_lit12; DW_OP_lit13; DW_OP_lit14;
    DW_OP_lit15; DW_OP_lit16; DW_OP_lit17; DW_OP_lit18; DW_OP_lit19;
    DW_OP_lit20; DW_OP_lit21; DW_OP_lit22; DW_OP_lit23; DW_OP_lit24;
    DW_OP_lit25; DW_OP_lit26; DW_OP_lit27; DW_OP_lit28; DW_OP_lit29;
    DW_OP_lit30; DW_OP_lit31;
  |] in
  fun c ->
    if Int64.compare c 0L >= 0 && Int64.compare c 31L <= 0 then begin
      Some table.(Int64.to_int c)
    end else begin
      None
    end
