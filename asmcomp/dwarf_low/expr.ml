(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Holding                                *)
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

(* DWARF expressions (DWARF-4 standard section 7.7.1) *)
type t =
  | DW_OP_addr of Target_address.t
  | DW_OP_deref
  | DW_OP_const1u of Int8.t
  | DW_OP_const1s of Int8.t
  | DW_OP_const2u of Int16.t
  | DW_OP_const2s of Int16.t
  | DW_OP_const4u of Int32.t
  | DW_OP_const4s of Int32.t
  | DW_OP_const8u of Int64.t
  | DW_OP_const8s of Int64.t
  | DW_OP_constu of Uleb128.t
  | DW_OP_consts of Sleb128.t
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
  | DW_OP_plus_uconst of Uleb128.t
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
  | DW_OP_breg0 0x70 1
  | DW_OP_breg1 0x71 1
...
  | DW_OP_breg31 0x8f 1
SLEB128 offset
base register 0..31 =
 (  | DW_OP_breg0 + regnum)
  | DW_OP_regx 0x90 1 ULEB128 register
  | DW_OP_fbreg 0x91 1 SLEB128 offset
  | DW_OP_bregx 0x92 2 ULEB128 register followed by
SLEB128 offset
  | DW_OP_piece 0x93 1 ULEB128 size of piece addressed
  | DW_OP_deref_size 0x94 1 1-byte size of data retrieved
  | DW_OP_xderef_size 0x95 1 1-byte size of data retrieved
  | DW_OP_nop
  | DW_OP_push_object_address
  | DW_OP_call2 0x98 1 2-byte offset of DIE
  | DW_OP_call4 0x99 1 4-byte offset of DIE
  | DW_OP_call_ref 0x9a 1 4- or 8-byte offset of DIE
  | DW_OP_form_tls_address
  | DW_OP_call_frame_cfa
  | DW_OP_bit_piece 0x9d 2 ULEB128 size followed by
ULEB128 offset
  | DW_OP_implicit_value ‡ 0x9e 2 ULEB128 size followed by block
of that size
  | DW_OP_stack_value


  | DW_OP_lo_user 0xe0
  | DW_OP_hi_user 0xff 

let write t ~stream =
  match t with
  | DW_OP_addr addr ->
    Stream.write_byte stream 0x03;
    Stream.write_target_address stream addr
  | DW_OP_deref -> Stream.write_byte stream 0x06
  | DW_OP_const1u c ->
    Stream.write_byte stream 0x08;
    Stream.write_byte stream c
  | DW_OP_const1s c ->
    Stream.write_byte stream 0x09;
    Stream.write_byte stream c
  | DW_OP_const2u c ->
    Stream.write_byte stream 0x0a;
    Stream.write_int16 stream c
  | DW_OP_const2s c ->
    Stream.write_byte stream 0x0b;
    Stream.write_int16 stream c
  | DW_OP_const4u c ->
    Stream.write_byte stream 0x0c;
    Stream.write_int32 stream c
  | DW_OP_const4s c ->
    Stream.write_byte stream 0x0d;
    Stream.write_int32 stream c
  | DW_OP_const8u c ->
    Stream.write_byte stream 0x0e;
    Stream.write_int64 stream c
  | DW_OP_const8s c ->
    Stream.write_byte stream 0x0f;
    Stream.write_int64 stream c
  | DW_OP_constu c ->
    Stream.write_byte stream 0x10;
    Stream.write_uleb128 stream c
  | DW_OP_consts c ->
    Stream.write_byte stream 0x10;
    Stream.write_sleb128 stream c
  | DW_OP_dup -> Stream.write_byte stream 0x12
  | DW_OP_drop -> Stream.write_byte stream 0x13
  | DW_OP_over -> Stream.write_byte stream 0x14
  | DW_OP_pick 0x15 1 1-byte stack index
  | DW_OP_swap -> Stream.write_byte stream 0x16
  | DW_OP_rot -> Stream.write_byte stream 0x17
  | DW_OP_xderef -> Stream.write_byte stream 0x18
  | DW_OP_abs -> Stream.write_byte stream 0x19
  | DW_OP_and -> Stream.write_byte stream 0x1a
  | DW_OP_div -> Stream.write_byte stream 0x1b 0 
  | DW_OP_minus -> Stream.write_byte stream 0x1c
  | DW_OP_mod -> Stream.write_byte stream 0x1d
  | DW_OP_mul -> Stream.write_byte stream 0x1e
  | DW_OP_neg -> Stream.write_byte stream 0x1f
  | DW_OP_not -> Stream.write_byte stream 0x20
  | DW_OP_or -> Stream.write_byte stream 0x21
  | DW_OP_plus -> Stream.write_byte stream 0x22
  | DW_OP_plus_uconst addend ->
    Stream.write_byte stream 0x23;
    Stream.write_uleb128 stream addend
  | DW_OP_shl -> Stream.write_byte stream 0x24
  | DW_OP_shr -> Stream.write_byte stream 0x25
  | DW_OP_shra -> Stream.write_byte stream 0x26
  | DW_OP_xor -> Stream.write_byte stream 0x27
  | DW_OP_skip c ->
    Stream.write_byte stream 0x2f;
    Stream.write_int16 stream c
  | DW_OP_bra ->
    Stream.write_byte stream 0x28;
    Stream.write_int16 stream c
  | DW_OP_eq -> Stream.write_byte stream 0x29
  | DW_OP_ge -> Stream.write_byte stream 0x2a
  | DW_OP_gt -> Stream.write_byte stream 0x2b
  | DW_OP_le -> Stream.write_byte stream 0x2c
  | DW_OP_lt -> Stream.write_byte stream 0x2d
  | DW_OP_ne -> Stream.write_byte stream 0x2e
  | DW_OP_lit0 -> Stream.write_byte stream 0x30
  | DW_OP_lit1 -> Stream.write_byte stream 0x31
  | DW_OP_lit2 -> Stream.write_byte stream 0x32
  | DW_OP_lit3 -> Stream.write_byte stream 0x33
  | DW_OP_lit4 -> Stream.write_byte stream 0x34
  | DW_OP_lit5 -> Stream.write_byte stream 0x35
  | DW_OP_lit6 -> Stream.write_byte stream 0x36
  | DW_OP_lit7 -> Stream.write_byte stream 0x37
  | DW_OP_lit8 -> Stream.write_byte stream 0x38
  | DW_OP_lit9 -> Stream.write_byte stream 0x39
  | DW_OP_lit10 -> Stream.write_byte stream 0x3a
  | DW_OP_lit11 -> Stream.write_byte stream 0x3b
  | DW_OP_lit12 -> Stream.write_byte stream 0x3c
  | DW_OP_lit13 -> Stream.write_byte stream 0x3d
  | DW_OP_lit14 -> Stream.write_byte stream 0x3e
  | DW_OP_lit15 -> Stream.write_byte stream 0x3f
  | DW_OP_lit16 -> Stream.write_byte stream 0x40
  | DW_OP_lit17 -> Stream.write_byte stream 0x41
  | DW_OP_lit18 -> Stream.write_byte stream 0x42
  | DW_OP_lit19 -> Stream.write_byte stream 0x43
  | DW_OP_lit20 -> Stream.write_byte stream 0x44
  | DW_OP_lit21 -> Stream.write_byte stream 0x45
  | DW_OP_lit22 -> Stream.write_byte stream 0x46
  | DW_OP_lit23 -> Stream.write_byte stream 0x47
  | DW_OP_lit24 -> Stream.write_byte stream 0x48
  | DW_OP_lit25 -> Stream.write_byte stream 0x49
  | DW_OP_lit26 -> Stream.write_byte stream 0x4a
  | DW_OP_lit27 -> Stream.write_byte stream 0x4b
  | DW_OP_lit28 -> Stream.write_byte stream 0x4c
  | DW_OP_lit29 -> Stream.write_byte stream 0x4d
  | DW_OP_lit30 -> Stream.write_byte stream 0x4e
  | DW_OP_lit31 -> Stream.write_byte stream 0x4f
  | DW_OP_reg0 0x50 0
  | DW_OP_reg1 0x51 0
...
  | DW_OP_reg31 0x6f 0
reg 0..31 =
 (  | DW_OP_reg0 + regnum)
  | DW_OP_breg0 0x70 1
  | DW_OP_breg1 0x71 1
...
  | DW_OP_breg31 0x8f 1
SLEB128 offset
base register 0..31 =
 (  | DW_OP_breg0 + regnum)
  | DW_OP_regx 0x90 1 ULEB128 register
  | DW_OP_fbreg 0x91 1 SLEB128 offset
  | DW_OP_bregx 0x92 2 ULEB128 register followed by SLEB128 offset
  | DW_OP_piece 0x93 1 ULEB128 size of piece addressed
  | DW_OP_deref_size 0x94 1 1-byte size of data retrieved
  | DW_OP_xderef_size 0x95 1 1-byte size of data retrieved
  | DW_OP_nop -> Stream.write_byte stream 0x96
  | DW_OP_push_object_address -> Stream.write_byte stream 0x97
  | DW_OP_call2 0x98 1 2-byte offset of DIE
  | DW_OP_call4 0x99 1 4-byte offset of DIE
  | DW_OP_call_ref 0x9a 1 4- or 8-byte offset of DIE
  | DW_OP_form_tls_address -> Stream.write_byte stream 0x9b
  | DW_OP_call_frame_cfa -> Stream.write_byte stream 0x9c
  | DW_OP_bit_piece 0x9d 2 ULEB128 size followed by ULEB128 offset
  | DW_OP_implicit_value 0x9e 2 ULEB128 size followed by block of that size
  | DW_OP_stack_value -> Stream.write_byte stream 0x9f
  | DW_OP_lo_user 0xe0
  | DW_OP_hi_user 0xff 
