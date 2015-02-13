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
  | DW_OP_breg0
  | DW_OP_breg1
  | DW_OP_breg2
  | DW_OP_breg3
  | DW_OP_breg4
  | DW_OP_breg5
  | DW_OP_breg6
  | DW_OP_breg7
  | DW_OP_breg8
  | DW_OP_breg9
  | DW_OP_breg10
  | DW_OP_breg11
  | DW_OP_breg12
  | DW_OP_breg13
  | DW_OP_breg14
  | DW_OP_breg15
  | DW_OP_breg16
  | DW_OP_breg17
  | DW_OP_breg18
  | DW_OP_breg19
  | DW_OP_breg20
  | DW_OP_breg21
  | DW_OP_breg22
  | DW_OP_breg23
  | DW_OP_breg24
  | DW_OP_breg25
  | DW_OP_breg26
  | DW_OP_breg27
  | DW_OP_breg28
  | DW_OP_breg29
  | DW_OP_breg30
  | DW_OP_breg31
  | DW_OP_regx of reg
  | DW_OP_fbreg of offset
  | DW_OP_bregx of reg * offset
  | DW_OP_piece of Uleb128.t
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
  | DW_OP_bit_piece of Uleb128.t * Uleb128.t  (* size, offset *)
  | DW_OP_implicit_value of Uleb128.t * Block.t
  | DW_OP_stack_value

let dw_op_lo_user = 0xe0
let dw_op_hi_user = 0xff

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
    Stream.write_byte stream 0x11;
    Stream.write_sleb128 stream c
  | DW_OP_dup -> Stream.write_byte stream 0x12
  | DW_OP_drop -> Stream.write_byte stream 0x13
  | DW_OP_over -> Stream.write_byte stream 0x14
  | DW_OP_pick -> Steram.write_byte stream 0x15 (* check 0x15 *)
  | DW_OP_swap -> Stream.write_byte stream 0x16
  | DW_OP_rot -> Stream.write_byte stream 0x17
  | DW_OP_xderef -> Stream.write_byte stream 0x18
  | DW_OP_abs -> Stream.write_byte stream 0x19
  | DW_OP_and -> Stream.write_byte stream 0x1a
  | DW_OP_div -> Stream.write_byte stream 0x1b
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
  | DW_OP_reg0 -> Stream.write_byte stream 0x50
  | DW_OP_reg1 -> Stream.write_byte stream 0x51
  | DW_OP_reg2 -> Stream.write_byte stream 0x52
  | DW_OP_reg3 -> Stream.write_byte stream 0x53
  | DW_OP_reg4 -> Stream.write_byte stream 0x54
  | DW_OP_reg5 -> Stream.write_byte stream 0x55
  | DW_OP_reg6 -> Stream.write_byte stream 0x56
  | DW_OP_reg7 -> Stream.write_byte stream 0x57
  | DW_OP_reg8 -> Stream.write_byte stream 0x58
  | DW_OP_reg9 -> Stream.write_byte stream 0x59
  | DW_OP_reg10 -> Stream.write_byte stream 0x5a
  | DW_OP_reg11 -> Stream.write_byte stream 0x5b
  | DW_OP_reg12 -> Stream.write_byte stream 0x5c
  | DW_OP_reg13 -> Stream.write_byte stream 0x5d
  | DW_OP_reg14 -> Stream.write_byte stream 0x5e
  | DW_OP_reg15 -> Stream.write_byte stream 0x5f
  | DW_OP_reg16 -> Stream.write_byte stream 0x60
  | DW_OP_reg17 -> Stream.write_byte stream 0x61
  | DW_OP_reg18 -> Stream.write_byte stream 0x62
  | DW_OP_reg19 -> Stream.write_byte stream 0x63
  | DW_OP_reg20 -> Stream.write_byte stream 0x64
  | DW_OP_reg21 -> Stream.write_byte stream 0x65
  | DW_OP_reg22 -> Stream.write_byte stream 0x66
  | DW_OP_reg23 -> Stream.write_byte stream 0x67
  | DW_OP_reg24 -> Stream.write_byte stream 0x68
  | DW_OP_reg25 -> Stream.write_byte stream 0x69
  | DW_OP_reg26 -> Stream.write_byte stream 0x6a
  | DW_OP_reg27 -> Stream.write_byte stream 0x6b
  | DW_OP_reg28 -> Stream.write_byte stream 0x6c
  | DW_OP_reg29 -> Stream.write_byte stream 0x6d
  | DW_OP_reg30 -> Stream.write_byte stream 0x6e
  | DW_OP_reg31 -> Stream.write_byte stream 0x6f
  | DW_OP_breg0 offset ->
    Stream.write_byte stream 0x70;
    Stream.write_sleb128 stream offset
  | DW_OP_breg1 offset ->
    Stream.write_byte stream 0x71;
    Stream.write_sleb128 stream offset
  | DW_OP_breg2 offset ->
    Stream.write_byte stream 0x72;
    Stream.write_sleb128 stream offset
  | DW_OP_breg3 offset ->
    Stream.write_byte stream 0x73;
    Stream.write_sleb128 stream offset
  | DW_OP_breg4 offset ->
    Stream.write_byte stream 0x74;
    Stream.write_sleb128 stream offset
  | DW_OP_breg5 offset ->
    Stream.write_byte stream 0x75;
    Stream.write_sleb128 stream offset
  | DW_OP_breg6 offset ->
    Stream.write_byte stream 0x76;
    Stream.write_sleb128 stream offset
  | DW_OP_breg7 offset ->
    Stream.write_byte stream 0x77;
    Stream.write_sleb128 stream offset
  | DW_OP_breg8 offset ->
    Stream.write_byte stream 0x78;
    Stream.write_sleb128 stream offset
  | DW_OP_breg9 offset ->
    Stream.write_byte stream 0x79;
    Stream.write_sleb128 stream offset
  | DW_OP_breg10 offset ->
    Stream.write_byte stream 0x7a;
    Stream.write_sleb128 stream offset
  | DW_OP_breg11 offset ->
    Stream.write_byte stream 0x7b;
    Stream.write_sleb128 stream offset
  | DW_OP_breg12 offset ->
    Stream.write_byte stream 0x7c;
    Stream.write_sleb128 stream offset
  | DW_OP_breg13 offset ->
    Stream.write_byte stream 0x7d;
    Stream.write_sleb128 stream offset
  | DW_OP_breg14 offset ->
    Stream.write_byte stream 0x7e;
    Stream.write_sleb128 stream offset
  | DW_OP_breg15 offset ->
    Stream.write_byte stream 0x7f;
    Stream.write_sleb128 stream offset
  | DW_OP_breg16 offset ->
    Stream.write_byte stream 0x80;
    Stream.write_sleb128 stream offset
  | DW_OP_breg17 offset ->
    Stream.write_byte stream 0x81;
    Stream.write_sleb128 stream offset
  | DW_OP_breg18 offset ->
    Stream.write_byte stream 0x82;
    Stream.write_sleb128 stream offset
  | DW_OP_breg19 offset ->
    Stream.write_byte stream 0x83;
    Stream.write_sleb128 stream offset
  | DW_OP_breg20 offset ->
    Stream.write_byte stream 0x84;
    Stream.write_sleb128 stream offset
  | DW_OP_breg21 offset ->
    Stream.write_byte stream 0x85;
    Stream.write_sleb128 stream offset
  | DW_OP_breg22 offset ->
    Stream.write_byte stream 0x86;
    Stream.write_sleb128 stream offset
  | DW_OP_breg23 offset ->
    Stream.write_byte stream 0x87;
    Stream.write_sleb128 stream offset
  | DW_OP_breg24 offset ->
    Stream.write_byte stream 0x88;
    Stream.write_sleb128 stream offset
  | DW_OP_breg25 offset ->
    Stream.write_byte stream 0x89;
    Stream.write_sleb128 stream offset
  | DW_OP_breg26 offset ->
    Stream.write_byte stream 0x8a;
    Stream.write_sleb128 stream offset
  | DW_OP_breg27 offset ->
    Stream.write_byte stream 0x8b;
    Stream.write_sleb128 stream offset
  | DW_OP_breg28 offset ->
    Stream.write_byte stream 0x8c;
    Stream.write_sleb128 stream offset
  | DW_OP_breg29 offset ->
    Stream.write_byte stream 0x8d;
    Stream.write_sleb128 stream offset
  | DW_OP_breg30 offset ->
    Stream.write_byte stream 0x8e;
    Stream.write_sleb128 stream offset
  | DW_OP_breg31 offset ->
    Stream.write_byte stream 0x8f;
    Stream.write_sleb128 stream offset
  | DW_OP_regx reg ->
    Stream.write_byte stream 0x90;
    Stream.write_uleb128 stream reg
  | DW_OP_fbreg offset ->
    Stream.write_byte stream 0x91;
    Stream.write_sleb128 stream offset;
  | DW_OP_bregx (reg, offset) ->
    Stream.write_byte stream 0x92;
    Stream.write_uleb128 stream reg;
    Stream.write_sleb128 stream offset
  | DW_OP_piece size ->
    Stream.write_byte stream 0x93;
    Stream.write_uleb128 stream size
  | DW_OP_deref_size size ->
    Stream.write_byte stream 0x94;
    Stream.write_byte stream size
  | DW_OP_xderef_size ->
    Stream.write_byte stream 0x95;
    Stream.write_byte stream size
  | DW_OP_nop -> Stream.write_byte stream 0x96
  | DW_OP_push_object_address -> Stream.write_byte stream 0x97
  | DW_OP_call2 offset_of_die ->
    Stream.write_byte stream 0x98;
    Stream.write_int16 stream offset_of_die
  | DW_OP_call4 offset_of_die ->
    Stream.write_byte stream 0x99;
    Stream.write_int32 stream offset_of_die
  | DW_OP_call_ref offset_of_die ->
    Stream.write_byte stream 0x9a;
    Stream.write_???(32 or 64) stream offset_of_die
  | DW_OP_form_tls_address -> Stream.write_byte stream 0x9b
  | DW_OP_call_frame_cfa -> Stream.write_byte stream 0x9c
  | DW_OP_bit_piece (size, offset) ->
    Stream.write_byte stream 0x9d;
    Stream.write_uleb128 stream size;
    Stream.write_uleb128 stream offset
  | DW_OP_implicit_value (size, block) ->
    Stream.write_byte stream 0x9e;
    Stream.write_uleb128 stream size;
    Block.write block ~stream
  | DW_OP_stack_value -> Stream.write_byte stream 0x9f

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
  fun c ~emit ->
    if Int64.compare c 0L >= 0 && Int64.compare c 31L <= 0 then begin
      Some table.(Int64.to_int c)
    end else begin
      None
    end
