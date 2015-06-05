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

(* DWARF expressions (DWARF-4 standard section 7.7.1) *)

type register_number = int

(* Some constructors that accept [Int64.t] arguments are actually
   represented in DWARF sections using variable-length encodings. *)
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

include Emittable.S with type t := t
include Parseable.S with type t := t
