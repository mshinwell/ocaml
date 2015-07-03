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

(** DWARF expressions (DWARF-4 standard section 7.7.1) *)

(** We omit the "DW_OP" prefix. *)
type t =
  | Addr of Target_addr.t
  | Deref
  | Const1u of Int8.t
  | Const1s of Int8.t
  | Const2u of Int16.t
  | Const2s of Int16.t
  | Const4u of Int32.t
  | Const4s of Int32.t
  | Const8u of Int64.t
  | Const8s of Int64.t
  | Constu of Uleb128.t
  | Consts of Sleb128.t
  | Du
  | Drop
  | Over
  | Pick of Int8.t
  | Swap
  | Rot
  | Xderef
  | Abs
  | And
  | Div
  | Minus
  | Mod
  | Mul
  | Neg
  | Not
  | Or
  | Plus
  | Plus_uconst of Uleb128.t
  | Shl
  | Shr
  | Shra
  | Xor
  | Skip of Int16.t
  | Bra of Int16.t
  | Eq
  | Ge
  | Gt
  | Le
  | Lt
  | Ne
  | Lit0
  | Lit1
  | Lit2
  | Lit3
  | Lit4
  | Lit5
  | Lit6
  | Lit7
  | Lit8
  | Lit9
  | Lit10
  | Lit11
  | Lit12
  | Lit13
  | Lit14
  | Lit15
  | Lit16
  | Lit17
  | Lit18
  | Lit19
  | Lit20
  | Lit21
  | Lit22
  | Lit23
  | Lit24
  | Lit25
  | Lit26
  | Lit27
  | Lit28
  | Lit29
  | Lit30
  | Lit31
  | Reg0
  | Reg1
  | Reg2
  | Reg3
  | Reg4
  | Reg5
  | Reg6
  | Reg7
  | Reg8
  | Reg9
  | Reg10
  | Reg11
  | Reg12
  | Reg13
  | Reg14
  | Reg15
  | Reg16
  | Reg17
  | Reg18
  | Reg19
  | Reg20
  | Reg21
  | Reg22
  | Reg23
  | Reg24
  | Reg25
  | Reg26
  | Reg27
  | Reg28
  | Reg29
  | Reg30
  | Reg31
  | Breg0 of Target_addr.t
  | Breg1 of Target_addr.t
  | Breg2 of Target_addr.t
  | Breg3 of Target_addr.t
  | Breg4 of Target_addr.t
  | Breg5 of Target_addr.t
  | Breg6 of Target_addr.t
  | Breg7 of Target_addr.t
  | Breg8 of Target_addr.t
  | Breg9 of Target_addr.t
  | Breg10 of Target_addr.t
  | Breg11 of Target_addr.t
  | Breg12 of Target_addr.t
  | Breg13 of Target_addr.t
  | Breg14 of Target_addr.t
  | Breg15 of Target_addr.t
  | Breg16 of Target_addr.t
  | Breg17 of Target_addr.t
  | Breg18 of Target_addr.t
  | Breg19 of Target_addr.t
  | Breg20 of Target_addr.t
  | Breg21 of Target_addr.t
  | Breg22 of Target_addr.t
  | Breg23 of Target_addr.t
  | Breg24 of Target_addr.t
  | Breg25 of Target_addr.t
  | Breg26 of Target_addr.t
  | Breg27 of Target_addr.t
  | Breg28 of Target_addr.t
  | Breg29 of Target_addr.t
  | Breg30 of Target_addr.t
  | Breg31 of Target_addr.t
  | Regx of Register.t
  | Fbreg of Target_addr.t
  | Bregx of Register.t * Target_addr.t
  | Piece of Uleb128.t
  | Deref_size of Int8.t
  | Xderef_size of Int8.t
  | Nop
  | Push_object_address
  (* Call* take the offset of a DIE within the current compilation
     unit. *)
  | Call2 of Int16.t
  | Call4 of Int32.t
  | Call_ref of Dwarf_format.Int.t
  | Form_tls_address
  | Call_frame_cfa
  | Bit_piece of Uleb128.t * Uleb128.t  (* size, offset *)
  | Implicit_value of Uleb128.t * Block.t
  | Stack_value

include Emittable.S with type t := t
include Parseable.S with type t := t
