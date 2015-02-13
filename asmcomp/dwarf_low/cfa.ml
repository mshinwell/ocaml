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

(* DWARF call frame information (DWARF-4 standard section 7.23). *)

type t =
  | DW_CFA_advance_loc of <delta>
  | DW_CFA_offset of <register>
  | DW_CFA_restore of <register> * <offset>
  | DW_CFA_nop
  | DW_CFA_set_loc of <address>
  | DW_CFA_advance_loc1 of <1-byte delta>
  | DW_CFA_advance_loc2 of <2-byte delta>
  | DW_CFA_advance_loc4 of <4-byte delta>
  | DW_CFA_offset_extended of reg * offset
  | DW_CFA_restore_extended of reg
  | DW_CFA_undefined of reg
  | DW_CFA_same_value of reg
  | DW_CFA_register of reg & reg
  | DW_CFA_remember_state
  | DW_CFA_restore_state
  | DW_CFA_def_cfa of reg * offset
  | DW_CFA_def_cfa_register of reg
  | DW_CFA_def_cfa_offset of offset
  | DW_CFA_def_cfa_expression of block
  | DW_CFA_expression of reg * block
  | DW_CFA_offset_extended_sf of reg * offset
  | DW_CFA_def_cfa_sf of reg * offset
  | DW_CFA_def_cfa_offset_sf of offset
  | DW_CFA_val_offset of Uleb128.t * Uleb128.t
  | DW_CFA_val_offset_sf of Uleb128.t * Sleb128.t
  | DW_CFA_val_expression of Uleb128.t * block

let dw_cfa_lo_user_high_2_bits = 0
let dw_cfa_lo_user_low_6_bits = 0x1c
let dw_cfa_hi_user_high_2_bits = 0
let dw_cfa_hi_user_low_6_bits = 0x3f

(* DWARF-4 specification figure 40, pages 181--182. *)
let high_2_bits = function
  | DW_CFA_advance_loc -> 0x1
  | DW_CFA_offset ->  0x2
  | DW_CFA_restore -> 0x3
  | DW_CFA_nop -> 0
  | DW_CFA_set_loc -> 0
  | DW_CFA_advance_loc1 -> 0
  | DW_CFA_advance_loc2 -> 0
  | DW_CFA_advance_loc4 -> 0
  | DW_CFA_offset_extended -> 0
  | DW_CFA_restore_extended -> 0
  | DW_CFA_undefined -> 0
  | DW_CFA_same_value -> 0
  | DW_CFA_register -> 0
  | DW_CFA_remember_state -> 0
  | DW_CFA_restore_state -> 0
  | DW_CFA_def_cfa -> 0
  | DW_CFA_def_cfa_register -> 0
  | DW_CFA_def_cfa_offset -> 0
  | DW_CFA_def_cfa_expression -> 0
  | DW_CFA_expression -> 0
  | DW_CFA_offset_extended_sf -> 0
  | DW_CFA_def_cfa_sf -> 0
  | DW_CFA_def_cfa_offset_sf -> 0
  | DW_CFA_val_offset -> 0
  | DW_CFA_val_offset_sf -> 0
  | DW_CFA_val_expression -> 0

(* DWARF-4 specification figure 40, pages 181--182. *)
let low_6_bits = function
  | DW_CFA_advance_loc delta -> delta
  | DW_CFA_offset reg ->  reg
  | DW_CFA_restore reg -> reg
  | DW_CFA_nop -> 0
  | DW_CFA_set_loc -> 0x01
  | DW_CFA_advance_loc1 -> 0x02
  | DW_CFA_advance_loc2 -> 0x03
  | DW_CFA_advance_loc4 -> 0x04
  | DW_CFA_offset_extended -> 0x05
  | DW_CFA_restore_extended -> 0x06
  | DW_CFA_undefined -> 0x07
  | DW_CFA_same_value -> 0x08
  | DW_CFA_register -> 0x09
  | DW_CFA_remember_state -> 0x0a
  | DW_CFA_restore_state -> 0x0b
  | DW_CFA_def_cfa -> 0x0c
  | DW_CFA_def_cfa_register -> 0x0d
  | DW_CFA_def_cfa_offset -> 0x0e
  | DW_CFA_def_cfa_expression -> 0x0f
  | DW_CFA_expression -> 0x10
  | DW_CFA_offset_extended_sf -> 0x11
  | DW_CFA_def_cfa_sf -> 0x12
  | DW_CFA_def_cfa_offset_sf -> 0x13
  | DW_CFA_val_offset -> 0x14
  | DW_CFA_val_offset_sf -> 0x15
  | DW_CFA_val_expression -> 0x16


