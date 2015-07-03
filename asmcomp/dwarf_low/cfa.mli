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

(** DWARF call frame information (DWARF-4 standard section 7.23). *)

(** We emit the "DW_CFA_" prefix. *)
type t =
  | Advance_loc of Int6.t
  | Offset of Register.Six_bit.t * Uleb128.t
  | Restore of Register.Six_bit.t
  | Nop
  | Set_loc of Target_addr.t
  | Advance_loc1 of Int8.t
  | Advance_loc2 of Int16.t
  | Advance_loc4 of Int32.t
  | Offset_extended of Register.t * Uleb128.t
  | Restore_extended of Register.t
  | Undefined of Register.t
  | Same_value of Register.t
  | Register of Register.t * Register.t
  | Remember_state
  | Restore_state
  | Def_cfa of Register.t * Uleb128.t
  | Def_cfa_register of Register.t
  | Def_cfa_offset of Uleb128.t
  | Def_cfa_expression of Block.t
  | Expression of Register.t * Block.t
  | Offset_extended_sf of Register.t * Sleb128.t
  | Def_cfa_sf of Register.t * Sleb128.t
  | Def_cfa_offset_sf of Sleb128.t
  | Val_offset of Uleb128.t * Uleb128.t
  | Val_offset_sf of Uleb128.t * Sleb128.t
  | Val_expression of Uleb128.t * Block.t

include Emittable with type t := t
include Parseable with type t := t
