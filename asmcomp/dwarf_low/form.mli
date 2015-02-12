(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2014, Jane Street Holding                          *)
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

(* DWARF attribute forms (DWARF-4 specification section 7.5.4, page 146).
   Each form belongs to one or more "classes" (see class.mli).
*)

type t

include Emittable.S with type t := t

val addr : t
val data1 : t
val data4 : t
val data8 : t
val string : t
val strp : t
val flag : t
val block : t
val ref_addr : t
val sec_offset : t
val exprloc : t
 

DW_FORM_addr 0x01 address
DW_FORM_block2 0x03 block
DW_FORM_block4 0x04 block
DW_FORM_data2 0x05 constant
DW_FORM_data4 0x06 constant
DW_FORM_data8 0x07 constant
DW_FORM_string 0x08 string
DW_FORM_block 0x09 block
DW_FORM_block1 0x0a block
DW_FORM_data1 0x0b constant
DW_FORM_flag 0x0c flag
DW_FORM_sdata 0x0d constant
DW_FORM_strp 0x0e string
DW_FORM_udata 0x0f constant
DW_FORM_ref_addr 0x10 reference
DW_FORM_ref1 0x11 reference
DW_FORM_ref2 0x12 reference
DW_FORM_ref4 0x13 reference
DW_FORM_ref8 0x14 referenceTION 7-- DATA REPRESENTATION
June 10, 2010 Page 161
Form name Value Class
DW_FORM_ref_udata 0x15 reference
DW_FORM_indirect 0x16 (see Section 7.5.3)
DW_FORM_sec_offset ‡ 0x17 lineptr, loclistptr, macptr, rangelistptr
DW_FORM_exprloc ‡ 0x18 exprloc
DW_FORM_flag_present ‡ 0x19 flag
DW_FORM_ref_sig8 ‡
