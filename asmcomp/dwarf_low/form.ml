(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2015, Jane Street Holding                          *)
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

(* DWARF attribute forms (DWARF-4 specification section 7.5.4). *)

type 'dwarf_class t =
  | DW_FORM_addr : Class.address t
  | DW_FORM_block : Class.block t
  | DW_FORM_block1 : Class.block t
  | DW_FORM_block2 : Class.block t
  | DW_FORM_block4 : Class.block t
  | DW_FORM_data1 : Class.constant t
  | DW_FORM_data2 : Class.constant t
  | DW_FORM_data4 : Class.constant t
  | DW_FORM_data8 : Class.constant t
  | DW_FORM_string : Class.string t
  | DW_FORM_flag : Class.flag t
  | DW_FORM_sdata : Class.constant t
  | DW_FORM_strp : Class.string t
  | DW_FORM_udata : Class.constant t
  | DW_FORM_ref_addr : Class.reference t
  | DW_FORM_ref1 : Class.reference t
  | DW_FORM_ref2 : Class.reference t
  | DW_FORM_ref4 : Class.reference t
  | DW_FORM_ref8 : Class.reference t
  | DW_FORM_ref_udata : Class.reference t
  | DW_FORM_indirect : 'dwarf_class t -> 'dwarf_class t
  | DW_FORM_sec_offset : Class.lineptr_or_loclistptr_or_macptr_or_rangelistptr
  | DW_FORM_exprloc : Class.exprloc t
  | DW_FORM_flag_present : Class.flag t
  | DW_FORM_ref_sig8 : Class.reference  t

let encode t =
  let code =
    (* DWARF-4 standard page 160 onwards. *)
    match t with
    | DW_FORM_addr -> 0x01
    | DW_FORM_block -> 0x09
    | DW_FORM_block1 -> 0x0a
    | DW_FORM_block2 -> 0x03
    | DW_FORM_block4 -> 0x04
    | DW_FORM_data1 -> 0x0b
    | DW_FORM_data2 -> 0x05
    | DW_FORM_data4 -> 0x06
    | DW_FORM_data8 -> 0x07
    | DW_FORM_string -> 0x08
    | DW_FORM_flag -> 0x0c
    | DW_FORM_sdata -> 0x0d
    | DW_FORM_strp -> 0x0e
    | DW_FORM_udata -> 0x0f
    | DW_FORM_ref_addr -> 0x10
    | DW_FORM_ref1 -> 0x11
    | DW_FORM_ref2 -> 0x12
    | DW_FORM_ref4 -> 0x13
    | DW_FORM_ref8 -> 0x14
    | DW_FORM_ref_udata -> 0x15
    | DW_FORM_indirect _ -> 0x16
    | DW_FORM_sec_offset -> 0x17
    | DW_FORM_exprloc -> 0x18
    | DW_FORM_flag_present -> 0x19
    | DW_FORM_ref_sig8 -> 0x20
  in
  Value.as_uleb128 code

let addr = DW_FORM_addr
let data1 = DW_FORM_data1
let data4 = DW_FORM_data4
let data8 = DW_FORM_data8
let string = DW_FORM_string
let strp = DW_FORM_strp
let flag = DW_FORM_flag
let block = DW_FORM_block
let ref_addr = DW_FORM_ref_addr
let sec_offset = DW_FORM_sec_offset
let exprloc = DW_FORM_exprloc

let size t =
  Value.size (encode t)

let emit t ~emitter =
  Value.emit (encode t) ~emitter
