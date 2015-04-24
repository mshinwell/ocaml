(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2015, Jane Street Group, LLC                       *)
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

type addr = [ `addr ]
type block = [ `block ]
type block1 = [ `block1 ]
type block2 = [ `block2 ]
type block4 = [ `block4 ]
type data1 = [ `data1 ]
type data2 = [ `data2 ]
type data4 = [ `data4 ]
type data8 = [ `data8 ]
type string = [ `string ]
type flag = [ `flag ]
type sdata = [ `sdata ]
type strp = [ `strp ]
type udata = [ `udata ]
type ref_addr = [ `ref_addr ]
type ref1 = [ `ref1 ]
type ref2 = [ `ref2 ]
type ref4 = [ `ref4 ]
type ref8 = [ `ref8 ]
type ref_udata = [ `ref_udata ]
type indirect = [ `indirect ]
type sec_offset = [ `sec_offset ]
type exprloc = [ `exprloc ]
type flag_present = [ `flag_present ]
type ref_sig8 = [ `ref_sig8 ]

(* We omit the "DW_FORM_" prefix. *)
type ('dwarf_classes, 'form) t =
  | Addr : (Class.address, addr) t
  | Block : (Class.block, block) t
  | Block1 : (Class.block, block1) t
  | Block2 : (Class.block, [ block1 | block2 ]) t
  | Block4 : (Class.block, [ block1 | block2 | block4 ]) t
  | Data1 : (Class.constant, data1) t
  | Data2 : (Class.constant, [ data1 | data2 ]) t
  | Data4 : (Class.constant, [ data1 | data2 | data4 ]) t
  | Data8 : (Class.constant, [ data1 | data2 | data4 | data8 ]) t
  | String : (Class.string, string) t
  | Flag : (Class.flag, data1) t
  | Sdata : (Class.constant, sdata) t
  | Strp : (Class.string, strp) t
  | Udata : (Class.constant, udata) t
  | Ref_addr : (Class.reference, ref_addr) t
  | Ref1 : (Class.reference, ref1) t
  | Ref2 : (Class.reference, [ ref1 | ref2 ]) t
  | Ref4 : (Class.reference, [ ref1 | ref2 | ref4 ]) t
  | Ref8 : (Class.reference, [ ref1 | ref2 | ref4 | ref8 ]) t
  | Ref_udata : (Class.reference, ref_udata) t
  | Indirect : 'dwarf_class t -> 'dwarf_class t (* XXX *)
  | Sec_offset :
    (Class.lineptr_or_loclistptr_or_macptr_or_rangelistptr, sec_offset) t
  | Exprloc : (Class.exprloc, exprloc) t
  | Flag_present : (Class.flag, flag_present) t
  | Ref_sig8 : (Class.reference, ref_sig8) t

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

let size t =
  Value.size (encode t)

let emit t ~emitter =
  Value.emit (encode t) ~emitter

let parse stream =
  Stream.read_uleb128_as_int stream
  >>= function
  | 0x01 -> Ok DW_FORM_addr
  | 0x09 -> Ok DW_FORM_block
  | 0x0a -> Ok DW_FORM_block1
  | 0x03 -> Ok DW_FORM_block2
  | 0x04 -> Ok DW_FORM_block4
  | 0x0b -> Ok DW_FORM_data1
  | 0x05 -> Ok DW_FORM_data2
  | 0x06 -> Ok DW_FORM_data4
  | 0x07 -> Ok DW_FORM_data8
  | 0x08 -> Ok DW_FORM_string
  | 0x0c -> Ok DW_FORM_flag
  | 0x0d -> Ok DW_FORM_sdata
  | 0x0e -> Ok DW_FORM_strp
  | 0x0f -> Ok DW_FORM_udata
  | 0x10 -> Ok DW_FORM_ref_addr
  | 0x11 -> Ok DW_FORM_ref1
  | 0x12 -> Ok DW_FORM_ref2
  | 0x13 -> Ok DW_FORM_ref4
  | 0x14 -> Ok DW_FORM_ref8
  | 0x15 -> Ok DW_FORM_ref_udata
  | 0x16 -> Ok DW_FORM_indirect
  | 0x17 -> Ok DW_FORM_sec_offset
  | 0x18 -> Ok DW_FORM_exprloc
  | 0x19 -> Ok DW_FORM_flag_present
  | 0x20 -> Ok DW_FORM_ref_sig8
  | _ -> Error (Printf.sprintf "unknown DWARF form code 0x%x" code)
