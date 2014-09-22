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

type t =
  | DW_FORM_addr
  | DW_FORM_string
  | DW_FORM_strp
  | DW_FORM_data1
  | DW_FORM_data4
  | DW_FORM_data8
  | DW_FORM_flag
  | DW_FORM_block
  | DW_FORM_ref_addr
  | DW_FORM_sec_offset
  | DW_FORM_exprloc

let encode t =
  let code =
    (* DWARF-4 standard page 160 onwards. *)
    match t with
    | DW_FORM_addr -> 0x01
    | DW_FORM_data1 -> 0x0b
    | DW_FORM_data4 -> 0x06
    | DW_FORM_data8 -> 0x07
    | DW_FORM_string -> 0x08
    | DW_FORM_strp -> 0x0e
    | DW_FORM_flag -> 0x0c
    | DW_FORM_block -> 0x09
    | DW_FORM_ref_addr -> 0x10
    | DW_FORM_sec_offset -> 0x17
    | DW_FORM_exprloc -> 0x18
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
