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
  | DW_AT_low_pc
  | DW_AT_high_pc
  | DW_AT_name
  | DW_AT_comp_dir
  | DW_AT_producer
  | DW_AT_stmt_list
  | DW_AT_external
  | DW_AT_location
  | DW_AT_type
  | DW_AT_encoding
  | DW_AT_byte_size
  | DW_AT_linkage_name  (* DWARF-4 *)
  | DW_AT_sibling

let encode = function
  | DW_AT_low_pc -> 0x11
  | DW_AT_high_pc -> 0x12
  | DW_AT_name -> 0x03
  | DW_AT_comp_dir -> 0x1b
  | DW_AT_producer -> 0x25
  | DW_AT_stmt_list -> 0x10
  | DW_AT_external -> 0x3f
  | DW_AT_location -> 0x02
  | DW_AT_type -> 0x49
  | DW_AT_encoding -> 0x3e
  | DW_AT_byte_size -> 0x0b
  | DW_AT_linkage_name -> 0x6e
  | DW_AT_sibling -> 0x01

let compare t1 t2 =
  compare (encode t1) (encode t2)

let form = function
  | DW_AT_low_pc -> Form.addr
  | DW_AT_high_pc -> Form.addr
  | DW_AT_name -> Form.strp
  | DW_AT_comp_dir -> Form.strp
  | DW_AT_producer -> Form.strp
  | DW_AT_stmt_list -> Form.data4
  | DW_AT_external -> Form.flag
  | DW_AT_location -> Form.data8
  | DW_AT_type -> Form.ref_addr
  | DW_AT_encoding -> Form.data1
  | DW_AT_byte_size -> Form.data1
  | DW_AT_linkage_name -> Form.strp
  | DW_AT_sibling -> Form.ref_addr

let low_pc = DW_AT_low_pc
let high_pc = DW_AT_high_pc
let producer = DW_AT_producer
let name = DW_AT_name
let comp_dir = DW_AT_comp_dir
let stmt_list = DW_AT_stmt_list
let extern'l = DW_AT_external
let location = DW_AT_location
let typ' = DW_AT_type
let encoding = DW_AT_encoding
let byte_size = DW_AT_byte_size
let linkage_name = DW_AT_linkage_name
let sibling = DW_AT_sibling

let size t =
  Int64.add (Value.size (Value.as_uleb128 (encode t)))
    (Form.size (form t))

let emit t ~emitter =
  Value.emit (Value.as_uleb128 (encode t)) ~emitter;
  Form.emit (form t) ~emitter
