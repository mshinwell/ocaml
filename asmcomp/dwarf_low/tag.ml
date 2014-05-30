(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
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
  | DW_TAG_compile_unit
  | DW_TAG_compile_unit__no_children
  | DW_TAG_subprogram
  | DW_TAG_subprogram__no_children
  | DW_TAG_formal_parameter
  | DW_TAG_variable
  | DW_TAG_base_type
  | DW_TAG_lexical_block

let encode t =
  let code =
    match t with
    | DW_TAG_compile_unit -> 0x11
    | DW_TAG_compile_unit__no_children -> 0x11
    | DW_TAG_subprogram -> 0x2e
    | DW_TAG_subprogram__no_children -> 0x2e
    | DW_TAG_formal_parameter -> 0x05
    | DW_TAG_variable -> 0x34
    | DW_TAG_base_type -> 0x24
    | DW_TAG_lexical_block -> 0x0b
  in
  Value.as_uleb128 code

(* CR mshinwell: "__no_children" is a hack *)

let child_determination = function
  | DW_TAG_compile_unit -> Child_determination.yes
  | DW_TAG_compile_unit__no_children -> Child_determination.no
  | DW_TAG_subprogram -> Child_determination.yes
  | DW_TAG_subprogram__no_children -> Child_determination.no
  | DW_TAG_formal_parameter -> Child_determination.no
  | DW_TAG_variable -> Child_determination.no
  | DW_TAG_base_type -> Child_determination.no
  | DW_TAG_lexical_block -> Child_determination.yes

let compile_unit = DW_TAG_compile_unit
let compile_unit_with_no_children = DW_TAG_compile_unit__no_children
let subprogram = DW_TAG_subprogram
let subprogram_with_no_children = DW_TAG_subprogram__no_children
let formal_parameter = DW_TAG_formal_parameter
let variable = DW_TAG_variable
let base_type = DW_TAG_base_type
let lexical_block = DW_TAG_lexical_block

let size t =
  Value.size (encode t)

let emit t ~emitter =
  Value.emit (encode t) ~emitter
