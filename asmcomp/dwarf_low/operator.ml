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
  | DW_op_regx of Value.t
  | DW_op_bregx of [ `Register of Value.t ] * [ `Offset of Value.t ]

let register ~reg_number =
  let reg_number = Value.as_uleb128 reg_number in
  DW_op_regx reg_number

let register_based_addressing ~reg_number ~offset_in_bytes =
  let reg_number = Value.as_uleb128 reg_number in
  (* CR mshinwell: strictly speaking the offset should be signed leb128 *)
  let offset_in_bytes = Value.as_uleb128 offset_in_bytes in
  DW_op_bregx (`Register reg_number, `Offset offset_in_bytes)

let opcode = function
  | DW_op_regx _ -> 0x90
  | DW_op_bregx _ -> 0x92

let size t =
  let opcode_size = 1 in
  let args_size =
    match t with
    | DW_op_regx reg_number -> Value.size reg_number
    | DW_op_bregx (`Register reg_number, `Offset offset) ->
      Value.size reg_number + Value.size offset
  in
  opcode_size + args_size

let emit t ~emitter =
  Value.emit (Value.as_byte (opcode t)) ~emitter;
  match t with
  | DW_op_regx reg_number -> Value.emit reg_number ~emitter
  | DW_op_bregx (`Register reg_number, `Offset offset) ->
    Value.emit reg_number ~emitter;
    Value.emit offset ~emitter
