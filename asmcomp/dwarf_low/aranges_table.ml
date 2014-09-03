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

open Std_internal

(* DWARF-4 standard section 6.1.2. *)

type t = {
  size : Int64.t;
  values : Value.t list;
}

let create ~start_of_code_symbol ~end_of_code_symbol =
  let values = [
    Value.as_two_byte_int 2;  (* section version number *)
    Value.as_absolute_offset Int64.zero;  (* offset into .debug_info *)
    Value.as_byte Arch.size_addr;
    Value.as_byte 0;
    Value.as_two_byte_int 0;
    Value.as_two_byte_int 0;
    Value.as_code_address_from_symbol start_of_code_symbol;
    Value.as_code_address_from_label_diff
      (`Symbol end_of_code_symbol) (`Symbol start_of_code_symbol);
    Value.as_code_address Nativeint.zero;
    Value.as_code_address Nativeint.zero;
  ]
  in
  let size =
    List.fold_left values
      ~init:Int64.zero
      ~f:(fun size value -> Int64.add size (Value.size value))
  in
  { size; values; }

let size t = t.size

let emit t ~emitter =
  Initial_length.emit (Initial_length.create t.size) ~emitter;
  List.iter t.values ~f:(Value.emit ~emitter)
