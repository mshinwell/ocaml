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

open Std_internal

type t = {
  size : int;
  values : Value.t list;
}

let create ~start_of_code_label ~end_of_code_label =
  let address_width_in_bytes_on_target = Value.as_byte 8 in
  let values = [
    Value.as_two_byte_int 2;  (* section version number *)
    Value.as_four_byte_int 0;
    address_width_in_bytes_on_target;
    Value.as_byte 0;
    Value.as_two_byte_int 0;
    Value.as_two_byte_int 0;
    Value.as_code_address_from_label start_of_code_label;
    Value.as_code_address_from_label_diff
      end_of_code_label start_of_code_label;
    Value.as_code_address Int64.zero;
    Value.as_code_address Int64.zero;
  ]
  in
  let size =
    List.fold_left values
      ~init:0
      ~f:(fun size value -> size + Value.size value)
  in
  { size; values; }

let size t = t.size

let emit t ~emitter =
  Value.emit (Value.as_four_byte_int t.size) ~emitter;
  List.iter t.values ~f:(Value.emit ~emitter)
