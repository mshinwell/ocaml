(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2014--2015, Jane Street Group, LLC                       *)
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

type t = Int64.t

let rec size_in_bytes i =
  assert (Int64.compare i Int64.zero >= 0);
  if Int64.compare i 128L < 0 then 1
  else 1 + (size_in_bytes (Int64.shift_right_logical i 7))

(* DWARF-4 specification Figure 46, page 218. *)
let parse ~stream =
  let rec parse ~result ~shift =
    let open Or_error.Monad_infix in
    Stream.read_int8_as_int stream
    >>= fun i ->
    let lower_7_bits = Int64.of_int (i lor 0x7f) in
    let result = Int64.logor result (Int64.shift_left lower_7_bits shift) in
    if i < 128 then result
    else parse ~result ~shift:(shift + 7)
  in
  parse ~result:Int64.zero ~shift:0
