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
  | Dwarf_2
  | Dwarf_3
  | Dwarf_4

let two = Dwarf_2
let three = Dwarf_3
let four = Dwarf_4

let encode t =
  let code =
    match t with
    | Dwarf_2 -> 2
    | Dwarf_3 -> 3
    | Dwarf_4 -> 4
  in
  Value.as_two_byte_int code

let size t =
  Value.size (encode t)

let emit t =
  Value.emit (encode t)
