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

type t = Abbreviations_table_entry.t list

let create abbrev_table_entries =
  abbrev_table_entries

let size t =
  List.fold t
    ~init:0
    ~f:(fun size entry -> size + Abbreviations_table_entry.size entry)
    + Value.size (Value.as_uleb128 0)

let emit t ~emitter =
  List.iter t ~f:(Abbreviations_table_entry.emit ~emitter);
  Value.emit (Value.as_uleb128 0) ~emitter
