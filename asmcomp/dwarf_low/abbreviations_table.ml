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

type t = Abbreviations_table_entry.t list

let create () = []

let add t entry = entry::t

let find t ~tag ~has_children ~attributes =
  try
    Some (Abbreviations_table_entry.abbreviation_code
      (List.find t ~f:(fun entry ->
        tag = Abbreviations_table_entry.tag entry
          && has_children = Abbreviations_table_entry.has_children entry
          && attributes = Abbreviations_table_entry.attributes entry)))
  with Not_found -> None

let size t =
  List.fold t
    ~init:0
    ~f:(fun size entry -> size + Abbreviations_table_entry.size entry)
    (* See below re. the zero word. *)
    + Value.size (Value.as_uleb128 0)

let emit t ~emitter =
  List.iter (List.rev t) ~f:(Abbreviations_table_entry.emit ~emitter);
  (* DWARF-4 spec section 7.5.3: "The abbreviations for a given compilation unit end
     with an entry consisting of a 0 byte for the abbreviation code." *)
  Value.emit (Value.as_uleb128 0) ~emitter
