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

type t = {
  name : Linearize.label;
  entries : Location_list_entry.t list;
}

let create entries =
  { name = Linearize.new_label ();
    entries;
  }

let label t = t.name

let size t =
  let body_size =
    List.fold t.entries
      ~init:0
      ~f:(fun size entry -> size + Location_list_entry.size entry)
  in
  body_size + 8 + 8

let emit t ~emitter =
  Emitter.emit_label_declaration emitter ~label_name:t.name;
  List.iter t.entries ~f:(Location_list_entry.emit ~emitter);
  Value.emit (Value.as_code_address Int64.zero) ~emitter;
  Value.emit (Value.as_code_address Int64.zero) ~emitter
