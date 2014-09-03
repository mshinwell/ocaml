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

(* It isn't exactly clear what the sorting requirement is, but we sort
   within a location list by increasing virtual memory address on the
   start addresses of the entries. *)
let sort entries =
  List.sort Location_list_entry.compare_ascending_vma entries

let create entries =
  { name = Linearize.new_label ();
    entries = sort entries;
  }

let label t = t.name

let end_marker = Value.as_code_address Nativeint.zero

let size t =
  let body_size =
    List.fold t.entries
      ~init:0
      ~f:(fun size entry -> size + Location_list_entry.size entry)
  in
  body_size + Value.size end_marker + Value.size end_marker

let compare_increasing_vma t1 t2 =
  match t1.entries, t2.entries with
  | t1_entry::_, t2_entry::_ ->
    Location_list_entry.compare_ascending_vma t1_entry t2_entry
  | _ -> failwith "Location_list.compare on empty location list(s)"

let emit t ~emitter =
  Emitter.emit_label_declaration emitter ~label_name:t.name;
  List.iter t.entries ~f:(Location_list_entry.emit ~emitter);
  (* DWARF-4 spec, section 2.6.2:
     The end of any given location list is marked by an end of list entry,
     which consists of a 0 for the beginning address offset and a 0 for the
     ending address offset. *)
  Value.emit end_marker ~emitter;
  Value.emit end_marker ~emitter
