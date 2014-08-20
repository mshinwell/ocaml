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
  label_name : string;
  abbreviation_code : Abbreviation_code.t;
  tag : Tag.t option;
  attribute_values : Attribute_value.t list;
}

let create ~label_name ~abbreviation_code ~tag ~attribute_values =
  { label_name;
    abbreviation_code;
    tag = Some tag;
    attribute_values;
  }

let create_null =
  let counter = ref 0 in
  fun () ->
  let count = !counter in
  counter := count + 1;
  { label_name = sprintf "null%d" count;
    abbreviation_code = Abbreviation_code.null ();
    tag = None;
    attribute_values = [];
  }

let emit t ~emitter =
  (* CR mshinwell: share code *)
  Emitter.emit_string emitter "Ldie__";
  Emitter.emit_symbol emitter t.label_name;
  Emitter.emit_string emitter ":\n";
  Abbreviation_code.emit t.abbreviation_code ~emitter;
  List.iter t.attribute_values ~f:(Attribute_value.emit ~emitter)

let size t =
  List.fold_left t.attribute_values
    ~init:(Abbreviation_code.size t.abbreviation_code)
    ~f:(fun size attribute_value ->
          size + Attribute_value.size attribute_value)

let to_abbreviations_table_entry t =
  match t.tag with
  | Some tag ->
    let attributes =
      List.map t.attribute_values ~f:Attribute_value.attribute
    in
    let entry =
      Abbreviations_table_entry.create
        ~abbreviation_code:t.abbreviation_code
        ~tag
        ~attributes
    in
    Some entry
  | None -> None
