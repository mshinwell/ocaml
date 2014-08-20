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
  label : Linearize.label;
  abbreviation_code : Abbreviation_code.t;
  attribute_values : Attribute_value.t list;
}

let create ~label ~abbreviation_code ~attribute_values =
  { label;
    abbreviation_code;
    attribute_values;
  }

let null =
  lazy (
    { label = Linearize.new_label ();
      abbreviation_code = Abbreviation_code.null ();
      attribute_values = [];
    })

let create_null () = Lazy.force null

let emit t ~emitter =
  Emitter.emit_label emitter t.label;
  Abbreviation_code.emit t.abbreviation_code ~emitter;
  List.iter t.attribute_values ~f:(Attribute_value.emit ~emitter)

let size t =
  List.fold_left t.attribute_values
    ~init:(Abbreviation_code.size t.abbreviation_code)
    ~f:(fun size attribute_value ->
          size + Attribute_value.size attribute_value)
