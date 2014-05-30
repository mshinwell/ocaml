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
  abbreviation_code : Abbreviation_code.t;
  tag : Tag.t;
  attributes : Attribute.t list;
}

let create ~abbreviation_code ~tag ~attributes =
  { abbreviation_code;
    tag;
    attributes;
  }

let size t =
  Abbreviation_code.size t.abbreviation_code
    + Tag.size t.tag
    + Child_determination.size (Tag.child_determination t.tag)
    + List.fold t.attributes
        ~init:0
        ~f:(fun size attr -> size + Attribute.size attr)
    + Value.size (Value.as_uleb128 0)
    + Value.size (Value.as_uleb128 0)

let emit t ~emitter =
  Abbreviation_code.emit t.abbreviation_code ~emitter;
  Tag.emit t.tag ~emitter;
  Child_determination.emit (Tag.child_determination t.tag) ~emitter;
  List.iter t.attributes ~f:(Attribute.emit ~emitter);
  Value.emit (Value.as_uleb128 0) ~emitter;
  Value.emit (Value.as_uleb128 0) ~emitter
