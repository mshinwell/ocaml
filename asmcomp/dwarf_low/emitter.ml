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

type t = {
  emit_string : string -> unit;
  emit_symbol : string -> unit;
  emit_label : Linearize.label -> unit;
  emit_label_declaration : label_name:Linearize.label -> unit;
  emit_section_declaration : section_name:string -> unit;
  emit_switch_to_section : section_name:string -> unit;
  target : [ `MacOS_X | `Other ];
  mutable strings : (Linearize.label * string) list;
  debug_str_label : Linearize.label;
}

let create ~emit_string ~emit_symbol ~emit_label ~emit_label_declaration
           ~emit_section_declaration ~emit_switch_to_section ~target =
  { emit_string; emit_symbol; emit_label; emit_label_declaration;
    emit_section_declaration; emit_switch_to_section; target;
    strings = [];
    debug_str_label = Linearize.new_label ();
  }

(* CR-soon mshinwell: work out a way of sharing labels *)
let cache_string t s =
  let label =
    if t.strings = [] then t.debug_str_label else Linearize.new_label ()
  in
  t.strings <- (label, s)::t.strings;
  label

let debug_str_label t = t.debug_str_label

let emit_strings t =
  ListLabels.iter t.strings
    ~f:(fun (label_name, s) ->
          t.emit_label_declaration ~label_name;
          match t.target with
          | `MacOS_X ->
            t.emit_string (Printf.sprintf "\t.asciz\t\"%s\"\n" s)
          | `Other ->
            t.emit_string (Printf.sprintf "\t.string\t\"%s\"\n" s))

let emit_string t = t.emit_string
let emit_symbol t = t.emit_symbol
let emit_label t = t.emit_label
let emit_label_declaration t = t.emit_label_declaration
let emit_section_declaration t = t.emit_section_declaration
let emit_switch_to_section t = t.emit_switch_to_section
let target t = t.target
