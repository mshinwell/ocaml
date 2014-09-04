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

type t

val create : emit_string:(string -> unit)
  -> emit_symbol:(string -> unit)
  -> emit_label:(Linearize.label -> unit)
  -> emit_label_declaration:(label_name:Linearize.label -> unit)
  -> emit_section_declaration:(section_name:string -> unit)
  -> emit_switch_to_section:(section_name:string -> unit)
  -> target:[ `MacOS_X | `Other ]
  -> t

val cache_string : t -> string -> Linearize.label
val debug_str_label : t -> Linearize.label
val emit_strings : t -> unit

val emit_string : t -> string -> unit
val emit_symbol : t -> string -> unit
val emit_label : t -> Linearize.label -> unit
val emit_label_declaration : t -> label_name:Linearize.label -> unit
val emit_section_declaration : t -> section_name:Section_names.t -> unit
val emit_switch_to_section : t -> section_name:Section_names.t -> unit
val target : t -> [ `MacOS_X | `Other ]
