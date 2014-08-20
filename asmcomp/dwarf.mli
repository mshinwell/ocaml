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

(* A value of type [t] holds all state necessary to emit DWARF debugging
   information for a single compilation unit. *)
type t

val create : source_file_path:string
  -> emit_string:(string -> unit)
  -> emit_symbol:(string -> unit)
  -> emit_label_declaration:(label_name:string -> unit)
  -> emit_section_declaration:(section_name:string -> unit)
  -> emit_switch_to_section:(section_name:string -> unit)
  -> t

val emit : t -> unit
