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

val create
   : source_file_path:string option
  -> emit_string:(string -> unit)
  -> emit_symbol:(string -> unit)
  -> emit_label:(Linearize.label -> unit)
  -> emit_label_declaration:(label_name:Linearize.label -> unit)
  -> emit_section_declaration:(section_name:string -> unit)
  -> emit_switch_to_section:(section_name:string -> unit)
  -> start_of_code_symbol:string
  -> end_of_code_symbol:string
  -> t

(* For the moment, matching pairs of [pre_emission_dwarf_for_function] and
   [post_emission_dwarf_for_function] should be called on function
   declarations in the same order (lowest address to highest) as they will be
   emitted in the executable file.  This is to ensure that the labels added
   to the linearized code end up with their integer ordering coinciding with
   program counter ordering.  (See CR in available_ranges.ml.)

   It is required that the label returned by
   [Available_ranges.start_of_function_label] on the return value of
   [pre_emission_dwarf_for_function] is emitted immediately after
   the starting symbol for the function concerned, before any prologue.  (We
   do not just use the function symbol since all of our location list
   calculations are based off labels, so we can exploit the ordering property
   mentioned above.)
*)
val pre_emission_dwarf_for_function
   : t
  -> fundecl:Linearize.fundecl
  -> Available_ranges.t

val post_emission_dwarf_for_function
   : t
  -> fundecl:Linearize.fundecl
  -> available_ranges:Available_ranges.t
  -> end_of_function_label:Linearize.label
  -> unit

val emit : t -> unit
