(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2015, Jane Street Group, LLC                       *)
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
val emit_strings : t -> unit

val emit_string : t -> string -> unit
val emit_symbol : t -> string -> unit
val emit_label : t -> Linearize.label -> unit
val emit_label_declaration : t -> label_name:Linearize.label -> unit
val emit_section_declaration : t -> section_name:Section_names.t -> unit
val emit_switch_to_section : t -> section_name:Section_names.t -> unit

(* The following two functions assume we are aliasing a function symbol or
   label. *)
val emit_symbol_alias : t -> old_sym:string -> new_sym:string -> unit
val emit_symbol_to_label_alias
   : t
  -> old_label:Linearize.label
  -> new_sym:string
  -> unit

val target : t -> [ `MacOS_X | `Other ]

val emit_byte : t -> Int8.t -> unit
val emit_byte_exn : t -> int -> unit
val emit_two_byte_int : t -> Int16.t -> unit
val emit_four_byte_int : t -> Int32.t -> unit
val emit_eight_byte_int : t -> Int64.t -> unit
val emit_native_int : t -> Nativeint.t -> unit

val emit_uleb128 : t -> int -> unit
val emit_uleb128_from_int64 : t -> Int64.t -> unit
val emit_leb128 : t -> int -> unit
val emit_leb128_from_int64 : t -> Int64.t -> unit

val emit_offset_into_section
   : t
  -> [ `Label of Linearize.label | `Symbol of string ]
  -> Section_names.t
  -> unit
val emit_code_address_from_label : t -> Linearize.label -> unit
val emit_code_address_from_symbol : t -> string -> unit
