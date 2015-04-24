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

(* Helper functions for constructing attribute values that do not
   require a knowledge of DWARF forms. *)

val create_low_pc
   : address_label:Linearize.label
  -> Attribute_value.t

val create_high_pc
   : address_label:Linearize.label
  -> Attribute_value.t

val create_low_pc_from_symbol
   : symbol:string
  -> Attribute_value.t

val create_high_pc_from_symbol
   : symbol:string
  -> Attribute_value.t

val create_producer
   : producer_name:string
  -> Attribute_value.t

val create_name
   : string
  -> Attribute_value.t

val create_comp_dir
   : directory:string
  -> Attribute_value.t

val create_stmt_list
   : section_offset_label:Linearize.label
  -> Attribute_value.t

val create_external
   : is_visible_externally:bool
  -> Attribute_value.t

val create_location
   : location_list_label:Linearize.label
  -> Attribute_value.t

val create_type
   : proto_die:Linearize.label
  -> Attribute_value.t

val create_import
   : proto_die:Linearize.label
  -> Attribute_value.t

val create_encoding
   : encoding:Encoding_attribute.t
  -> Attribute_value.t

val create_byte_size_exn
   : byte_size:int
  -> Attribute_value.t

val create_linkage_name
   : linkage_name:string
  -> Attribute_value.t

val create_sibling
   : proto_die:Linearize.label
  -> Attribute_value.t

val create_single_location_description
   : Single_location_description.t
  -> Attribute_value.t

(* DW_AT_specification across compilation units. *)
val create_specification_different_unit
   : die_symbol:string
  -> Attribute_value.t

(* DW_AT_specification within the same compilation unit. *)
val create_specification_same_unit
   : proto_die:Linearize.label
  -> Attribute_value.t
