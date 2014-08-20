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

type t

include Emittable.S with type t := t

val create_low_pc : address_label:Linearize.label -> t
val create_high_pc : address_label:Linearize.label -> t
val create_producer : producer_name:string -> t
val create_name : string -> t
val create_comp_dir : directory:string -> t
val create_stmt_list : section_offset_label:Linearize.label -> t
val create_external : is_visible_externally:bool -> t
val create_location : location_list_label:Linearize.label -> t
val create_type : label_name:string -> t
val create_encoding : encoding:Encoding_attribute.t -> t
val create_byte_size : byte_size:int -> t
val create_linkage_name : linkage_name:string -> t
val create_sibling : die_label:Linearize.label -> t

val attribute : t -> Attribute.t
