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

type t = Attribute.t * Value.t

let create_low_pc ~address_label =
  Attribute.low_pc,
    Value.as_code_address_from_label address_label

let create_low_pc_from_symbol ~symbol =
  Attribute.low_pc,
    Value.as_code_address_from_symbol symbol

let create_high_pc ~address_label =
  Attribute.high_pc,
    Value.as_code_address_from_label address_label

let create_producer ~producer_name =
  Attribute.producer, Value.as_string producer_name

let create_name name =
  Attribute.name, Value.as_string name

let create_comp_dir ~directory =
  Attribute.comp_dir, Value.as_string directory

let create_stmt_list ~section_offset_label =
  Attribute.stmt_list,
    Value.as_four_byte_int_from_label section_offset_label

let create_external ~is_visible_externally =
  let flag = if is_visible_externally then 1 else 0 in
  Attribute.extern'l, Value.as_byte flag

(*
let create_location ~offset_from_start_of_debug_loc =
  Attribute.location,
    Value.as_four_byte_int offset_from_start_of_debug_loc
*)
let create_location ~location_list_label =
  Attribute.location, Value.as_code_address_from_label location_list_label

let create_type ~label_name =
  Attribute.typ', Value.as_code_address_from_label label_name

let create_encoding ~encoding =
  Attribute.encoding, Encoding_attribute.as_dwarf_value encoding

let create_sibling ~sibling =
  Attribute.sibling, Value.as_code_address_from_label sibling

let create_byte_size ~byte_size =
  assert (byte_size >= 1 && byte_size <= 0xff); (* CR mshinwell: not assert *)
  Attribute.byte_size, Value.as_byte byte_size

let create_linkage_name ~linkage_name =
  Attribute.linkage_name, Value.as_string linkage_name

let emit (_attr, value) ~emitter =
  Value.emit value ~emitter

let size (_attr, value) =
  Value.size value

let attribute (attr, _value) = attr
