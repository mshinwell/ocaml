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

type t = Attribute.t
  * [ `Value of Value.t
    | `Single_location_description of Single_location_description.t ]

let create_low_pc ~address_label =
  Attribute.low_pc,
    `Value (Value.as_code_address_from_label address_label)

let create_low_pc_from_symbol ~symbol =
  Attribute.low_pc,
    `Value (Value.as_code_address_from_symbol symbol)

let create_high_pc ~address_label =
  Attribute.high_pc,
    `Value (Value.as_code_address_from_label address_label)

let create_high_pc_from_symbol ~symbol =
  Attribute.high_pc,
    `Value (Value.as_code_address_from_symbol symbol)

let create_producer ~producer_name =
  Attribute.producer, `Value (Value.as_string producer_name)

let create_name name =
  Attribute.name, `Value (Value.as_string name)

let create_comp_dir ~directory =
  Attribute.comp_dir, `Value (Value.as_string directory)

let create_stmt_list ~section_offset_label =
  (* DWARF-4 standard section 3.1.1.4. *)
  Attribute.stmt_list,
    `Value (Value.as_offset_from_label section_offset_label
      ~section:Section_names.debug_line)

let create_external ~is_visible_externally =
  let flag = if is_visible_externally then 1 else 0 in
  Attribute.extern'l, `Value (Value.as_byte flag)

let create_location ~location_list_label =
  (* CR mshinwell: rename this function *)
  Attribute.location_using_location_list,
    `Value (Value.as_offset_from_label location_list_label
      ~section:Section_names.debug_loc)

let create_single_location_description sld =
  Attribute.location_using_single_location_description,
    (`Single_location_description sld)

let create_type ~proto_die =
  Attribute.typ',
    `Value (Value.as_offset_from_label proto_die
      ~section:Section_names.debug_info)

let create_encoding ~encoding =
  Attribute.encoding, `Value (Encoding_attribute.as_dwarf_value encoding)

let create_sibling ~proto_die =
  Attribute.sibling,
    `Value (Value.as_offset_from_label proto_die
      ~section:Section_names.debug_info)

let create_byte_size ~byte_size =
  assert (byte_size >= 1 && byte_size <= 0xff); (* CR mshinwell: not assert *)
  Attribute.byte_size, `Value (Value.as_byte byte_size)

let create_linkage_name ~linkage_name =
  Attribute.linkage_name, `Value (Value.as_string linkage_name)

let emit (_attr, thing) ~emitter =
  match thing with
  | `Value value -> Value.emit value ~emitter
  | `Single_location_description sld ->
    (* DWARF-4 standard section 7.5.4 (page 148), "exprloc". *)
    let size = Single_location_description.size sld in
    Value.emit (Value.as_uleb128_64 size) ~emitter;
    Single_location_description.emit sld ~emitter

let size (_attr, thing) =
  match thing with
  | `Value value -> Value.size value
  | `Single_location_description sld ->
    let size = Single_location_description.size sld in
    Int64.add (Value.size (Value.as_uleb128_64 size)) size

let attribute (attr, _thing) = attr
