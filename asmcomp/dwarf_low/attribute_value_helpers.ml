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

module A = Attribute
module AV = Attribute_value
module F = Form

let create_low_pc ~address_label =
  AV.create A.Low_pc F.Addr (Value.code_address_from_label address_label)

let create_high_pc ~address_label =
  AV.create A.High_pc F.Addr (Value.code_address_from_label address_label)

let create_low_pc_from_symbol ~symbol =
  AV.create A.Low_pc F.Addr (Value.code_address_from_symbol ~symbol)

let create_high_pc_from_symbol ~symbol =
  AV.create A.High_pc F.Addr (Value.code_address_from_symbol ~symbol)

let create_producer ~producer_name =
  AV.create A.Producer F.Strp (Value.string producer_name)

let create_name name =
  AV.create A.Name F.Strp (Value.string name)

let create_comp_dir ~directory =
  AV.create A.Comp_dir F.Strp (Value.string name)

let create_stmt_list ~section_offset_label =
  (* DWARF-4 standard section 3.1.1.4. *)
  AV.create A.Stmt_list F.Lineptr
    (V.offset_into_debug_line section_offset_label)

let create_external ~is_visible_externally =
  if is_visible_externally then
    AV.create A.External F.Flag_present V.constant_true
  else
    AV.create A.External F.Flag (V.constant_bool false)

let create_location ~location_list_label =
  (* CR mshinwell: rename this function *)
  AV.create A.Location F.Sec_offset
    (V.offset_into_debug_loc location_list_label)

let create_single_location_description loc_desc =
  AV.create A.Location F.Exprloc (V.location_description loc_desc)

let create_encoding ~encoding =
  AV.create A.Encoding F.Data1 (Encoding_attribute.as_dwarf_value encoding)

let reference_proto_die attribute proto_die =
  AV.create attribute F.Ref_addr
    (V.offset_into_debug_info_from_label (Proto_DIE.reference proto_die))

let create_type ~proto_die = reference_proto_die A.Type proto_die
let create_sibling ~proto_die = reference_proto_die A.Sibling proto_die
let create_import ~proto_die = reference_proto_die A.Import proto_die
let create_specification_same_unit ~proto_die =
  reference_proto_die A.Specification proto_die

let create_specification_different_unit ~die_symbol =
  AV.create A.Specification F.Ref_addr
    (V.offset_into_debug_info_from_symbol ~symbol:die_symbol)

let create_byte_size_exn ~byte_size =
  AV.create A.Byte_size F.Data1 (V.constant_one_byte_int_exn byte_size)

let create_linkage_name ~linkage_name =
  AV.create A.Linkage_name F.Strp (V.indirect_string linkage_name)
