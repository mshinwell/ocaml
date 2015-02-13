(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2015, Jane Street Holding                          *)
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

type t =
  | DW_TAG_array_type
  | DW_TAG_class_type
  | DW_TAG_entry_point
  | DW_TAG_enumeration_type
  | DW_TAG_formal_parameter
  | DW_TAG_imported_declaration
  | DW_TAG_label
  | DW_TAG_lexical_block
  | DW_TAG_member
  | DW_TAG_pointer_type
  | DW_TAG_reference_type
  | DW_TAG_compile_unit
  | DW_TAG_string_type
  | DW_TAG_structure_type
  | DW_TAG_subroutine_type
  | DW_TAG_typedef
  | DW_TAG_union_type
  | DW_TAG_unspecified_parameters
  | DW_TAG_variant
  | DW_TAG_common_block
  | DW_TAG_common_inclusion
  | DW_TAG_inheritance
  | DW_TAG_inlined_subroutine
  | DW_TAG_module
  | DW_TAG_ptr_to_member_type
  | DW_TAG_set_type
  | DW_TAG_subrange_type
  | DW_TAG_with_stmt
  | DW_TAG_access_declaration
  | DW_TAG_base_type
  | DW_TAG_catch_block
  | DW_TAG_const_type
  | DW_TAG_constant
  | DW_TAG_enumerator
  | DW_TAG_file_type
  | DW_TAG_friend
  | DW_TAG_namelist
  | DW_TAG_namelist_item
  | DW_TAG_packed_type
  | DW_TAG_subprogram
  | DW_TAG_template_type_parameter
  | DW_TAG_template_value_parameter
  | DW_TAG_thrown_type
  | DW_TAG_try_block
  | DW_TAG_variant_part
  | DW_TAG_variable
  | DW_TAG_volatile_type
  | DW_TAG_dwarf_procedure
  | DW_TAG_restrict_type
  | DW_TAG_interface_type
  | DW_TAG_namespace
  | DW_TAG_imported_module
  | DW_TAG_unspecified_type
  | DW_TAG_partial_unit
  | DW_TAG_imported_unit
  | DW_TAG_condition
  | DW_TAG_shared_type
  | DW_TAG_type_unit
  | DW_TAG_rvalue_reference_type
  | DW_TAG_template_alias
  | User of int

let to_string = function
  | DW_TAG_array_type -> "DW_TAG_array_type"
  | DW_TAG_class_type -> "DW_TAG_class_type"
  | DW_TAG_entry_point -> "DW_TAG_entry_point"
  | DW_TAG_enumeration_type -> "DW_TAG_enumeration_type"
  | DW_TAG_formal_parameter -> "DW_TAG_formal_parameter"
  | DW_TAG_imported_declaration -> "DW_TAG_imported_declaration"
  | DW_TAG_label -> "DW_TAG_label"
  | DW_TAG_lexical_block -> "DW_TAG_lexical_block"
  | DW_TAG_member -> "DW_TAG_member"
  | DW_TAG_pointer_type -> "DW_TAG_pointer_type"
  | DW_TAG_reference_type -> "DW_TAG_reference_type"
  | DW_TAG_compile_unit -> "DW_TAG_compile_unit"
  | DW_TAG_string_type -> "DW_TAG_string_type"
  | DW_TAG_structure_type -> "DW_TAG_structure_type"
  | DW_TAG_subroutine_type -> "DW_TAG_subroutine_type"
  | DW_TAG_typedef -> "DW_TAG_typedef"
  | DW_TAG_union_type -> "DW_TAG_union_type"
  | DW_TAG_unspecified_parameters -> "DW_TAG_unspecified_parameters"
  | DW_TAG_variant -> "DW_TAG_variant"
  | DW_TAG_common_block -> "DW_TAG_common_block"
  | DW_TAG_common_inclusion -> "DW_TAG_common_inclusion"
  | DW_TAG_inheritance -> "DW_TAG_inheritance"
  | DW_TAG_inlined_subroutine -> "DW_TAG_inlined_subroutine"
  | DW_TAG_module -> "DW_TAG_module"
  | DW_TAG_ptr_to_member_type -> "DW_TAG_ptr_to_member_type"
  | DW_TAG_set_type -> "DW_TAG_set_type"
  | DW_TAG_subrange_type -> "DW_TAG_subrange_type"
  | DW_TAG_with_stmt -> "DW_TAG_with_stmt"
  | DW_TAG_access_declaration -> "DW_TAG_access_declaration"
  | DW_TAG_base_type -> "DW_TAG_base_type"
  | DW_TAG_catch_block -> "DW_TAG_catch_block"
  | DW_TAG_const_type -> "DW_TAG_const_type"
  | DW_TAG_constant -> "DW_TAG_constant"
  | DW_TAG_enumerator -> "DW_TAG_enumerator"
  | DW_TAG_file_type -> "DW_TAG_file_type"
  | DW_TAG_friend -> "DW_TAG_friend"
  | DW_TAG_namelist -> "DW_TAG_namelist"
  | DW_TAG_namelist_item -> "DW_TAG_namelist_item"
  | DW_TAG_packed_type -> "DW_TAG_packed_type"
  | DW_TAG_subprogram -> "DW_TAG_subprogram"
  | DW_TAG_template_type_parameter -> "DW_TAG_template_type_parameter"
  | DW_TAG_template_value_parameter -> "DW_TAG_template_value_parameter"
  | DW_TAG_thrown_type -> "DW_TAG_thrown_type"
  | DW_TAG_try_block -> "DW_TAG_try_block"
  | DW_TAG_variant_part -> "DW_TAG_variant_part"
  | DW_TAG_variable -> "DW_TAG_variable"
  | DW_TAG_volatile_type -> "DW_TAG_volatile_type"
  | DW_TAG_dwarf_procedure -> "DW_TAG_dwarf_procedure"
  | DW_TAG_restrict_type -> "DW_TAG_restrict_type"
  | DW_TAG_interface_type -> "DW_TAG_interface_type"
  | DW_TAG_namespace -> "DW_TAG_namespace"
  | DW_TAG_imported_module -> "DW_TAG_imported_module"
  | DW_TAG_unspecified_type -> "DW_TAG_unspecified_type"
  | DW_TAG_partial_unit -> "DW_TAG_partial_unit"
  | DW_TAG_imported_unit -> "DW_TAG_imported_unit"
  | DW_TAG_condition -> "DW_TAG_condition"
  | DW_TAG_shared_type -> "DW_TAG_shared_type"
  | DW_TAG_type_unit -> "DW_TAG_type_unit"
  | DW_TAG_rvalue_reference_type -> "DW_TAG_rvalue_reference_type"
  | DW_TAG_template_alias -> "DW_TAG_template_alias"
  | User code -> Printf.sprintf "DW_TAG_<user>(%d)" code

let dw_tag_lo_user = 0x4080
let dw_tag_hi_user = 0xffff

let encode t =
  let code =
    match t with
    | DW_TAG_array_type -> 0x01
    | DW_TAG_class_type -> 0x02
    | DW_TAG_entry_point -> 0x03
    | DW_TAG_enumeration_type -> 0x04
    | DW_TAG_formal_parameter -> 0x05
    | DW_TAG_imported_declaration -> 0x08
    | DW_TAG_label -> 0x0a
    | DW_TAG_lexical_block -> 0x0b
    | DW_TAG_member -> 0x0d
    | DW_TAG_pointer_type -> 0x0f
    | DW_TAG_reference_type -> 0x10
    | DW_TAG_compile_unit -> 0x11
    | DW_TAG_string_type -> 0x12
    | DW_TAG_structure_type -> 0x13
    | DW_TAG_subroutine_type -> 0x15
    | DW_TAG_typedef -> 0x16 
    | DW_TAG_union_type -> 0x17
    | DW_TAG_unspecified_parameters -> 0x18
    | DW_TAG_variant -> 0x19
    | DW_TAG_common_block -> 0x1a
    | DW_TAG_common_inclusion -> 0x1b
    | DW_TAG_inheritance -> 0x1c
    | DW_TAG_inlined_subroutine -> 0x1d
    | DW_TAG_module -> 0x1e
    | DW_TAG_ptr_to_member_type -> 0x1f
    | DW_TAG_set_type -> 0x20
    | DW_TAG_subrange_type -> 0x21
    | DW_TAG_with_stmt -> 0x22
    | DW_TAG_access_declaration -> 0x23
    | DW_TAG_base_type -> 0x24
    | DW_TAG_catch_block -> 0x25
    | DW_TAG_const_type -> 0x26
    | DW_TAG_constant -> 0x27
    | DW_TAG_enumerator -> 0x28
    | DW_TAG_file_type -> 0x29
    | DW_TAG_friend -> 0x2a 
    | DW_TAG_namelist -> 0x2b
    | DW_TAG_namelist_item -> 0x2c
    | DW_TAG_packed_type -> 0x2d
    | DW_TAG_subprogram -> 0x2e
    | DW_TAG_template_type_parameter -> 0x2f
    | DW_TAG_template_value_parameter -> 0x30
    | DW_TAG_thrown_type -> 0x31
    | DW_TAG_try_block -> 0x32
    | DW_TAG_variant_part -> 0x33
    | DW_TAG_variable -> 0x34
    | DW_TAG_volatile_type -> 0x35
    | DW_TAG_dwarf_procedure -> 0x36
    | DW_TAG_restrict_type -> 0x37
    | DW_TAG_interface_type -> 0x38
    | DW_TAG_namespace -> 0x39
    | DW_TAG_imported_module -> 0x3a
    | DW_TAG_unspecified_type -> 0x3b
    | DW_TAG_partial_unit -> 0x3c
    | DW_TAG_imported_unit -> 0x3d
    | DW_TAG_condition -> 0x3f 
    | DW_TAG_shared_type -> 0x40
    | DW_TAG_type_unit -> 0x41
    | DW_TAG_rvalue_reference_type -> 0x42
    | DW_TAG_template_alias -> 0x43
    | User of code ->
      assert (code >= dw_tag_lo_user && code <= dw_tag_hi_user);
      code
  in
  Value.as_uleb128 code

let compile_unit = DW_TAG_compile_unit
let subprogram = DW_TAG_subprogram
let formal_parameter = DW_TAG_formal_parameter
let variable = DW_TAG_variable
let base_type = DW_TAG_base_type
let lexical_block = DW_TAG_lexical_block
let imported_declaration = DW_TAG_imported_declaration

let size t =
  Value.size (encode t)

let emit t ~emitter =
  Value.emit (encode t) ~emitter
