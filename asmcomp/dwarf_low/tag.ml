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

type user = Int16.t

type t =
  | Array_type
  | Class_type
  | Entry_point
  | Enumeration_type
  | Formal_parameter
  | Imported_declaration
  | Label
  | Lexical_block
  | Member
  | Pointer_type
  | Reference_type
  | Compile_unit
  | String_type
  | Structure_type
  | Subroutine_type
  | Typedef
  | Union_type
  | Unspecified_parameters
  | Variant
  | Common_block
  | Common_inclusion
  | Inheritance
  | Inlined_subroutine
  | Module
  | Ptr_to_member_type
  | Set_type
  | Subrange_type
  | With_stmt
  | Access_declaration
  | Base_type
  | Catch_block
  | Const_type
  | Constant
  | Enumerator
  | File_type
  | Friend
  | Namelist
  | Namelist_item
  | Packed_type
  | Subprogram
  | Template_type_parameter
  | Template_value_parameter
  | Thrown_type
  | Try_block
  | Variant_part
  | Variable
  | Volatile_type
  | Dwarf_procedure
  | Restrict_type
  | Interface_type
  | Namespace
  | Imported_module
  | Unspecified_type
  | Partial_unit
  | Imported_unit
  | Condition
  | Shared_type
  | Type_unit
  | Rvalue_reference_type
  | Template_alias
  | User of user

let to_string = function
  | Array_type -> "Array_type"
  | Class_type -> "Class_type"
  | Entry_point -> "Entry_point"
  | Enumeration_type -> "Enumeration_type"
  | Formal_parameter -> "Formal_parameter"
  | Imported_declaration -> "Imported_declaration"
  | Label -> "Label"
  | Lexical_block -> "Lexical_block"
  | Member -> "Member"
  | Pointer_type -> "Pointer_type"
  | Reference_type -> "Reference_type"
  | Compile_unit -> "Compile_unit"
  | String_type -> "String_type"
  | Structure_type -> "Structure_type"
  | Subroutine_type -> "Subroutine_type"
  | Typedef -> "Typedef"
  | Union_type -> "Union_type"
  | Unspecified_parameters -> "Unspecified_parameters"
  | Variant -> "Variant"
  | Common_block -> "Common_block"
  | Common_inclusion -> "Common_inclusion"
  | Inheritance -> "Inheritance"
  | Inlined_subroutine -> "Inlined_subroutine"
  | Module -> "Module"
  | Ptr_to_member_type -> "Ptr_to_member_type"
  | Set_type -> "Set_type"
  | Subrange_type -> "Subrange_type"
  | With_stmt -> "With_stmt"
  | Access_declaration -> "Access_declaration"
  | Base_type -> "Base_type"
  | Catch_block -> "Catch_block"
  | Const_type -> "Const_type"
  | Constant -> "Constant"
  | Enumerator -> "Enumerator"
  | File_type -> "File_type"
  | Friend -> "Friend"
  | Namelist -> "Namelist"
  | Namelist_item -> "Namelist_item"
  | Packed_type -> "Packed_type"
  | Subprogram -> "Subprogram"
  | Template_type_parameter -> "Template_type_parameter"
  | Template_value_parameter -> "Template_value_parameter"
  | Thrown_type -> "Thrown_type"
  | Try_block -> "Try_block"
  | Variant_part -> "Variant_part"
  | Variable -> "Variable"
  | Volatile_type -> "Volatile_type"
  | Dwarf_procedure -> "Dwarf_procedure"
  | Restrict_type -> "Restrict_type"
  | Interface_type -> "Interface_type"
  | Namespace -> "Namespace"
  | Imported_module -> "Imported_module"
  | Unspecified_type -> "Unspecified_type"
  | Partial_unit -> "Partial_unit"
  | Imported_unit -> "Imported_unit"
  | Condition -> "Condition"
  | Shared_type -> "Shared_type"
  | Type_unit -> "Type_unit"
  | Rvalue_reference_type -> "Rvalue_reference_type"
  | Template_alias -> "Template_alias"
  | User code -> Printf.sprintf "DW_TAG_<user>(%d)" code

let dw_tag_lo_user = 0x4080
let dw_tag_hi_user = 0xffff

let encode t =
  let code =
    match t with
    | Array_type -> 0x01
    | Class_type -> 0x02
    | Entry_point -> 0x03
    | Enumeration_type -> 0x04
    | Formal_parameter -> 0x05
    | Imported_declaration -> 0x08
    | Label -> 0x0a
    | Lexical_block -> 0x0b
    | Member -> 0x0d
    | Pointer_type -> 0x0f
    | Reference_type -> 0x10
    | Compile_unit -> 0x11
    | String_type -> 0x12
    | Structure_type -> 0x13
    | Subroutine_type -> 0x15
    | Typedef -> 0x16
    | Union_type -> 0x17
    | Unspecified_parameters -> 0x18
    | Variant -> 0x19
    | Common_block -> 0x1a
    | Common_inclusion -> 0x1b
    | Inheritance -> 0x1c
    | Inlined_subroutine -> 0x1d
    | Module -> 0x1e
    | Ptr_to_member_type -> 0x1f
    | Set_type -> 0x20
    | Subrange_type -> 0x21
    | With_stmt -> 0x22
    | Access_declaration -> 0x23
    | Base_type -> 0x24
    | Catch_block -> 0x25
    | Const_type -> 0x26
    | Constant -> 0x27
    | Enumerator -> 0x28
    | File_type -> 0x29
    | Friend -> 0x2a
    | Namelist -> 0x2b
    | Namelist_item -> 0x2c
    | Packed_type -> 0x2d
    | Subprogram -> 0x2e
    | Template_type_parameter -> 0x2f
    | Template_value_parameter -> 0x30
    | Thrown_type -> 0x31
    | Try_block -> 0x32
    | Variant_part -> 0x33
    | Variable -> 0x34
    | Volatile_type -> 0x35
    | Dwarf_procedure -> 0x36
    | Restrict_type -> 0x37
    | Interface_type -> 0x38
    | Namespace -> 0x39
    | Imported_module -> 0x3a
    | Unspecified_type -> 0x3b
    | Partial_unit -> 0x3c
    | Imported_unit -> 0x3d
    | Condition -> 0x3f
    | Shared_type -> 0x40
    | Type_unit -> 0x41
    | Rvalue_reference_type -> 0x42
    | Template_alias -> 0x43
    | User code ->
      assert (code >= dw_tag_lo_user && code <= dw_tag_hi_user);
      code
  in
  Value.as_uleb128 code

let parse ~stream =
  let open Or_error.Monad_infix in
  Stream.read_uleb128 stream
  >>= fun code ->
  match Uleb128.to_int code with
  | Error error -> Error (Error.tag error "bad DWARF tag")
  | Ok code ->
    match code with
    | 0x01 -> Ok Array_type
    | 0x02 -> Ok Class_type
    | 0x03 -> Ok Entry_point
    | 0x04 -> Ok Enumeration_type
    | 0x05 -> Ok Formal_parameter
    | 0x08 -> Ok Imported_declaration
    | 0x0a -> Ok Label
    | 0x0b -> Ok Lexical_block
    | 0x0d -> Ok Member
    | 0x0f -> Ok Pointer_type
    | 0x10 -> Ok Reference_type
    | 0x11 -> Ok Compile_unit
    | 0x12 -> Ok String_type
    | 0x13 -> Ok Structure_type
    | 0x15 -> Ok Subroutine_type
    | 0x16 -> Ok Typedef
    | 0x17 -> Ok Union_type
    | 0x18 -> Ok Unspecified_parameters
    | 0x19 -> Ok Variant
    | 0x1a -> Ok Common_block
    | 0x1b -> Ok Common_inclusion
    | 0x1c -> Ok Inheritance
    | 0x1d -> Ok Inlined_subroutine
    | 0x1e -> Ok Module
    | 0x1f -> Ok Ptr_to_member_type
    | 0x20 -> Ok Set_type
    | 0x21 -> Ok Subrange_type
    | 0x22 -> Ok With_stmt
    | 0x23 -> Ok Access_declaration
    | 0x24 -> Ok Base_type
    | 0x25 -> Ok Catch_block
    | 0x26 -> Ok Const_type
    | 0x27 -> Ok Constant
    | 0x28 -> Ok Enumerator
    | 0x29 -> Ok File_type
    | 0x2a -> Ok Friend
    | 0x2b -> Ok Namelist
    | 0x2c -> Ok Namelist_item
    | 0x2d -> Ok Packed_type
    | 0x2e -> Ok Subprogram
    | 0x2f -> Ok Template_type_parameter
    | 0x30 -> Ok Template_value_parameter
    | 0x31 -> Ok Thrown_type
    | 0x32 -> Ok Try_block
    | 0x33 -> Ok Variant_part
    | 0x34 -> Ok Variable
    | 0x35 -> Ok Volatile_type
    | 0x36 -> Ok Dwarf_procedure
    | 0x37 -> Ok Restrict_type
    | 0x38 -> Ok Interface_type
    | 0x39 -> Ok Namespace
    | 0x3a -> Ok Imported_module
    | 0x3b -> Ok Unspecified_type
    | 0x3c -> Ok Partial_unit
    | 0x3d -> Ok Imported_unit
    | 0x3f -> Ok Condition
    | 0x40 -> Ok Shared_type
    | 0x41 -> Ok Type_unit
    | 0x42 -> Ok Rvalue_reference_type
    | 0x43 -> Ok Template_alias
    | code ->
      if code >= dw_tag_lo_user && code <= dw_tag_hi_user then
        Ok (User (Int16.of_int64_exn code))
      else
        Error (Printf.sprintf "bad DWARF tag %Ld" code)

let size t =
  Value.size (encode t)

let emit t ~emitter =
  Value.emit (encode t) ~emitter
