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

(* We omit the "DW_AT_" prefix. *)
type 'dwarf_classes t =
  | Sibling : Class.reference t
  | Location : Class.exprloc_or_loclistptr t
  | Name : Class.string t
  | Ordering : Class.constant t
  | Byte_size : Class.constant_or_exprloc_or_reference t
  | Bit_offset : Class.constant_or_exprloc_or_reference t
  | Bit_size : Class.constant_or_exprloc_or_reference t
  | Stmt_list : Class.lineptr t
  | Low_pc : Class.address t
  | High_pc : Class.address_or_constant t
  | Language : Class.constant t
  | Discr : Class.reference t
  | Discr_value : Class.constant t
  | Visibility : Class.constant t
  | Import : Class.reference t
  | String_length : Class.exprloc_or_loclistptr t
  | Common_reference : Class.reference t
  | Comp_dir : Class.string t
  | Const_value : Class.block_or_constant_or_string t
  | Containing_type : Class.reference t
  | Default_value : Class.reference t
  | Inline : Class.constant t
  | Is_optional : Class.flag t
  | Lower_bound : Class.constant_or_exprloc_or_reference t
  | Producer : Class.string t
  | Prototyped : Class.flag t
  | Return_addr : Class.exprloc_or_loclistptr t
  | Start_scope : Class.constant_or_rangelistptr t
  | Bit_stride : Class.constant_or_exprloc_or_reference t
  | Upper_bound : Class.constant_or_exprloc_or_reference t
  | Abstract_origin : Class.reference t
  | Accessibility : Class.constant t
  | Address_class : Class.constant t
  | Artificial : Class.flag t
  | Base_types : Class.reference t
  | Calling_convention : Class.constant t
  | Count : Class.constant_or_exprloc_or_reference t
  | Data_member_location : Class.constant_or_exprloc_or_loclistptr t
  | Decl_column : Class.constant t
  | Decl_file : Class.constant t
  | Decl_line : Class.constant t
  | Declaration : Class.flag t
  | Discr_list : Class.block t
  | Encoding : Class.constant t
  | External : Class.flag t
  | Frame_base : Class.exprloc_or_loclistptr t
  | Friend : Class.reference t
  | Identifier_case : Class.constant t
  | Macro_info : Class.macptr t
  | Namelist_item : Class.reference t
  | Priority : Class.reference t
  | Segment : Class.exprloc_or_loclistptr t
  | Specification : Class.reference t
  | Static_link : Class.exprloc_or_loclistptr t
  | Type : Class.reference t
  | Use_location : Class.exprloc_or_loclistptr t
  | Variable_parameter : Class.flag t
  | Virtuality : Class.constant t
  | Vtable_elem_location : Class.exprloc_or_loclistptr t
  | Allocated : Class.constant_or_exprloc_or_reference t
  | Associated : Class.constant_or_exprloc_or_reference t
  | Data_location : Class.exprloc t
  | Byte_stride : Class.constant_or_exprloc_or_reference t
  | Entry_pc : Class.address t
  | Use_UTF8 : Class.flag t
  | Extension : Class.reference t
  | Ranges : Class.rangelistptr t
  | Trampoline : Class.address_or_flag_or_reference_or_string t
  | Call_column : Class.constant t
  | Call_file : Class.constant t
  | Call_line : Class.constant t
  | Description : Class.string t
  | Binary_scale : Class.constant t
  | Decimal_scale : Class.constant t
  | Small : Class.reference t
  | Decimal_sign : Class.constant t
  | Digit_count : Class.constant t
  | Picture_string : Class.string t
  | Mutable : Class.flag t
  | Threads_scaled : Class.flag t
  | Explicit : Class.flag t
  | Object_pointer : Class.reference t
  | Endianity : Class.constant t
  | Elemental : Class.flag t
  | Pure : Class.flag t
  | Recursive : Class.flag t
  | Signature : Class.reference t
  | Main_subprogram : Class.flag t
  | Data_bit_offset : Class.constant t
  | Const_expr : Class.flag t
  | Enum_class : Class.flag t
  | Linkage_name : Class.string t

let low_user = 0x2000
let hi_user = 0x3fff

let compare = Pervasives.compare

let parse ~stream =
  Stream.read_uleb128_as_int stream
  >>= function
  | 0x00 -> Ok None
  | 0x01 -> Ok (Some Sibling)
  | 0x02 -> Ok (Some Location)
  | 0x03 -> Ok (Some Name)
  | 0x09 -> Ok (Some Ordering)
  | 0x0b -> Ok (Some Byte_size)
  | 0x0c -> Ok (Some Bit_offset)
  | 0x0d -> Ok (Some Bit_size)
  | 0x10 -> Ok (Some Stmt_list)
  | 0x11 -> Ok (Some Low_pc)
  | 0x12 -> Ok (Some High_pc)
  | 0x13 -> Ok (Some Language)
  | 0x15 -> Ok (Some Discr)
  | 0x16 -> Ok (Some Discr_value)
  | 0x17 -> Ok (Some Visibility)
  | 0x18 -> Ok (Some Import)
  | 0x19 -> Ok (Some String_length)
  | 0x1a -> Ok (Some Common_reference)
  | 0x1b -> Ok (Some Comp_dir)
  | 0x1c -> Ok (Some Const_value)
  | 0x1d -> Ok (Some Containing_type)
  | 0x1e -> Ok (Some Default_value)
  | 0x20 -> Ok (Some Inline)
  | 0x21 -> Ok (Some Is_optional)
  | 0x22 -> Ok (Some Lower_bound)
  | 0x25 -> Ok (Some Producer)
  | 0x27 -> Ok (Some Prototyped)
  | 0x2a -> Ok (Some Return_addr)
  | 0x2c -> Ok (Some Start_scope)
  | 0x2e -> Ok (Some Bit_stride)
  | 0x2f -> Ok (Some Upper_bound)
  | 0x31 -> Ok (Some Abstract_origin)
  | 0x32 -> Ok (Some Accessibility)
  | 0x33 -> Ok (Some Address_class)
  | 0x34 -> Ok (Some Artificial)
  | 0x35 -> Ok (Some Base_types)
  | 0x36 -> Ok (Some Calling_convention)
  | 0x37 -> Ok (Some Count)
  | 0x38 -> Ok (Some Data_member_location)
  | 0x39 -> Ok (Some Decl_column)
  | 0x3a -> Ok (Some Decl_file)
  | 0x3b -> Ok (Some Decl_line)
  | 0x3c -> Ok (Some Declaration)
  | 0x3d -> Ok (Some Discr_list)
  | 0x3e -> Ok (Some Encoding)
  | 0x3f -> Ok (Some External)
  | 0x40 -> Ok (Some Frame_base)
  | 0x41 -> Ok (Some Friend)
  | 0x42 -> Ok (Some Identifier_case)
  | 0x43 -> Ok (Some Macro_info)
  | 0x44 -> Ok (Some Namelist_item)
  | 0x45 -> Ok (Some Priority)
  | 0x46 -> Ok (Some Segment)
  | 0x47 -> Ok (Some Specification)
  | 0x48 -> Ok (Some Static_link)
  | 0x49 -> Ok (Some Type)
  | 0x4a -> Ok (Some Use_location)
  | 0x4b -> Ok (Some Variable_parameter)
  | 0x4c -> Ok (Some Virtuality)
  | 0x4d -> Ok (Some Vtable_elem_location)
  | 0x4e -> Ok (Some Allocated)
  | 0x4f -> Ok (Some Associated)
  | 0x50 -> Ok (Some Data_location)
  | 0x51 -> Ok (Some Byte_stride)
  | 0x52 -> Ok (Some Entry_pc)
  | 0x53 -> Ok (Some Use_UTF8)
  | 0x54 -> Ok (Some Extension)
  | 0x55 -> Ok (Some Ranges)
  | 0x56 -> Ok (Some Trampoline)
  | 0x57 -> Ok (Some Call_column)
  | 0x58 -> Ok (Some Call_file)
  | 0x59 -> Ok (Some Call_line)
  | 0x5a -> Ok (Some Description)
  | 0x5b -> Ok (Some Binary_scale)
  | 0x5c -> Ok (Some Decimal_scale)
  | 0x5d -> Ok (Some Small)
  | 0x5e -> Ok (Some Decimal_sign)
  | 0x5f -> Ok (Some Digit_count)
  | 0x60 -> Ok (Some Picture_string)
  | 0x61 -> Ok (Some Mutable)
  | 0x62 -> Ok (Some Threads_scaled)
  | 0x63 -> Ok (Some Explicit)
  | 0x64 -> Ok (Some Object_pointer)
  | 0x65 -> Ok (Some Endianity)
  | 0x66 -> Ok (Some Elemental)
  | 0x67 -> Ok (Some Pure)
  | 0x68 -> Ok (Some Recursive)
  | 0x69 -> Ok (Some Signature)
  | 0x6a -> Ok (Some Main_subprogram)
  | 0x6b -> Ok (Some Data_bit_offset)
  | 0x6c -> Ok (Some Const_expr)
  | 0x6d -> Ok (Some Enum_class)
  | 0x6e -> Ok (Some Linkage_name)
  | code when code >= lo_user && code <= hi_user -> Ok (Some (User code))
  | code -> Error (Printf.sprintf "unknown DWARF attribute code 0x%x" code)

let encode = function
  | Sibling -> 0x01
  | Location -> 0x02
  | Name -> 0x03
  | Ordering -> 0x09
  | Byte_size -> 0x0b
  | Bit_offset -> 0x0c
  | Bit_size -> 0x0d
  | Stmt_list -> 0x10
  | Low_pc -> 0x11
  | High_pc -> 0x12
  | Language -> 0x13
  | Discr -> 0x15
  | Discr_value -> 0x16
  | Visibility -> 0x17
  | Import -> 0x18
  | String_length -> 0x19
  | Common_reference -> 0x1a
  | Comp_dir -> 0x1b
  | Const_value -> 0x1c
  | Containing_type -> 0x1d
  | Default_value -> 0x1e
  | Inline -> 0x20
  | Is_optional -> 0x21
  | Lower_bound -> 0x22
  | Producer -> 0x25
  | Prototyped -> 0x27
  | Return_addr -> 0x2a
  | Start_scope -> 0x2c
  | Bit_stride -> 0x2e
  | Upper_bound -> 0x2f
  | Abstract_origin -> 0x31
  | Accessibility -> 0x32
  | Address_class -> 0x33
  | Artificial -> 0x34
  | Base_types -> 0x35
  | Calling_convention -> 0x36
  | Count -> 0x37
  | Data_member_location -> 0x38
  | Decl_column -> 0x39
  | Decl_file -> 0x3a
  | Decl_line -> 0x3b
  | Declaration -> 0x3c
  | Discr_list -> 0x3d
  | Encoding -> 0x3e
  | External -> 0x3f
  | Frame_base -> 0x40
  | Friend -> 0x41
  | Identifier_case -> 0x42
  | Macro_info -> 0x43
  | Namelist_item -> 0x44
  | Priority -> 0x45
  | Segment -> 0x46
  | Specification -> 0x47
  | Static_link -> 0x48
  | Type -> 0x49
  | Use_location -> 0x4a
  | Variable_parameter -> 0x4b
  | Virtuality -> 0x4c
  | Vtable_elem_location -> 0x4d
  | Allocated -> 0x4e
  | Associated -> 0x4f
  | Data_location -> 0x50
  | Byte_stride -> 0x51
  | Entry_pc -> 0x52
  | Use_UTF8 -> 0x53
  | Extension -> 0x54
  | Ranges -> 0x55
  | Trampoline -> 0x56
  | Call_column -> 0x57
  | Call_file -> 0x58
  | Call_line -> 0x59
  | Description -> 0x5a
  | Binary_scale -> 0x5b
  | Decimal_scale -> 0x5c
  | Small -> 0x5d
  | Decimal_sign -> 0x5e
  | Digit_count -> 0x5f
  | Picture_string -> 0x60
  | Mutable -> 0x61
  | Threads_scaled -> 0x62
  | Explicit -> 0x63
  | Object_pointer -> 0x64
  | Endianity -> 0x65
  | Elemental -> 0x66
  | Pure -> 0x67
  | Recursive -> 0x68
  | Signature -> 0x69
  | Main_subprogram -> 0x6a
  | Data_bit_offset -> 0x6b
  | Const_expr -> 0x6c
  | Enum_class -> 0x6d
  | Linkage_name -> 0x6e
  | User code -> code

let size t =
  Variable_length_encoding.uleb128_size (encode t)

let emit t ~emitter =
  Emitter.emit_uleb128 emitter (encode t)
