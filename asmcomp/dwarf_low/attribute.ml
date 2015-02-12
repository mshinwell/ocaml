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

type t =
  | DW_AT_low_pc
  | DW_AT_high_pc
  | DW_AT_name
  | DW_AT_comp_dir
  | DW_AT_producer
  | DW_AT_stmt_list
  | DW_AT_external
  (* DW_AT_location may have two different classes (DWARF-4 standard
     section 2.6, page 26). *)
  | DW_AT_location of [ `exprloc | `loclistptr ]
  | DW_AT_type
  | DW_AT_encoding
  | DW_AT_byte_size
  | DW_AT_linkage_name
  | DW_AT_sibling
  | DW_AT_import
  | DW_AT_specification

let dw_at_low_user = 0x2000
let dw_at_hi_user = 0x3fff

let compare t1 t2 =
  compare (encode t1) (encode t2)

let form = function
  | DW_AT_low_pc -> Form.addr
  | DW_AT_high_pc -> Form.addr
  | DW_AT_name -> Form.strp
  | DW_AT_comp_dir -> Form.strp
  | DW_AT_producer -> Form.strp
  | DW_AT_stmt_list -> Form.sec_offset
  | DW_AT_external -> Form.flag
  | DW_AT_location `exprloc -> Form.exprloc
  | DW_AT_location `loclistptr -> Form.sec_offset
  | DW_AT_type -> Form.ref_addr
  | DW_AT_encoding -> Form.data1
  | DW_AT_byte_size -> Form.data1
  | DW_AT_linkage_name -> Form.strp
  | DW_AT_sibling -> Form.ref_addr
  | DW_AT_import -> Form.ref_addr
  | DW_AT_specification -> Form.ref_addr

let low_pc = DW_AT_low_pc
let high_pc = DW_AT_high_pc
let producer = DW_AT_producer
let name = DW_AT_name
let comp_dir = DW_AT_comp_dir
let stmt_list = DW_AT_stmt_list
let extern'l = DW_AT_external
let location_using_single_location_description = DW_AT_location `exprloc
let location_using_location_list = DW_AT_location `loclistptr
let typ' = DW_AT_type
let encoding = DW_AT_encoding
let byte_size = DW_AT_byte_size
let linkage_name = DW_AT_linkage_name
let sibling = DW_AT_sibling
let import = DW_AT_import
let specification = DW_AT_specification

let size t =
  Int64.add (Value.size (Value.as_uleb128 (encode t)))
    (Form.size (form t))

let emit t ~emitter =
  Value.emit (Value.as_uleb128 (encode t)) ~emitter;
  Form.emit (form t) ~emitter



type 'value_types t = t
  | DW_AT_sibling : Class.reference t
  | DW_AT_location : Class.exprloc_or_loclistptr t
  | DW_AT_name : Class.string t
  | DW_AT_ordering : Class.constant t
  | DW_AT_byte_size : Class.constant_or_exprloc_or_reference t
  | DW_AT_bit_offset : Class.constant_or_exprloc_or_reference t
  | DW_AT_bit_size : Class.constant_or_exprloc_or_reference t
  | DW_AT_stmt_list : Class.lineptr t
  | DW_AT_low_pc : Class.address t
  | DW_AT_high_pc : Class.address_or_constant t
  | DW_AT_language : Class.constant t
  | DW_AT_discr : Class.reference t
  | DW_AT_discr_value : Class.constant t
  | DW_AT_visibility : Class.constant t
  | DW_AT_import : Class.reference t
  | DW_AT_string_length : Class.exprloc_or_loclistptr t
  | DW_AT_common_reference : Class.reference t
  | DW_AT_comp_dir : Class.string t
  | DW_AT_const_value : Class.block_or_constant_or_string t
  | DW_AT_containing_type : Class.reference t
  | DW_AT_default_value : Class.reference t
  | DW_AT_inline : Class.constant t
  | DW_AT_is_optional : Class.flag t
  | DW_AT_lower_bound : Class.constant_or_exprloc_or_reference t
  | DW_AT_producer : Class.string t
  | DW_AT_prototyped : Class.flag t
  | DW_AT_return_addr : Class.exprloc_or_loclistptr t
  | DW_AT_start_scope : Class.constant_or_rangelistptr t
  | DW_AT_bit_stride : Class.constant_or_exprloc_or_reference t
  | DW_AT_upper_bound : Class.constant_or_exprloc_or_reference t
  | DW_AT_abstract_origin : Class.reference t
  | DW_AT_accessibility : Class.constant t
  | DW_AT_address_class : Class.constant t
  | DW_AT_artificial : Class.flag t
  | DW_AT_base_types : Class.reference t
  | DW_AT_calling_convention : Class.constant t
  | DW_AT_count : Class.constant_or_exprloc_or_reference t
  | DW_AT_data_member_location : Class.constant_or_exprloc_or_loclistptr t
  | DW_AT_decl_column : Class.constant t
  | DW_AT_decl_file : Class.constant t
  | DW_AT_decl_line : Class.constant t
  | DW_AT_declaration : Class.flag t
  | DW_AT_discr_list : Class.block t
  | DW_AT_encoding : Class.constant t
  | DW_AT_external : Class.flag t
  | DW_AT_frame_base : Class.exprloc_or_loclistptr t
  | DW_AT_friend : Class.reference t
  | DW_AT_identifier_case : Class.constant t
  | DW_AT_macro_info : Class.macptr t
  | DW_AT_namelist_item : Class.reference t
  | DW_AT_priority : Class.reference t
  | DW_AT_segment : Class.exprloc_or_loclistptr t
  | DW_AT_specification : Class.reference t
  | DW_AT_static_link : Class.exprloc_or_loclistptr t
  | DW_AT_type : Class.reference t
  | DW_AT_use_location : Class.exprloc_or_loclistptr t
  | DW_AT_variable_parameter : Class.flag t
  | DW_AT_virtuality : Class.constant t
  | DW_AT_vtable_elem_location : Class.exprloc_or_loclistptr t
  | DW_AT_allocated : Class.constant_or_exprloc_or_reference t
  | DW_AT_associated : Class.constant_or_exprloc_or_reference t
  | DW_AT_data_location : Class.exprloc t
  | DW_AT_byte_stride : Class.constant_or_exprloc_or_reference t
  | DW_AT_entry_pc : Class.address t
  | DW_AT_use_UTF8 : Class.flag t
  | DW_AT_extension : Class.reference t
  | DW_AT_ranges : Class.rangelistptr t
  | DW_AT_trampoline : Class.address_or_flag_or_reference_or_string t
  | DW_AT_call_column : Class.constant t
  | DW_AT_call_file : Class.constant t
  | DW_AT_call_line : Class.constant t
  | DW_AT_description : Class.string t
  | DW_AT_binary_scale : Class.constant t
  | DW_AT_decimal_scale : Class.constant t
  | DW_AT_small : Class.reference t
  | DW_AT_decimal_sign : Class.constant t
  | DW_AT_digit_count : Class.constant t
  | DW_AT_picture_string : Class.string t
  | DW_AT_mutable : Class.flag t
  | DW_AT_threads_scaled : Class.flag t
  | DW_AT_explicit : Class.flag t
  | DW_AT_object_pointer : Class.reference t
  | DW_AT_endianity : Class.constant t
  | DW_AT_elemental : Class.flag t
  | DW_AT_pure : Class.flag t
  | DW_AT_recursive : Class.flag t
  | DW_AT_signature : Class.reference t
  | DW_AT_main_subprogram : Class.flag t
  | DW_AT_data_bit_offset : Class.constant t
  | DW_AT_const_expr : Class.flag t
  | DW_AT_enum_class : Class.flag t
  | DW_AT_linkage_name : Class.string t

let parse ~stream =
  Stream.read_uleb128_as_int64 stream
  >>= fun code ->
  Form.parse ~stream
  >>= fun form ->
  let t =
    match code with
    | 0x01 -> Some DW_AT_sibling
    | 0x02 -> Some DW_AT_location
    | 0x03 -> Some DW_AT_name
    | 0x09 -> Some DW_AT_ordering
    | 0x0b -> Some DW_AT_byte_size
    | 0x0c -> Some DW_AT_bit_offset
    | 0x0d -> Some DW_AT_bit_size
    | 0x10 -> Some DW_AT_stmt_list
    | 0x11 -> Some DW_AT_low_pc
    | 0x12 -> Some DW_AT_high_pc
    | 0x13 -> Some DW_AT_language
    | 0x15 -> Some DW_AT_discr
    | 0x16 -> Some DW_AT_discr_value
    | 0x17 -> Some DW_AT_visibility
    | 0x18 -> Some DW_AT_import
    | 0x19 -> Some DW_AT_string_length
    | 0x1a -> Some DW_AT_common_reference
    | 0x1b -> Some DW_AT_comp_dir
    | 0x1c -> Some DW_AT_const_value
    | 0x1d -> Some DW_AT_containing_type
    | 0x1e -> Some DW_AT_default_value
    | 0x20 -> Some DW_AT_inline
    | 0x21 -> Some DW_AT_is_optional
    | 0x22 -> Some DW_AT_lower_bound
    | 0x25 -> Some DW_AT_producer
    | 0x27 -> Some DW_AT_prototyped
    | 0x2a -> Some DW_AT_return_addr
    | 0x2c -> Some DW_AT_start_scope
    | 0x2e -> Some DW_AT_bit_stride
    | 0x2f -> Some DW_AT_upper_bound
    | 0x31 -> Some DW_AT_abstract_origin
    | 0x32 -> Some DW_AT_accessibility
    | 0x33 -> Some DW_AT_address_class
    | 0x34 -> Some DW_AT_artificial
    | 0x35 -> Some DW_AT_base_types
    | 0x36 -> Some DW_AT_calling_convention
    | 0x37 -> Some DW_AT_count
    | 0x38 -> Some DW_AT_data_member_location
    | 0x39 -> Some DW_AT_decl_column
    | 0x3a -> Some DW_AT_decl_file
    | 0x3b -> Some DW_AT_decl_line
    | 0x3c -> Some DW_AT_declaration
    | 0x3d -> Some DW_AT_discr_list
    | 0x3e -> Some DW_AT_encoding
    | 0x3f -> Some DW_AT_external
    | 0x40 -> Some DW_AT_frame_base
    | 0x41 -> Some DW_AT_friend
    | 0x42 -> Some DW_AT_identifier_case
    | 0x43 -> Some DW_AT_macro_info
    | 0x44 -> Some DW_AT_namelist_item
    | 0x45 -> Some DW_AT_priority
    | 0x46 -> Some DW_AT_segment
    | 0x47 -> Some DW_AT_specification
    | 0x48 -> Some DW_AT_static_link
    | 0x49 -> Some DW_AT_type
    | 0x4a -> Some DW_AT_use_location
    | 0x4b -> Some DW_AT_variable_parameter
    | 0x4c -> Some DW_AT_virtuality
    | 0x4d -> Some DW_AT_vtable_elem_location
    | 0x4e -> Some DW_AT_allocated
    | 0x4f -> Some DW_AT_associated
    | 0x50 -> Some DW_AT_data_location
    | 0x51 -> Some DW_AT_byte_stride
    | 0x52 -> Some DW_AT_entry_pc
    | 0x53 -> Some DW_AT_use_UTF8
    | 0x54 -> Some DW_AT_extension
    | 0x55 -> Some DW_AT_ranges
    | 0x56 -> Some DW_AT_trampoline
    | 0x57 -> Some DW_AT_call_column
    | 0x58 -> Some DW_AT_call_file
    | 0x59 -> Some DW_AT_call_line
    | 0x5a -> Some DW_AT_description
    | 0x5b -> Some DW_AT_binary_scale
    | 0x5c -> Some DW_AT_decimal_scale
    | 0x5d -> Some DW_AT_small
    | 0x5e -> Some DW_AT_decimal_sign
    | 0x5f -> Some DW_AT_digit_count
    | 0x60 -> Some DW_AT_picture_string
    | 0x61 -> Some DW_AT_mutable
    | 0x62 -> Some DW_AT_threads_scaled
    | 0x63 -> Some DW_AT_explicit
    | 0x64 -> Some DW_AT_object_pointer
    | 0x65 -> Some DW_AT_endianity
    | 0x66 -> Some DW_AT_elemental
    | 0x67 -> Some DW_AT_pure
    | 0x68 -> Some DW_AT_recursive
    | 0x69 -> Some DW_AT_signature
    | 0x6a -> Some DW_AT_main_subprogram
    | 0x6b -> Some DW_AT_data_bit_offset
    | 0x6c -> Some DW_AT_const_expr
    | 0x6d -> Some DW_AT_enum_class
    | 0x6e -> Some DW_AT_linkage_name
    | code when code >= dw_at_lo_user && code <= dw_at_hi_user ->
      Some (User code)
    | _code -> 
      None
  in

let encode = function
  | DW_AT_sibling -> 0x01
  | DW_AT_location -> 0x02
  | DW_AT_name -> 0x03
  | DW_AT_ordering -> 0x09
  | DW_AT_byte_size -> 0x0b
  | DW_AT_bit_offset -> 0x0c
  | DW_AT_bit_size -> 0x0d
  | DW_AT_stmt_list -> 0x10
  | DW_AT_low_pc -> 0x11
  | DW_AT_high_pc -> 0x12
  | DW_AT_language -> 0x13
  | DW_AT_discr -> 0x15
  | DW_AT_discr_value -> 0x16
  | DW_AT_visibility -> 0x17
  | DW_AT_import -> 0x18
  | DW_AT_string_length -> 0x19
  | DW_AT_common_reference -> 0x1a
  | DW_AT_comp_dir -> 0x1b
  | DW_AT_const_value -> 0x1c
  | DW_AT_containing_type -> 0x1d
  | DW_AT_default_value -> 0x1e
  | DW_AT_inline -> 0x20
  | DW_AT_is_optional -> 0x21
  | DW_AT_lower_bound -> 0x22
  | DW_AT_producer -> 0x25
  | DW_AT_prototyped -> 0x27
  | DW_AT_return_addr -> 0x2a
  | DW_AT_start_scope -> 0x2c
  | DW_AT_bit_stride -> 0x2e
  | DW_AT_upper_bound -> 0x2f
  | DW_AT_abstract_origin -> 0x31
  | DW_AT_accessibility -> 0x32
  | DW_AT_address_class -> 0x33
  | DW_AT_artificial -> 0x34
  | DW_AT_base_types -> 0x35
  | DW_AT_calling_convention -> 0x36
  | DW_AT_count -> 0x37
  | DW_AT_data_member_location -> 0x38
  | DW_AT_decl_column -> 0x39
  | DW_AT_decl_file -> 0x3a
  | DW_AT_decl_line -> 0x3b
  | DW_AT_declaration -> 0x3c
  | DW_AT_discr_list -> 0x3d
  | DW_AT_encoding -> 0x3e
  | DW_AT_external -> 0x3f
  | DW_AT_frame_base -> 0x40
  | DW_AT_friend -> 0x41
  | DW_AT_identifier_case -> 0x42
  | DW_AT_macro_info -> 0x43
  | DW_AT_namelist_item -> 0x44
  | DW_AT_priority -> 0x45
  | DW_AT_segment -> 0x46
  | DW_AT_specification -> 0x47
  | DW_AT_static_link -> 0x48
  | DW_AT_type -> 0x49
  | DW_AT_use_location -> 0x4a
  | DW_AT_variable_parameter -> 0x4b
  | DW_AT_virtuality -> 0x4c
  | DW_AT_vtable_elem_location -> 0x4d
  | DW_AT_allocated -> 0x4e
  | DW_AT_associated -> 0x4f
  | DW_AT_data_location -> 0x50
  | DW_AT_byte_stride -> 0x51
  | DW_AT_entry_pc -> 0x52
  | DW_AT_use_UTF8 -> 0x53
  | DW_AT_extension -> 0x54
  | DW_AT_ranges -> 0x55
  | DW_AT_trampoline -> 0x56
  | DW_AT_call_column -> 0x57
  | DW_AT_call_file -> 0x58
  | DW_AT_call_line -> 0x59
  | DW_AT_description -> 0x5a
  | DW_AT_binary_scale -> 0x5b
  | DW_AT_decimal_scale -> 0x5c
  | DW_AT_small -> 0x5d
  | DW_AT_decimal_sign -> 0x5e
  | DW_AT_digit_count -> 0x5f
  | DW_AT_picture_string -> 0x60
  | DW_AT_mutable -> 0x61
  | DW_AT_threads_scaled -> 0x62
  | DW_AT_explicit -> 0x63
  | DW_AT_object_pointer -> 0x64
  | DW_AT_endianity -> 0x65
  | DW_AT_elemental -> 0x66
  | DW_AT_pure -> 0x67
  | DW_AT_recursive -> 0x68
  | DW_AT_signature -> 0x69
  | DW_AT_main_subprogram -> 0x6a
  | DW_AT_data_bit_offset -> 0x6b
  | DW_AT_const_expr -> 0x6c
  | DW_AT_enum_class -> 0x6d
  | DW_AT_linkage_name -> 0x6e
