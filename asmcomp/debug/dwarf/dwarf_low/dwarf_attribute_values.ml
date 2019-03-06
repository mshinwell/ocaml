(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module A = Asm_directives

module Value = struct
  type internal_t =
    | Dwarf_value of Dwarf_value.t
    (* Hack to break circular dependency. *)
    | Single_location_description of Single_location_description.t
    | Composite_location_description of Composite_location_description.t

  type _ t = internal_t

  module V = Dwarf_value

  let flag_true = Dwarf_value V.Flag_true
  let bool b = Dwarf_value (V.Bool b)
  let int8 i = Dwarf_value (V.Int8 i)
  let int16 i = Dwarf_value (V.Int16 i)
  let int32 i = Dwarf_value (V.Int32 i)
  let int64 i = Dwarf_value (V.Int64 i)
  let string s = Dwarf_value (V.String s)
  let indirect_string ptr = Dwarf_value (V.Indirect_string ptr)
  let code_address_from_label lbl =
    Dwarf_value (V.Code_address_from_label lbl)
  let code_address_from_symbol sym =
    Dwarf_value (V.Code_address_from_symbol sym)
  let offset_into_debug_line lbl = Dwarf_value (V.Offset_into_debug_line lbl)
  let offset_into_debug_line_from_symbol sym =
    Dwarf_value (V.Offset_into_debug_line_from_symbol sym)
  let offset_into_debug_info lbl =
    Dwarf_value (V.Offset_into_debug_info lbl)
  let offset_into_debug_info_from_symbol sym =
    Dwarf_value (V.Offset_into_debug_info_from_symbol sym)
  let offset_into_debug_loc lbl = Dwarf_value (V.Offset_into_debug_loc lbl)
  let single_location_description sld = Single_location_description sld
  let composite_location_description sld = Composite_location_description sld
  let encoding_attribute attr =
    Dwarf_value (Encoding_attribute.as_dwarf_value attr)

  let symbol_32 sym =
    Dwarf_value (V.Code_address_from_symbol sym)

  let symbol_64 sym =
    Dwarf_value (V.Code_address_from_symbol sym)
end

module Attribute_value = struct
  type t =
    Dwarf_attributes.Attribute_specification.Sealed.t * Value.internal_t

  let create attr_spec value =
    let attr_spec = Dwarf_attributes.Attribute_specification.seal attr_spec in
    attr_spec, value

  (* CR mshinwell: as per CR above.  This shouldn't be here *)
  let rec uleb128_size i =
    assert (Int64.compare i 0L >= 0);
    if Int64.compare i 128L < 0 then Dwarf_int.one ()
    else Dwarf_int.succ (uleb128_size (Int64.shift_right_logical i 7))

  let size ((_spec, value) : t) =
    match value with
    | Dwarf_value value -> Dwarf_value.size value
    | Single_location_description loc_desc ->
      let loc_desc_size = Single_location_description.size loc_desc in
      Dwarf_int.add (uleb128_size (Dwarf_int.to_int64 loc_desc_size))
        loc_desc_size
    | Composite_location_description loc_desc ->
      let loc_desc_size = Composite_location_description.size loc_desc in
      Dwarf_int.add (uleb128_size (Dwarf_int.to_int64 loc_desc_size))
        loc_desc_size

  let emit ((_spec, value) : t) =
    match value with
    | Dwarf_value value -> Dwarf_value.emit value
    | Single_location_description loc_desc ->
      A.uleb128 (Dwarf_int.to_int64 (
        Single_location_description.size loc_desc));
      Single_location_description.emit loc_desc
    | Composite_location_description loc_desc ->
      A.uleb128 (Dwarf_int.to_int64 (
        Composite_location_description.size loc_desc));
      Composite_location_description.emit loc_desc

  let attribute_spec (spec, _value) = spec
end
