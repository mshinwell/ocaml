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

open Std_internal

(* CR-someday mshinwell: if proper cross compilation support is
   implemented, change this as appropriate. *)
module Target_int = Nativeint

type 'form t =
  | True : Form.flag_present t
  | Bool : bool -> Form.data1 t
  | One_byte_int : Int8.t -> Form.data1 t
  | Two_byte_int : Int16.t -> Form.data2 t
  | Four_byte_int : Int32.t -> Form.data3 t
  | Eight_byte_int : Int64.t -> Form.data4 t
  | Indirect_string : string -> Form.strp t
  | Absolute_code_address : Target_int.t -> Form.addr t
  | Code_address_from_label : Linearize.label -> Form.addr t
  | Code_address_from_symbol : string -> Form.addr t
  | Offset_into_debug_line : Linearize.label -> Form.lineptr t
  | Offset_into_debug_info_from_label : Linearize.label -> Form.ref_addr t
  | Offset_into_debug_info_from_symbol : string -> Form.ref_addr t
  | Offset_into_debug_loc : Linearize.label -> Form.sec_offset t
  | Location_description : Single_location_description.t -> Form.exprloc t

(* DWARF-4 standard section 7.6. *)
let rec uleb128_size i =
  assert (i >= 0);
  if i < 128 then 1
  else 1 + (uleb128_size (i lsr 7))

let rec leb128_size i =
  if i >= -64 && i < 64 then 1
  else 1 + (leb128_size (i asr 7))

let size t =
  let size =
    match t with
    | True -> 0
    | Bool _ -> 1
    | One_byte_int _ -> 1
    | Two_byte_int _ -> 2
    | Four_byte_int _ -> 4
    | Eight_byte_int _ -> 8
    | Absolute_code_address _
    | Code_address_from_label _
    | Code_address_from_symbol _ -> Target_int.size
    | Indirect_string _
    | Offset_into_debug_line _
    | Offset_into_debug_info_from_label _
    | Offset_into_debug_info_from_symbol _
    | Offset_into_debug_loc _ ->
      begin match Dwarf_format.size () with
      | `Thirty_two -> 4
      | `Sixty_four -> 8
      end
    | Location_description loc_desc ->
      let loc_desc_size = Single_location_description.size loc_desc in
      (uleb128_size loc_desc_size) + loc_desc_size
  in
  Int64.of_int size

let emit t ~emitter =
  match t with
  | True -> ()
  | Bool b ->
    if b then Emitter.emit_byte emitter Int8.one
    else Emitter.emit_byte emitter Int8.zero
  | One_byte_int i -> Emitter.emit_byte emitter i
  | Two_byte_int i -> Emitter.emit_two_byte_int emitter i
  | Four_byte_int i -> Emitter.emit_four_byte_int emitter i
  | Eight_byte_int i -> Emitter.emit_eight_byte_int emitter i
  | Indirect_string s ->
    (* Strings are collected together into ".debug_str". *)
    let label = Emitter.cache_string emitter s in
    begin match Emitter.target emitter with
    | `Other ->
      Emitter.emit_offset_into_section emitter
        (`Label label) Section_names.debug_str
    | `MacOS_X -> assert false
    (*
      let count = !set_counter in
      let name = Printf.sprintf "Ldwarf_value%d" count in
      incr set_counter;
      Emitter.emit_string emitter name;
      Emitter.emit_string emitter " = ";
      Emitter.emit_label emitter label;
      Emitter.emit_string emitter "-";
      Emitter.emit_label emitter
        (Section_names.starting_label Section_names.debug_str);
      Emitter.emit_string emitter "\n";
      emit (Offset_from_var name) ~emitter
    *)
    end
  | Absolute_code_address addr -> Emitter.emit_native_int emitter addr
  | Code_address_from_label label ->
    Emitter.emit_code_address_from_label t label
  | Code_address_from_symbol symbol ->
    Emitter.emit_code_address_from_symbol t symbol
  | Offset_into_debug_line label ->
    Emitter.emit_offset_into_section emitter
      (`Label label) Section_names.debug_line
  | Offset_into_debug_info_from_label label ->
    Emitter.emit_offset_into_section emitter
      (`Label label) Section_names.debug_info
  | Offset_into_debug_info_from_symbol symbol ->
    Emitter.emit_offset_into_section emitter
      (`Symbol symbol) Section_names.debug_info
  | Offset_into_debug_loc label ->
    Emitter.emit_offset_into_section emitter
      (`Label label) Section_names.debug_loc
  | Location_description loc_desc ->
    let loc_desc_size = Single_location_description.size loc_desc in
    Emitter.emit_uleb128 emitter loc_desc_size;
    Single_location_description.emit loc_desc ~emitter



(*

type t =
  | Byte of int
  | Two_byte_int of int
  | Four_byte_int of Int32.t
  | Eight_byte_int of Int64.t
  | Uleb128 of int
  | Leb128 of int
  (* [Absolute_offset] is 32 bits wide when emitting 32-bit DWARF format
     and 64 bits when emitting 64-bit DWARF.  (Thus potentially a
     different size from the target's address width.)  We check during
     emission that the value is not too large. *)
  | Absolute_offset of Int64.t
  | Offset_from_label of Linearize.label * Section_names.t
  | Offset_from_symbol of string * Section_names.t
  | Offset_from_var of string
  | Reference_from_label of Linearize.label
  (* CR-someday mshinwell: this will need adjusting for cross-compilation
     support *)
  (* Absolute or computed code addresses cannot be wider than the target's
     address width, whether or not we are emitting 32-bit or 64-bit DWARF
     format. *)
  | Code_address of Nativeint.t
  | Code_address_from_symbol of string
  | Code_address_from_label of Linearize.label
  | Code_address_from_label_diff of
      [ `Label of Linearize.label | `Symbol of string
      | `Symbol_plus_offset_in_bytes of string * int ]
    * [ `Label of Linearize.label | `Symbol of string ]
  (* CR mshinwell: remove the following once we probably address CR in
     location_list_entry.ml (to do with boundary conditions on PC ranges). *)
  | Code_address_from_label_diff_minus_8 of
      [ `Label of Linearize.label | `Symbol of string ]
    * string
  | String of string

exception Too_large_for_two_byte_int of int
exception Too_large_for_byte of int

let as_four_byte_int i = Four_byte_int i
let as_eight_byte_int i = Eight_byte_int i

let as_two_byte_int i =
  if not (i >= 0 && i <= 0xffff) then
    raise (Too_large_for_two_byte_int i);
  Two_byte_int i

let as_byte i =
  if not (i >= 0 && i <= 0xff) then
    raise (Too_large_for_byte i);
  Byte i

let as_uleb128 i =
  assert (i >= 0);
  Uleb128 i

let as_uleb128_64 i =
  (* CR mshinwell: see mli *)
  let i = Int64.to_int i in
  assert (i >= 0);
  Uleb128 i


  | Absolute_offset o ->
    (* CR mshinwell: share with initial_length.ml *)
    if Int64.compare o 0xfffffff0L >= 0 then begin
      failwith "Absolute offset is too large for 32-bit DWARF"
    end;
    emit_directive_for_offset ~emitter;
    Emitter.emit_string emitter (sprintf "0x%Lx\n" o);
  | Offset_from_label (label, section) ->
  | Reference_from_label label ->
    emit_directive_for_offset ~emitter;
    Emitter.emit_label emitter label;
    Emitter.emit_string emitter "\n"
  | Code_address_from_label_diff (s2, s1) ->
    let count = !set_counter in
    let name = Printf.sprintf "Ldwarf_value%d" count in
    incr set_counter;
    begin match Emitter.target emitter with
    | `Other -> emit_directive_for_nativeint ~emitter
    | `MacOS_X ->
      Emitter.emit_string emitter name;
      Emitter.emit_string emitter " = "
    end;
    begin match s2 with
    | `Symbol s2 -> Emitter.emit_symbol emitter s2
    | `Symbol_plus_offset_in_bytes (s2, offset) ->
      Emitter.emit_symbol emitter s2;
      Emitter.emit_string emitter " + ";
      Emitter.emit_string emitter (Printf.sprintf "%d" offset)
    | `Label s2 -> Emitter.emit_label emitter s2
    end;
    begin match Emitter.target emitter with
    | `Other -> Emitter.emit_string emitter " - "
    | `MacOS_X -> Emitter.emit_string emitter "-"
    end;
    begin match s1 with
    | `Symbol s1 -> Emitter.emit_symbol emitter s1
    | `Label s1 -> Emitter.emit_label emitter s1
    end;
    Emitter.emit_string emitter "\n";
    begin match Emitter.target emitter with
    | `Other -> ()
    | `MacOS_X ->
      emit_as_native_int (`String name) ~emitter
    end
  | Code_address_from_label_diff_minus_8 (s2, s1) ->
    (* XXX fix this nonsense *)
    let count = !set_counter in
    let name = Printf.sprintf "Ldwarf_value%d" count in
    incr set_counter;
    begin match Emitter.target emitter with
    | `Other -> emit_directive_for_nativeint ~emitter
    | `MacOS_X ->
      Emitter.emit_string emitter name;
      Emitter.emit_string emitter " = "
    end;
    begin match s2 with
    | `Symbol s2 -> Emitter.emit_symbol emitter s2
    | `Label s2 -> Emitter.emit_label emitter s2
    end;
    begin match Emitter.target emitter with
    | `Other -> Emitter.emit_string emitter " - 1 - "
    | `MacOS_X -> Emitter.emit_string emitter "-1-"
    end;
    Emitter.emit_symbol emitter s1;
    Emitter.emit_string emitter "\n";
    begin match Emitter.target emitter with
    | `Other -> ()
    | `MacOS_X ->
      emit_as_native_int (`String name) ~emitter
    end
*)
