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

open Std_internal

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

let as_leb128 i =
  Leb128 i

let as_string s =
  String s

let as_absolute_offset o = Absolute_offset o
let as_offset_from_label l ~section = Offset_from_label (l, section)
let as_reference_from_label l = Reference_from_label l

let as_code_address_from_symbol s =
  Code_address_from_symbol s

let as_code_address_from_label s =
  Code_address_from_label s

(* CR mshinwell: this mangling stuff is crap, and needs to be fixed *)
let as_code_address_from_label_diff s2 s1 =
  Code_address_from_label_diff (s2, s1)

let as_code_address_from_label_diff_minus_8 s2 s1 =
  Code_address_from_label_diff_minus_8 (s2, s1)

let as_code_address p =
  Code_address p

let size =
  (* DWARF-4 standard section 7.6. *)
  let rec uleb128_size i =
    assert (i >= 0);
    if i < 128 then 1
    else 1 + (uleb128_size (i lsr 7))
  in
  let rec leb128_size i =
    if i >= -64 && i < 64 then 1
    else 1 + (leb128_size (i asr 7))
  in
  fun t ->
    let size =
      match t with
      | Byte _ -> 1
      | Two_byte_int _ -> 2
      | Four_byte_int _ -> 4
      | Eight_byte_int _ -> 8
      | Uleb128 i -> uleb128_size i
      | Leb128 i -> leb128_size i
      | String _
        (* Strings are emitted as offsets into .debug_str.  The size of
           DW_FORM_strp depends on the DWARF format (DWARF-4 standard section
           7.4.3). *)
      | Absolute_offset _
      | Offset_from_label _
      | Offset_from_var _
      | Reference_from_label _ ->
        (* The size of offsets depends on the DWARF format being emitted, not
           on the target word size.  Ditto for "ref_addr" forms. *)
        begin match Dwarf_format.size () with
        | `Thirty_two -> 4
        | `Sixty_four -> 8
        end
      | Code_address _
      | Code_address_from_symbol _
      | Code_address_from_label _
      | Code_address_from_label_diff _
      | Code_address_from_label_diff_minus_8 _ -> Arch.size_addr
    in
    Int64.of_int size

let set_counter = ref 0

let emit_directive_for_offset ~emitter =
  match Dwarf_format.size () with
  | `Thirty_two -> Emitter.emit_string emitter "\t.long\t"
  | `Sixty_four -> Emitter.emit_string emitter "\t.quad\t"

let emit_directive_for_nativeint ~emitter =
  match Arch.size_addr with
  | 4 -> Emitter.emit_string emitter "\t.long\t"
  | 8 -> Emitter.emit_string emitter "\t.quad\t"
  | _ -> failwith "DWARF emitter does not understand Arch.size_addr's value"

let emit_as_native_int datum ~emitter =
  emit_directive_for_nativeint ~emitter;
  match datum with
  | `Native_int n ->
    Emitter.emit_string emitter (Printf.sprintf "%nd\n" n)
  | `Label label ->
    Emitter.emit_label emitter label;
    Emitter.emit_string emitter "\n"
  | `String str ->
    Emitter.emit_string emitter (Printf.sprintf "%s\n" str)
  | `Symbol symbol ->
    Emitter.emit_symbol emitter symbol;
    Emitter.emit_string emitter "\n"

let rec emit t ~emitter =
  match t with
  | Eight_byte_int i ->
    Emitter.emit_string emitter (sprintf "\t.quad\t0x%Lx\n" i);
  | Four_byte_int i ->
    Emitter.emit_string emitter (sprintf "\t.long\t0x%lx\n" i);
  | Two_byte_int i ->
    let directive =
      match Emitter.target emitter with
      | `Other -> "value"
      | `MacOS_X -> "short"
    in
    Emitter.emit_string emitter (sprintf "\t.%s\t0x%x\n" directive i)
  | Byte b ->
    Emitter.emit_string emitter (sprintf "\t.byte\t0x%x\n" b)
  | Uleb128 i ->
    Emitter.emit_string emitter (sprintf "\t.uleb128\t0x%x\n" i)
  | Leb128 i ->
    Emitter.emit_string emitter (sprintf "\t.sleb128\t%d\n" i)
  | Absolute_offset o ->
    (* CR mshinwell: share with initial_length.ml *)
    if Int64.compare o 0xfffffff0L >= 0 then begin
      failwith "Absolute offset is too large for 32-bit DWARF"
    end;
    emit_directive_for_offset ~emitter;
    Emitter.emit_string emitter (sprintf "0x%Lx\n" o);
  | Offset_from_label (label, section) ->
    begin match Emitter.target emitter with
    | `Other ->
      emit_directive_for_offset ~emitter;
      Emitter.emit_label emitter label;
      Emitter.emit_string emitter "\n"
    | `MacOS_X ->
      let count = !set_counter in
      let name = Printf.sprintf "Ldwarf_value%d" count in
      incr set_counter;
      Emitter.emit_string emitter name;
      Emitter.emit_string emitter " = ";
      Emitter.emit_label emitter label;
      Emitter.emit_string emitter "-";
      Emitter.emit_label emitter (Section_names.starting_label section);
      Emitter.emit_string emitter "\n";
      emit (Offset_from_var name) ~emitter
    end
  | Offset_from_var var ->
    emit_directive_for_offset ~emitter;
    Emitter.emit_string emitter var;
    Emitter.emit_string emitter "\n"
  | String s ->
    (* Strings are collected together into ".debug_str". *)
    let label = Emitter.cache_string emitter s in
    begin match Emitter.target emitter with
    | `Other ->
      emit (Offset_from_label (label, Section_names.debug_str)) ~emitter
    | `MacOS_X ->
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
    end
  | Reference_from_label label ->
    emit_directive_for_offset ~emitter;
    Emitter.emit_label emitter label;
    Emitter.emit_string emitter "\n"
  | Code_address p ->
    emit_as_native_int (`Native_int p) ~emitter
  | Code_address_from_symbol sym ->
    emit_as_native_int (`Symbol sym) ~emitter
  | Code_address_from_label label ->
    emit_as_native_int (`Label label) ~emitter
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
