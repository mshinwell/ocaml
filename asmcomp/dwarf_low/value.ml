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
  | Four_byte_int of int
  | Four_byte_int_from_label of Linearize.label
  | Two_byte_int of int
  | Byte of int
  | Uleb128 of int
  | Leb128 of int
  | String of string
  | Code_address_from_symbol of string
  | Code_address_from_label of Linearize.label
  | Code_address_from_label_diff of
      [ `Label of Linearize.label | `Symbol of string ]
    * [ `Label of Linearize.label | `Symbol of string ]
  (* CR mshinwell: remove the following once we probably address CR in
     location_list_entry.ml (to do with boundary conditions on PC ranges). *)
  | Code_address_from_label_diff_minus_8 of
      [ `Label of Linearize.label | `Symbol of string ]
    * string
  | Code_address of Int64.t

exception Too_large_for_four_byte_int of int
exception Too_large_for_two_byte_int of int
exception Too_large_for_byte of int

let as_four_byte_int i =
  if not (i >= 0 && i <= 0xffff_ffff) then
    raise (Too_large_for_four_byte_int i);
  Four_byte_int i

let as_four_byte_int_from_label l =
  Four_byte_int_from_label l

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

exception Negative_leb128_not_yet_thought_about
let as_leb128 i =
  if i < 0 then raise Negative_leb128_not_yet_thought_about;
  Leb128 i

let as_string s =
  String s

let as_code_address_from_symbol s =
  Code_address_from_symbol s

let as_code_address_from_label s =
  Code_address_from_label s

(* CR mshinwell: this mangling stuff is crap, and needs to be fixed *)
let as_code_address_from_label_diff s2 s1 =
  Code_address_from_label_diff (s2, s1)

let as_code_address_from_label_diff_minus_8 s2 s1 =
  Code_address_from_label_diff_minus_8 (s2, s1)

let as_code_address i =
  Code_address i

let size = function
  | Four_byte_int _ | Four_byte_int_from_label _ -> 4
  | Two_byte_int _ -> 2
  | Byte _ -> 1
  | Uleb128 i | Leb128 i ->
    if i = 0 then 1
    else 1 + int_of_float (floor (log (float_of_int i) /. log 128.))
  | String s -> 1 + String.length s
  | Code_address_from_symbol _ | Code_address_from_label _ | Code_address _
  | Code_address_from_label_diff _
  | Code_address_from_label_diff_minus_8 _ -> 8

(* CR mshinwell: the assembler directives need to be target-specific *)
let emit t ~emitter =
  match t with
  | Four_byte_int i ->
    Emitter.emit_string emitter (sprintf "\t.long\t0x%x\n" i);
  | Four_byte_int_from_label l ->
    Emitter.emit_string emitter "\t.long\t";
    Emitter.emit_label emitter l;
    Emitter.emit_string emitter "\n"
  | Two_byte_int i ->
    Emitter.emit_string emitter (sprintf "\t.value\t0x%x\n" i)
  | Byte b ->
    Emitter.emit_string emitter (sprintf "\t.byte\t0x%x\n" b)
  | Uleb128 i ->
    Emitter.emit_string emitter (sprintf "\t.uleb128\t0x%x\n" i)
  | Leb128 i ->
    Emitter.emit_string emitter (sprintf "\t.sleb128\t0x%x\n" i)
  | String s ->
    Emitter.emit_string emitter (sprintf "\t.string\t\"%s\"\n" s)
  | Code_address_from_symbol s ->
    Emitter.emit_string emitter "\t.quad\t";
    Emitter.emit_symbol emitter s;
    Emitter.emit_string emitter "\n"
  | Code_address_from_label s ->
    Emitter.emit_string emitter "\t.quad\t";
    Emitter.emit_label emitter s;
    Emitter.emit_string emitter "\n"
  | Code_address_from_label_diff (s2, s1) ->
    Emitter.emit_string emitter "\t.quad\t";
    begin match s2 with
    | `Symbol s2 -> Emitter.emit_string emitter s2
    | `Label s2 -> Emitter.emit_label emitter s2
    end;
    Emitter.emit_string emitter " - ";
    begin match s1 with
    | `Symbol s1 -> Emitter.emit_string emitter s1
    | `Label s1 -> Emitter.emit_label emitter s1
    end;
    Emitter.emit_string emitter "\n"
  | Code_address_from_label_diff_minus_8 (s2, s1) ->
    Emitter.emit_string emitter "\t.quad\t";
    begin match s2 with
    | `Symbol s2 -> Emitter.emit_string emitter s2
    | `Label s2 -> Emitter.emit_label emitter s2
    end;
    Emitter.emit_string emitter " - 1 - "; (* CR mshinwell: !!! *)
    Emitter.emit_string emitter s1;
    Emitter.emit_string emitter "\n"
  | Code_address i ->
    Emitter.emit_string emitter (sprintf "\t.quad\t0x%Lx\n" i)
