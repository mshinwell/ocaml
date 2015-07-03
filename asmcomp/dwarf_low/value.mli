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

(* Values encoded in DWARF forms. *)

type 'form t

include Emittable.S with type t := t

val total_size : t list -> Int64.t

val constant_true : Form.flag_present t
val constant_bool : bool -> Form.data1 t
val constant_one_byte_int : Int8.t -> Form.data1 t
val constant_two_byte_int : Int16.t -> Form.data2 t
val constant_four_byte_int : Int32.t -> Form.data4 t
val constant_eight_byte_int : Int64.t -> Form.data8 t

of_int8
of_int16
of_target_addr
of_uleb128

val indirect_string : string -> Form.strp t

val code_address_from_label : Linearize.label -> Form.address t
val code_address_from_symbol : symbol:string -> Form.address t

val offset_into_debug_line : Linearize.label -> Form.lineptr t
val offset_into_debug_info_from_label : Linearize.label -> Form.ref_addr t
val offset_into_debug_info_from_symbol : symbol:string -> Form.ref_addr t
val offset_into_debug_loc : Linearize.label -> Form.sec_offset t

val location_description : Single_location_description.t -> Form.exprloc t

val target_addr : Target_addr.t -> Form.xxx t



val as_reference_from_label : Linearize.label -> t
val as_absolute_offset : Int64.t -> t

(* CR mshinwell: consider changing [as_uleb128] interface *)
val as_uleb128_64 : Int64.t -> t
val as_uleb128 : int -> t
val as_leb128 : int -> t
val as_string : string -> t
val as_code_address_from_label_diff
   : [ `Symbol of string
     | `Symbol_plus_offset_in_bytes of string * int
     | `Label of Linearize.label ]
  -> [ `Symbol of string | `Label of Linearize.label ]
  -> t
val as_code_address_from_label_diff_minus_8
   : [ `Symbol of string | `Label of Linearize.label ]
  -> string (* symbol *)
  -> t

val as_code_address : Nativeint.t -> Form.addr t
