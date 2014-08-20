(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
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

type t

include Emittable.S with type t := t

val as_four_byte_int : int -> t
val as_four_byte_int_from_label : string -> t
val as_two_byte_int : int -> t
val as_byte : int -> t
val as_uleb128 : int -> t
val as_leb128 : int -> t
val as_string : string -> t
val as_code_address_from_symbol : string -> t
val as_code_address_from_label : Linearize.label -> t
val as_code_address_from_label_diff
   : [ `Symbol of string | `Label of Linearize.label ]
  -> [ `Symbol of string | `Label of Linearize.label ]
  -> t
val as_code_address_from_label_diff_minus_8
  : [ `Symbol of string | `Label of Linearize.label ] -> string (* symbol *) -> t
val as_code_address : Int64.t -> t
