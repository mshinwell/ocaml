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

(** DWARF base type attribute encodings (DWARF-4 specification section 7.8). *)

type user = private Int8.t

(** We omit the "DW_ATE_" prefix. *)
type t =
  | Address
  | Boolean
  | Complex_float
  | Float
  | Signed
  | Signed_char
  | Unsigned
  | Unsigned_char
  | Imaginary_float
  | Packed_decimal
  | Numeric_string
  | Edited
  | Signed_fixed
  | Unsigned_fixed
  | Decimal_float
  | UTF
  | User of user

include Emittable with type t := t
