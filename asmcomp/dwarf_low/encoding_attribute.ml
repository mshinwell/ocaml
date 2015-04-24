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

(* We omit the "DW_ATE_" prefix. *)
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
  | User of Int8.t

let lo_user = 0x80
let hi_user = 0xff

let encode t =
  let code =
    | Signed -> 0x05
    | Address -> 0x01
    | Boolean -> 0x02
    | Complex_float -> 0x03
    | Float -> 0x04
    | Signed -> 0x05
    | Signed_char -> 0x06
    | Unsigned -> 0x07
    | Unsigned_char -> 0x08
    | Imaginary_float -> 0x09
    | Packed_decimal -> 0x0a
    | Numeric_string -> 0x0b
    | Edited -> 0x0c
    | Signed_fixed -> 0x0d
    | Unsigned_fixed -> 0x0e
    | Decimal_float -> 0x0f
    | UTF -> 0x10
    | User code ->
      assert (code >= lo_user && code <= hi_user);
      code
  in
  Value.constant_one_byte_int code

let parse ~stream =
  Stream.read_byte stream
  >>= function
  | 0x05 -> Ok Signed
  | 0x01 -> Ok Address
  | 0x02 -> Ok Boolean
  | 0x03 -> Ok Complex_float
  | 0x04 -> Ok Float
  | 0x05 -> Ok Signed
  | 0x06 -> Ok Signed_char
  | 0x07 -> Ok Unsigned
  | 0x08 -> Ok Unsigned_char
  | 0x09 -> Ok Imaginary_float
  | 0x0a -> Ok Packed_decimal
  | 0x0b -> Ok Numeric_string
  | 0x0c -> Ok Edited
  | 0x0d -> Ok Signed_fixed
  | 0x0e -> Ok Unsigned_fixed
  | 0x0f -> Ok Decimal_float
  | 0x10 -> Ok UTF
  | code when code >= lo_user && code <= hi_user -> Ok (User code)
  | code ->
    Error (Printf.sprintf "unknown DWARF encoding attribute code 0x%x" code)

let emit t ~emitter = Value.emit (encode t) ~emitter
let size t = Value.size (encode t)
