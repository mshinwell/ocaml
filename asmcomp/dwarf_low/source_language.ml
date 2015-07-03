(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Holding                                *)
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

type user = Int16.t

type t =
  | C89
  | C
  | Ada83
  | C_plus_plus
  | Cobol74
  | Cobol85
  | Fortran77
  | Fortran90
  | Pascal83
  | Modula2
  | Java
  | C99
  | Ada95
  | Fortran95
  | PLI
  | ObjC
  | ObjC_plus_plus
  | UPC
  | D
  | Python
  | User of user

let dw_lang_lo_user = Int16.of_int_exn 0x8000
let dw_lang_hi_user = Int16.of_int_exn 0xffff

let ocaml = C

let encode t =
  let code =
    match t with
    | C89 -> Int16.of_int_exn 0x1
    | C -> Int16.of_int_exn 0x2
    | Ada83 -> Int16.of_int_exn 0x3
    | C_plus_plus -> Int16.of_int_exn 0x4
    | Cobol74 -> Int16.of_int_exn 0x5
    | Cobol85 -> Int16.of_int_exn 0x6
    | Fortran77 -> Int16.of_int_exn 0x7
    | Fortran90 -> Int16.of_int_exn 0x8
    | Pascal83 -> Int16.of_int_exn 0x9
    | Modula2 -> Int16.of_int_exn 0xa
    | Java -> Int16.of_int_exn 0xb
    | C99 -> Int16.of_int_exn 0xc
    | Ada95 -> Int16.of_int_exn 0xd
    | Fortran95 -> Int16.of_int_exn 0xe
    | PLI -> Int16.of_int_exn 0xf
    | ObjC -> Int16.of_int_exn 0x10
    | ObjC_plus_plus -> Int16.of_int_exn 0x11
    | UPC -> Int16.of_int_exn 0x12
    | D -> Int16.of_int_exn 0x13
    | Python -> Int16.of_int_exn 0x14
    | User code ->
      assert (code >= dw_lang_lo_user && code <= dw_lang_hi_user);
      code
  in
  Value.constant_two_byte_int code

let parse ~stream =
  let open Stream.Monad_infix in
  Stream.read_int16 stream
  >>= function
  | 0x1 -> Ok C89
  | 0x2 -> Ok C
  | 0x3 -> Ok Ada83
  | 0x4 -> Ok C_plus_plus
  | 0x5 -> Ok Cobol74
  | 0x6 -> Ok Cobol85
  | 0x7 -> Ok Fortran77
  | 0x8 -> Ok Fortran90
  | 0x9 -> Ok Pascal83
  | 0xa -> Ok Modula2
  | 0xb -> Ok Java
  | 0xc -> Ok C99
  | 0xd -> Ok Ada95
  | 0xe -> Ok Fortran95
  | 0xf -> Ok PLI
  | 0x10 -> Ok ObjC
  | 0x11 -> Ok ObjC_plus_plus
  | 0x12 -> Ok UPC
  | 0x13 -> Ok D
  | 0x14 -> Ok Python
  | code ->
    if code >= dw_lang_lo_user && code <= dw_lang_hi_user then
      Ok (User code)
    else
      Error (Printf.sprintf "bad DWARF source language code %d" code)

let emit t ~emitter = Value.emit (encode t) ~emitter
let size t = Value.size (encode t)
