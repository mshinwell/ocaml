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

(* DWARF source languages (DWARF-4 standard section 7.12). *)

type t =
  | DW_LANG_C89
  | DW_LANG_C
  | DW_LANG_Ada83
  | DW_LANG_C_plus_plus
  | DW_LANG_Cobol74
  | DW_LANG_Cobol85
  | DW_LANG_Fortran77
  | DW_LANG_Fortran90
  | DW_LANG_Pascal83
  | DW_LANG_Modula2
  | DW_LANG_Java
  | DW_LANG_C99
  | DW_LANG_Ada95
  | DW_LANG_Fortran95
  | DW_LANG_PLI
  | DW_LANG_ObjC
  | DW_LANG_ObjC_plus_plus
  | DW_LANG_UPC
  | DW_LANG_D
  | DW_LANG_Python
  | User of Int16.t

let dw_lang_lo_user = Int16.of_int_exn 0x8000
let dw_lang_hi_user = Int16.of_int_exn 0xffff

let encode t = function
  | DW_LANG_C89 -> Int16.of_int_exn 0001
  | DW_LANG_C -> Int16.of_int_exn 0002
  | DW_LANG_Ada83 -> Int16.of_int_exn 0003
  | DW_LANG_C_plus_plus -> Int16.of_int_exn 0004
  | DW_LANG_Cobol74 -> Int16.of_int_exn 0005
  | DW_LANG_Cobol85 -> Int16.of_int_exn 0006
  | DW_LANG_Fortran77 -> Int16.of_int_exn 0007
  | DW_LANG_Fortran90 -> Int16.of_int_exn 0008
  | DW_LANG_Pascal83 -> Int16.of_int_exn 0009
  | DW_LANG_Modula2 -> Int16.of_int_exn 000a
  | DW_LANG_Java -> Int16.of_int_exn 000b
  | DW_LANG_C99 -> Int16.of_int_exn 000c
  | DW_LANG_Ada95 -> Int16.of_int_exn 000d
  | DW_LANG_Fortran95 -> Int16.of_int_exn 000e
  | DW_LANG_PLI -> Int16.of_int_exn 000f
  | DW_LANG_ObjC -> Int16.of_int_exn 0010
  | DW_LANG_ObjC_plus_plus -> Int16.of_int_exn 0011
  | DW_LANG_UPC -> Int16.of_int_exn 0012
  | DW_LANG_D -> Int16.of_int_exn 0013
  | DW_LANG_Python -> Int16.of_int_exn 0014
  | User code -> code
