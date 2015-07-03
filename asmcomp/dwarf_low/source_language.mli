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

(** DWARF source languages (DWARF-4 standard section 7.12). *)

type user = private Int16.t

(** We omit the "DW_LANG_" prefix. *)
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

val ocaml : t

include Emittable.S with type t := t
include Parseable.S with type t := t
