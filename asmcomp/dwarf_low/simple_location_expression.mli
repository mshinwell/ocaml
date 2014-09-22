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

(* CR-soon mshinwell: Shouldn't this module be called
   [Simple_location_description]?  (DWARF-4 spec 2.6.1) *)

type t

include Emittable.S with type t := t

(* The address of [symbol + offset_in_bytes] relative to the symbol [base]. *)
val at_offset_from_symbol
   : base:string
  -> symbol:string
  -> offset_in_bytes:int
  -> t

(* CR-soon mshinwell: consider a new type to identify whose register
   numbering is in use here *)
val in_register : reg_number:int -> t
val register_based_addressing : reg_number:int -> offset_in_bytes:int -> t
val frame_base_register : offset_in_bytes:int -> t
