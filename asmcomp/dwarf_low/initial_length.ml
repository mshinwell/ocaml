(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2014, Jane Street Holding                                *)
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

(* DWARF-4 standard section 7.4. *)

type t = Nativeint.t

let create t = t

let size t =
  match Dwarf_format.size () with
  | `Thirty_two -> 4
  | `Sixty_four -> 4 + 8

let emit t ~emitter =
  match Dwarf_format.size () with
  | `Thirty_two ->
    Value.emit (Value.as_native_int t) ~emitter
  | `Sixty_four ->
    Value.emit (Value.as_four_byte_int Int32.minus_one) ~emitter;
    Value.emit (Value.as_native_int t) ~emitter
