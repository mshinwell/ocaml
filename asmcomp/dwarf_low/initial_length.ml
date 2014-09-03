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

(* Even on a 32-bit platform, a DWARF section may be larger than the
   maximum representable positive signed 32-bit integer... *)
type t = Int64.t

let create t = t

let size t =
  let size =
    match Dwarf_format.size () with
    | `Thirty_two -> 4
    | `Sixty_four -> 4 + 8
  in
  Int64.of_int size

let emit t ~emitter =
  match Dwarf_format.size () with
  | `Thirty_two ->
    (* Careful: not "Int64.of_int32 0xfffffff0l", which would sign
       extend. *)
    if Int64.compare t 0xfffffff0L >= 0 then begin
      failwith "Initial length value is too large for 32-bit DWARF"
    end;
    (* CR mshinwell: check this "to_int32" is correct if the size is
       large *)
    Value.emit (Value.as_four_byte_int (Int64.to_int32 t)) ~emitter
  | `Sixty_four ->
    Value.emit (Value.as_four_byte_int 0xffffffffl) ~emitter;
    Value.emit (Value.as_eight_byte_int t) ~emitter
