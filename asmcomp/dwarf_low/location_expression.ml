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

module SLE = Simple_location_expression

type t =
  | Immediate of SLE.t  (* will do for the moment *)
  | Computed of (unit -> SLE.t)

let in_register ~reg_number =
  let sle =
    (* CR mshinwell: this remapping needs to be somewhere else *)
    (* CR mshinwell: this is x86-64 specific; will not do *)
    let reg_number =
      match reg_number with
      | 0 -> 0     (* %rax *)
      | 1 -> 3     (* %rbx *)
      | 2 -> 5     (* %rdi *)
      | 3 -> 4     (* %rsi *)
      | 4 -> 1     (* %rdx *)
      | 5 -> 2     (* %rcx *)
      | 6 -> 8     (* %r8 *)
      | 7 -> 9     (* %r9 *)
      | 8 -> 12    (* %r12 *)
      | 9 -> 13    (* %r13 *)
      | 10 -> 6    (* %rbp *)
      | 11 -> 10   (* %r10 *)
      | 12 -> 11   (* %r11 *)
      | r -> r     (* CR mshinwell: fixme *)
    in
    SLE.in_register ~reg_number
  in
  Immediate sle

let at_offset_from_stack_pointer ~offset_in_bytes =
  let sle =
    SLE.register_based_addressing ~reg_number:7 (* %rsp *)
      ~offset_in_bytes
  in
  Immediate sle

let at_computed_offset_from_stack_pointer ~offset_in_bytes =
  let sle () =
    SLE.register_based_addressing ~reg_number:7 (* %rsp *)
      ~offset_in_bytes:(offset_in_bytes ())
  in
  Computed sle

let sle t =
  match t with
  | Immediate sle -> sle
  | Computed sle -> sle ()

let size t =
  SLE.size (sle t)

let emit t ~emitter =
  SLE.emit (sle t) ~emitter
