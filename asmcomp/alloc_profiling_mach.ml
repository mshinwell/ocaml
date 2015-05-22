(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Group, LLC                             *)
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

open Mach

let rec instrument_tails_and_returns insn =
  (* At every tail call point and return point, insert code to adjust the
     backtrace stack pointer appropriately. *)
  let needs_increment =
    match insn.desc with
    (* The backtrace stack pointer must be incremented when we return from
       the current function, or make a tail call to another function, so
       long as that tail call does not enter the current function again via
       the "tailrec entry point".  (See comment below.) *)
    | Ireturn -> true
    | Iop Itailcall_ind -> true
    | Iop (Itailcall_imm _) -> true
    (* The backtrace stack pointer does not need to be adjusted when tail
       calling the current function, since we will enter via the "tailrec
       entry point", after the backtrace bucket-calculating prologue. *)
    | Iop (Itailcall_imm lbl) when lbl = !current_function_name -> false
    | _ -> false
  in
  if needs_increment then
    instr_cons Iincrement_backtrace_stack [| |] [| |]
      (instrument_tails_and_returns insn.next)
  else
    insn

let fundecl decl =
  if not !Clflags.allocation_profiling then
    decl
  else
    instrument_tails_and_returns decl.fun_body
