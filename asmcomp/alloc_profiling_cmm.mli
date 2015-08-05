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

(** Insertion of instrumentation code for allocation profiling. *)

val code_for_function_prologue
   : num_instrumented_alloc_points:int
  -> num_direct_call_points:int
  -> Cmm.expression

val code_for_direct_call
   : node:Cmm.expression
  -> num_instrumented_alloc_points:int
  -> callee:string
  -> direct_call_point_index:int
  -> Cmm.expression

val instrument_fundecl : Cmm.fundecl -> Cmm.fundecl
