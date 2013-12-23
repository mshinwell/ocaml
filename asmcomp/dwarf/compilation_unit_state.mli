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

open Dwarf_low_dot_std

(* A value of type [t] holds all state necessary to emit DWARF
   debugging information for a single compilation unit. *)
type t

val create : emitter:Dwarf_low.Emitter.t
  -> source_file_path:string option
  -> start_of_code_label:string
  -> end_of_code_label:string
  -> t

val emit_debugging_info_prologue : t -> unit
val emit_debugging_info_epilogue : t -> unit

module Function : sig
  type t
end

module Reg_location : sig
  type t

  val hard_register : reg_num:int -> t
  val stack : unit -> t
end

val start_function : t
  -> linearized_fundecl:Linearize.fundecl
  -> slot_offset_in_bytes:(
          reg_on_stack:Reg.t
       -> stack_offset:int
       -> int
     )
  -> Function.t * Linearize.fundecl

val end_function : t -> Function.t -> unit
