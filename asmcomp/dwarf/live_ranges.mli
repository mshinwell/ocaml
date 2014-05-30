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

(* Translate available registers information in a linearized function
   declaration into DWARF attributes.  The return value is the input
   function declaration augmented with labels necessary to identify
   the ranges over which named values are available. *)

open Dwarf_low_dot_std

module Many_live_ranges : sig
  type t

  val compare : t -> t -> int

  val starting_label : t -> start_of_function_label:string -> string

  val to_dwarf : t
    -> debug_loc_table:Dwarf_low.Debug_loc_table.t
    -> type_creator:(stamped_name:string -> string)
    -> start_of_function_label:string
    -> slot_offset_in_bytes:(
            reg_on_stack:Reg.t
         -> stack_offset:int
         -> int
       )
    -> Dwarf_low.Tag.t * Dwarf_low.Attribute_value.t list
         * Dwarf_low.Debug_loc_table.t

  val introduce_param : t -> bool
end

(* [process_fundecl fundecl] may modify [fundecl] in-place by inserting label
   declarations. *)
val process_fundecl : Linearize.fundecl
  -> Many_live_ranges.t list * Linearize.fundecl
