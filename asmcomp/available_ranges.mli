(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                  Mark Shinwell, Jane Street Europe                  *)
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

(* Given a value identifier [x] and a function, an "available subrange" is
   a contiguous subset of that function's code paired with a register [r],
   such that at all times during the block's execution the value of [x] is
   available in [r].

   Say that an "available range" is a set of available subranges that do not
   overlap in code space, again for a single identifier and function.

   This module calculates, for a single function, available ranges for all
   value identifiers for which we have availability information (which in
   turn comes from an earlier pass---see available_regs.ml).  It also
   mutates the linearized code to insert labels to delimit such ranges.
*)

module Available_subrange : sig
  type t

  val start_pos : t -> [ `Start_of_function | `At_label of Linearize.label ]
  val end_pos : t -> Linearize.label
  val reg : t -> Reg.t
end

module Available_range : sig
  type t
  
  val is_parameter : t -> bool
  val extremities
     : t
    -> [ `Start_of_function | `At_label of Linearize.label ] * Linearize.label

  val fold
     : t
    -> init:'a
    -> f:('a -> available_subrange:Available_subrange.t -> 'a)
    -> 'a
end

type t

val create : fundecl:Linearize.fundecl -> t

val function_name : t -> string

val fold
   : t
  -> init:'a
  (* [is_unique] is [true] if there is no other value identifier with the
     same (unstamped) name as [ident] in [t].  (It follows that using the
     unstamped name is sufficient to uniquely identify the identifier amongst
     a list of, say, local variables in a debugger.) *)
  -> f:('a -> ident:Ident.t -> is_unique:bool -> range:Available_range.t -> 'a)
  -> 'a
