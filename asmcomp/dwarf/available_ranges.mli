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
   a contiguous code block where for every instruction in the block, we know
   (via the availability information calculated by available_regs.ml) in which
   register [x] may be found.

   Say that an "available range" is a set of non-overlapping available
   subranges, again for a single identifier and function.

   This module calculates, for a single function, available ranges for all
   value identifiers for which we have availability information.  It also
   mutates the linearized code to insert labels to delimit such ranges.
*)

type t

val create : fundecl:Linearize.fundecl -> t

val fold
   : t
  -> init:'a
  (* [is_unique] is [true] if there is no other value identifier with the
     same (unstamped) name as [ident] in [t].  (It follows that using the
     unstamped name is sufficient to uniquely identify the identifier amongst
     a list of, say, local variables in a debugger.) *)
  -> f:('a -> ident:Ident.t -> is_unique:bool -> range:Available_range.t -> 'a)
  -> 'a
