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

(* Deduce DWARF "lexical block" bounds from available range information.
   - Parameters are in the scope of a function, not a lexical block.
   - Lexical blocks do not exactly follow the OCaml source scoping: they
     start (resp. finish) when the register holding the value of a given
     identifier first becomes available (resp. stops being available).
     (This can in fact be a useful property; in particular, it may enable
     the value of an identifier to be examined in the debugger even though
     it's just gone out of scope.)
*)

module Lexical_block : sig
  type t

  val start_pos : t -> Linearize.label
  val end_pos : t -> Linearize.label
end

type t

val create : available_ranges:Available_ranges.t -> t

val fold
   : t
  -> init:'a
  -> f:('a
    -> ident:Ident.t
    -> is_unique:bool  (* see available_ranges.mli *)
    -> scope:[ `Block_scope of Lexical_block.t | `Function_scope ]
    -> range:Available_range.t
    -> 'a)
  -> 'a
