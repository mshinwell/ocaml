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

(* A "proto-DIE" contains similar information to a DWARF debugging
   information entry (DIE), but contains the actual attributes whose values
   are being specified, rather than linking to them via an abbrevation
   code.  The abbreviation codes are assigned later, when the proto-DIEs
   are turned into DIEs.
   Proto-DIEs may be formed into the required tree structures using a
   simple parenting relationship.  The complexities of flattening the
   structure to the DWARF representation (Section 2.3, DWARF-4 spec) are
   hidden. *)

type t

(* It is an error for [parent] to be [None] unless the [tag] is that for
   a compilation unit (which is a top-level entity). *)
val create
   : parent:t option
  -> tag:Tag.t
  -> attribute_values:Attribute_value.t list
  -> t

val create_ignore
   : parent:t option
  -> tag:Tag.t
  -> attribute_values:Attribute_value.t list
  -> unit

(* [reference t] returns a label that may be used when constructing other
    attribute values. *)
(* CR-someday mshinwell: ideally, attribute values could accept proto-DIE
   values directly, but there is a circularity. *)
val reference : t -> Linearize.label

(* [depth_first_fold] traverses a proto-DIE tree in a depth-first order
   convenient for DWARF information emission.  (Section 2.3, DWARF-4 spec.)
   [`End_of_siblings] indicates that a chain of siblings---that is to say,
   proto-DIEs with the same parent and at the same tree depth as each other---
   has finished.  This should correspond exactly to the points at which a
   "chain of sibling entries [must be] terminated by a null entry" specified
   in the DWARF-4 spec.
*)
val depth_first_fold
   : t
  -> init:'a
  -> f:('a
    -> [ `DIE of Tag.t * Child_determination.t * (Attribute_value.t list)
       | `End_of_siblings
       ]
    -> 'a)
  -> 'a
