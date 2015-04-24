(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2014, Jane Street Holding                          *)
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

type t

(* keep a list of the attributes, and then a function identifying which ones have
   arguments that need patching up when they are read. *)

module Variant : sig
  type t =
    | Low_pc
    | High_pc
    | Producer
    | Name
    | Comp_dir
    | Stmt_list
    | Extern'l
    | Location_using_single_location_description
    | Location_using_location_list
    | Typ'
    | Encoding
    | Byte_size
    | Linkage_name
    | Sibling
    | Import
    | Specification
end
val to_variant : t -> Variant.t

include Emittable.S with type t := t

(* A value of [None] from [parse] indicates that a zero was read from the
   stream; this is likely to be the end of a sequence of attribute
   specifications (DWARF-4 specification section 7.5.3). *)
include Parseable.S with type t := t option

val compare : t -> t -> int
