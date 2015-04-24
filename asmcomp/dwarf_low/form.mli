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

(* DWARF attribute forms (DWARF-4 specification section 7.5.4, page 146).
   "The attribute form governs how the value of the attribute is encoded."
   Each form belongs to one or more "classes" (see class.mli).
*)

type t

include Emittable.S with type t := t

(* A value of [None] from [parse] indicates that a zero was read from the
   stream; this is likely to be the end of a sequence of attribute
   specifications (DWARF-4 specification section 7.5.3). *)
include Parseable.S with type t := t option
