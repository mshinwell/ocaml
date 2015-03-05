(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Pierre Chambart and Mark Shinwell                   *)
(*                                                                     *)
(*  Copyright 2014--2015, OCamlPro                                     *)
(*  Copyright 2015, Jane Street Group                                  *)
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

(* Tracking of export information collated from two sources:
   1. the current compilation unit;
   2. units imported by the current compilation unit.
*)

open Abstract_identifiers

type t

val create : unit -> t

val get_fun_offset : t -> Closure_id.t -> int
val get_fv_offset : t -> Variable_in_closure_id.t -> int

val is_function_constant : t -> Closure_id.t -> bool
val is_function_local_and_constant : t -> Closure_id.t -> bool

val function_arity : t -> Closure_id.t -> int
