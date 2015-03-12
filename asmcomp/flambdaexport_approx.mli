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

module Export_id : Ext_types.UnitId
   with module Compilation_unit := Compilation_unit

type tag = int

type _ boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type descr =
  | Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_string
  | Value_closure of descr_closure
  | Value_set_of_closures of descr_set_of_closures

and descr_closure = {
  closure_id : Closure_id.t;
  set_of_closures : descr_set_of_closures;
}

and descr_set_of_closures = {
  set_of_closures_id : Set_of_closures_id.t;
  (** Approximations to the variables bound by the set of closures. *)
  bound_var : approx Var_within_closure.Map.t;
  (** Approximations to the results of the functions bound by the set of
      closures. *)
  results : approx Closure_id.Map.t;
}

type approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t

val print_descr : Format.formatter -> descr -> unit
