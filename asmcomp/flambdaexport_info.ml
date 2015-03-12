(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Pierre Chambart and Mark Shinwell                   *)
(*                                                                     *)
(*  Copyright 2013--2015, OCamlPro                                     *)
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

type t = {
  functions : unit Flambda.function_declarations Set_of_closures_id.Map.t;
  (** Code of exported functions indexed by function identifier *)
  functions_off : unit Flambda.function_declarations Closure_id.Map.t;
  (** Code of exported functions indexed by offset identifier *)
  ex_values : descr Export_id.Map.t;
  (** Structure of exported values *)
  globals : approx Ident.Map.t;
  (** Global variables provided by the unit: usually only the top-level
      module identifier, but there may be multiple identifiers in the case
      of packs. *)
  id_symbol : Symbol.t Export_id.Map.t;
  symbol_id : Export_id.t Symbol.Map.t;
  (** Associates symbols and values *)
  offset_fun : int Closure_id.Map.t;
  (** Positions of function pointers in their closures *)
  (* CR mshinwell: update comments, just pasted in from elsewhere *)
  (* [fun_offset_table] associates a function label to its offset inside
     a closure.  One table suffices, since the identifiers used as keys
     are globally unique. *)
  (* [fv_offset_table] is like [fun_offset_table], but for free variables. *)
  offset_fv : int Var_within_closure.Map.t;
  (** Positions of value pointers in their closures *)
  constants : Symbol.Set.t;
  (** Symbols that are effectively constants (the top-level module is not
      always a constant for instance) *)
  constant_closures : Set_of_closures_id.Set.t;
  kept_arguments : Variable.Set.t Set_of_closures_id.Map.t;
}

let empty : t = {
  functions = Set_of_closures_id.Map.empty;
  functions_off = Closure_id.Map.empty;
  ex_values =  Compilation_unit.Map.empty;
  globals = Ident.Map.empty;
  id_symbol =  Compilation_unit.Map.empty;
  symbol_id = Symbol.Map.empty;
  offset_fun = Closure_id.Map.empty;
  offset_fv = Var_within_closure.Map.empty;
  constants = Symbol.Set.empty;
  constant_closures = Set_of_closures_id.Set.empty;
  kept_arguments = Set_of_closures_id.Map.empty;
}

let find_declaration t closure_id =
  Closure_id.Map.find closure_id t.functions_off

let get_fun_offset_exn t closure_id =
  Closure_id.Map.find closure_id t.offset_fun

let get_fv_offset_exn t var =
  Var_within_closure.Map.find var t.offset_fv

let closure_declaration_position_exn t closure_id =
  Closure_id.Map.find closure_id t.functions_off

let set_of_closures_declaration_position_exn t set_of_closures_id =
  Set_of_closures_id.Map.find set_of_closures_id t.functions

let is_set_of_closures_constant t set_of_closures_id =
  Set_of_closures_id.Set.mem set_of_closures_id t.constant_closures
