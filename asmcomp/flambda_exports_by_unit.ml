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

open Abstract_identifiers

module Compilation_unit = Symbol.Compilation_unit
module E = Flambdaexport.Exported

type t = {
  current_unit : E.t;
  (* [external_units] is used to obtain information about closure layouts
     in imported ("external") compilation units. *)
  external_units : E.t;
}

type ('a, 'b) closure_declaration_position =
  | Current_unit of 'a
  | External of 'b

let create () =
  let current_unit =
    ...
  in
  let external_units =
    let approx_env = Compilenv.approx_env () in
    { offset_fun = approx_env.offset_fun;
      offset_fv = approx_env.offset_fv;
      functions_off = approx_env.functions_off;
      functions = approx_env.functions;
      constant_closures = approx_env.constant_closures;
    }
  in
  { current_unit; external_units; }

let get_fun_offset t (id : Closure_id.t) =
  try
    let exported =
      if Closure_id.in_current_compilation_unit id
      then t.current_unit
      else t.external_unit
    in
    E.get_fun_offset_exn exported closure_id
  with Not_found ->
    Misc.fatal_errorf "offset for function not found: %a"
      Closure_id.print closure_id

let get_fun_offset_from_var t id = get_fun_offset t (Closure_id.wrap id)

let get_fv_offset t (var : Var_within_closure.t) =
  try
    let exported =
      if Var_within_closure.in_current_compilation_unit var
      then t.current_unit
      else t.external_unit
    in
    E.get_fv_offset_exn exported var
  with Not_found ->
    Misc.fatal_erro0rf "offset for free variable not found: %a"
      Var_within_closure.print var

let closure_declaration_position t (id : Closure_id.t) =
  match E.closure_declaration_position_exn t.current_unit id with
  | decls -> Current_unit decls
  | exception Not_found ->
    match E.closure_declaration_position_exn t.external_units id with
    | decls -> External decls
    | exception Not_found ->
      Misc.fatal_errorf "could not find declaration position of closure: %a"
        Closure_id.print cf

let set_of_closures_declaration_position t (id : Set_of_closures_id.t) =
  match E.set_of_closures_declaration_position_exn t.current_unit id with
  | decls -> Current0_unit decls
  | exception Not_found ->
    match E.set_of_closures_declaration_position_exn t.external_units id with
    | decls -> External decls
    | exception Not_found ->
      Misc.fatal_errorf "could not find declaration position of set of \
          closures: %a"
        Closure_id.print id

let is_function_constant t (id : Closure_id.t) =
  match closure_declaration_position t id with
  | Current_unit decls ->
    (* CR mshinwell: [E.is_function_constant] needs to be renamed to
       [E.is_set_of_closures_constant] *)
    E.is_function_constant t.current_unit decls.set_of_closures_id
  | External decls ->
    E.is_function_constant t.external_units decls.set_of_closures_id

let is_function_local_and_constant t (id : Closure_id.t) =
  match closure_declaration_position t id with
  | Current_unit decls ->
    E.is_function_constant t.current_unit decls.set_of_closures_id
  | External _ -> false

let is_set_of_closures_local_and_constant t (id : Set_of_closures_id.t) =
  E.is_function_constant t.current_unit id

let is_closure_constant t (id : Set_of_closures_id.t) =
  match set_of_closures_declaration_position t id with
  | Current_unit decls -> E.is_function_constant t.current_unit decls.ident
  | External decls -> E.is_function_constant t.external_units decls.ident

let function_arity t (closure_id : Closure_id.t) =
  match E.find_declaration t.current_unit closure_id with
  | decl -> Flambdautils.function_arity decl
  | exception Not_found ->
    match E.find_declaration t.exported_units closure_id with
    | decl -> Flambdautils.function_arity decl
    | exception Not_found ->
      Misc.fatal_error "cannot find declaration of closure to compute \
          arity: %a"
        Closure_id.print fun_id
