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

type t = {
  subst : (unit Flambda.t * Clambda.ulambda) Variable.Map.t;
  (* XXX merge the next two *)
  variable_symbol_equalities : Symbol.t Variable.Map.t;
  constants : (Symbol.t * Ident.t) Variable.Map.t;
  approx : approx Variable.Map.t;
}


let create () =
  {

  }

let add_approximations t vars_to_approxs =

let find_variable_symbol_equality t var =
  Variable.Map.find var t.variable_symbol_equalities

let variable_has_symbol_equality t var =
  match find_variable_symbol_equality t var with
  | None -> false
  | Some _ -> true

let add_variable_symbol_equalities t vars_to_vars =
  Variable.Map.filter_mapi (fun arg specialised_to ->
      find_variable_symbol_equality t specialised_to)
    vars_to_vars
