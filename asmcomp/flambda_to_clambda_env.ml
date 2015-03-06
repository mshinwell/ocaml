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
  (* CR mshinwell: [constants] might be misleading, since [subst] can
     map constants too (but not ones that have symbols associated
     with them). *)
  constants : (Symbol.t * Ident.t) Variable.Map.t;
  approx : approx Variable.Map.t;
  constant_closures : ...;
}


let create () =
  {

  }

let clean t = { t with subst = Variable.Map.empty; }

(* XXX this is needed in the [Flet] case in flambda_to_clambda *)
type constant_classification =
  | Constant_accessed_via_symbol of Symbol.t
  | Constant_not_accessed_via_symbol
  | Constant_closure
  | Not_constant

let classify_constant t (expr : unit Flambda.t) : constant_classification =
  match expr with
  | Fsymbol (sym, ()) -> Constant_accessed_via_symbol sym
  | Fconst (_, ()) -> Constant_not_accessed_via_symbol
  | Fset_of_closures ({ cl_fun }, _) ->
      if Set_of_closures_id.Set.mem cl_fun.ident t.constant_closures
      then Constant_closure
      else Not_constant
  | _ -> Not_constant

let add_variable t var lam =
  (* CR mshinwell: consider checking that [var] is not already mapped *)
  match classify_constant lam with
  | Constant_accessed_via_symbol symbol ->
    { t with
      constants = Variable.Map.add var symbol t.constants;
    }
  | Constant_not_accessed_via_symbol ->
    { t with
      (* XXX what about the clambda component? *)
      subst = Variable.Map.add var lam t.subst;
    }
  | Constant_closure -> t
  | Not_constant ->
    assert (not is_constant);
    env

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
