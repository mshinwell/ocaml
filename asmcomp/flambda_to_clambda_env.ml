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
  fclambda_and_approx_for_symbol :
    (Symbol.t -> unit Flambda.t * Clambda.ulambda * Flambdaapprox.t);
  substitution :
    (unit Flambda.t option * Clambda.ulambda * Flambdaapprox.t) Variable.Map.t;
  identifier_assignment : Ident.t Variable.Map.t;
  symbol_assignment : (Symbol.t * Ident.t) Variable.Map.t;
}

let create ~fclambda_and_approx_for_symbol =
  { fclambda_and_approx_for_symbol;
    substitution;
    identifier_assignment;
    symbol_assignment;
  }

let identifier_assignment t = t.identifier_assignment
let symbol_assignment t = t.symbol_assignment

let add_unique_ident t var =
  let id = Variable.unique_ident var in
  let identifier_assignment =
    Variable.Map.add var id t.identifier_assignment
  in
  id, { t with identifier_assignment; }

let add_substitution t var lam ulam approx =
  let id, env = add_unique_ident t in
  let substitution =
    Variable.Map.add var (lam, ulam, approx) t.substitution
  in
  id, { t with substitution; }

let add_substitution_via_known_symbol t var sym lam ulam approx =
  t.symbol_assignment <-
    Symbol.Map.add sym (lam, ulam, approx) t.symbol_assignment;
  let lam, ulam, approx = t.fclambda_and_approx_for_symbol sym in
  (* XXX not sure which approximation should be added here *)
  let id, env = add_substitution t var lam ulam approx in
  id, sym, env

let add_substitution_via_fresh_symbol t var lam ulam approx =
  let sym = Compilenv.new_const_symbol' () in
  add_substitution_via_known_symbol t var sym lam ulam approx

let clean t =
  (* XXX this may erase too much *)
  { t with substitution = Variable.Map.empty; }

(*
(* XXX this is needed in the [Flet] case in flambda_to_clambda *)
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
*)

(*
let add_unique_idents vars env =
  let env_handler, ids =
    List.fold_right (fun var (env, ids) ->
        let id, env = add_unique_ident var env in
        env, id :: ids)
      vars (env, [])
  in
  ids, env_handler
*)
