(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module E = Simplify_env_and_result.Env
module T = Flambda_type
module TE = T.Typing_env

let simplify_name env name =
  let typing_env = E.typing_env env in
  let name = TE.get_canonical_name typing_env name in
  (* CR mshinwell: Avoid double lookup here *)
  let ty = TE.find typing_env name in
  name, ty

let simplify_simple env (simple : Simple.t) =
  match simple with
  | Const c -> simple, T.type_for_const c
  | Discriminant t -> simple, T.this_discriminant t
  | Name name ->
    let typing_env = E.typing_env env in
    let ty = TE.find typing_env name in
    let reified = T.reify ~allow_free_variables:true typing_env ty in
    match reified with
    | Term (simple, ty) -> simple, ty
    | Cannot_reify | Lift _ -> Simple.name name, ty
    | Invalid -> Simple.name name, T.bottom_like ty

let simplify_simple_and_drop_type env simple =
  fst (simplify_simple env simple)

let simplify_simples env simples =
  List.map (fun simple -> simplify_simple env simple) simples

let simplify_simples_and_drop_types env simples =
  List.map (fun simple -> simplify_simple_and_drop_type env simple) simples
