(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module E = Simplify_env_and_result.Env
module T = Flambda_type

let type_for_const (const : Simple.Const.t) =
  match const with
  (* CR mshinwell: unify terminology: "untagged" vs "naked" *)
  | Untagged_immediate i -> T.this_naked_immediate i
  | Tagged_immediate i -> T.this_tagged_immediate i
  | Naked_float f -> T.this_naked_float f
  | Naked_int32 n -> T.this_naked_int32 n
  | Naked_int64 n -> T.this_naked_int64 n
  | Naked_nativeint n -> T.this_naked_nativeint n

let simplify_name env name =
  let name = Freshening.apply_name (E.freshening env) name in
  let ty = E.find_name env name in
  let ty, canonical_name =
    T.resolve_aliases (E.get_typing_environment env, ty)
  in
  match canonical_name with
  | None -> name, ty
  | Some canonical_name -> canonical_name, ty

let simplify_simple env (simple : Simple.t) =
  match simple with
  | Const c -> simple, type_for_const c
  | Name name ->
    let name = Freshening.apply_name (E.freshening env) name in
    let ty = E.find_name env name in
    let reified =
      T.reify ~allow_free_variables:true
        (E.get_typing_environment env)
        ty
    in
    match reified with
    | Term (simple, ty) -> simple, ty
    | Cannot_reify | Lift _ -> Simple.name name, ty
    | Invalid -> Simple.name name, T.bottom_like ty

let simplify_simples env simples =
  List.map (fun simple -> simplify_simple env simple) simples
