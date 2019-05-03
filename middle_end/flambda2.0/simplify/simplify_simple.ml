(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
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

let type_for_const (const : Simple.Const.t) =
  match const with
  | Naked_immediate i -> T.this_naked_immediate i
  | Tagged_immediate i -> T.this_tagged_immediate i
  | Naked_float f -> T.this_naked_float f
  | Naked_int32 n -> T.this_naked_int32 n
  | Naked_int64 n -> T.this_naked_int64 n
  | Naked_nativeint n -> T.this_naked_nativeint n

let simplify_name_for_let env r name =
  let typing_env = E.typing_env env in
  let ty, _ = TE.find_exn typing_env name in
  let ty, canonical_simple = TE.resolve_aliases typing_env ty in
  let simple =
    match canonical_simple with
    | None -> Simple.name name
    | Some canonical_simple -> canonical_simple
  in
  simple, T.alias_type_of (T.kind ty) simple, r

let simplify_simple_for_let env r (simple : Simple.t) =
  match simple with
  | Const c -> simple, type_for_const c, r
  | Discriminant t -> simple, T.this_discriminant t, r
  | Name name -> simplify_name_for_let env r name

let simplify_name env name =
  let typing_env = E.typing_env env in
  let ty, _ = TE.find_exn typing_env name in
  let ty, canonical_simple = TE.resolve_aliases typing_env ty in
  match canonical_simple with
  | None -> Simple.name name, ty
  | Some canonical_simple -> canonical_simple, ty

let simplify_simple env (simple : Simple.t) =
  match simple with
  | Const c -> simple, type_for_const c
  | Discriminant t -> simple, T.this_discriminant t
  | Name name ->
    let typing_env = E.typing_env env in
    let ty, _ = TE.find_exn typing_env name in
(* Experiment: don't reify so that we preserve relations *)
    let simple = Simple.name name in
    simple, T.alias_type_of (T.kind ty) simple
(* The following works.  Maybe we could do this only for names which were
   syntactically bound to constants.
    let reified =
      T.reify ~allow_free_variables:true
        (E.get_typing_environment env)
        ty
    in
    match reified with
    | Term (simple, ty) -> simple, ty
    | Cannot_reify | Lift _ -> Simple.name name, ty
    | Invalid -> Simple.name name, T.bottom_like ty
*)

let simplify_simples env simples =
  List.map (fun simple -> simplify_simple env simple) simples
