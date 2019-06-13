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

module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module T = Flambda_type
module TE = T.Typing_env

(* CR mshinwell: This should be simplified if possible *)
let simplify_simple dacc (simple : Simple.t) =
let _orig = simple in
  match Simple.descr simple with
  | Const c -> simple, T.type_for_const c
  | Discriminant t -> simple, T.this_discriminant t
  | Name name ->
(*Format.eprintf "simplify_simple %a\n%!" Name.print name;*)
    let typing_env = DE.typing_env (DA.denv dacc) in
    let ty = TE.find typing_env name in
(*Format.eprintf "ty: %a\n%!" T.print ty;*)
    (* We reify an [Equals] type in case the type itself can't be reified but
       there is an interesting alias (e.g. a symbol). *)
    let kind = T.kind ty in
    let reified =
      T.reify ~allow_free_variables:true typing_env
        (T.alias_type_of kind (Simple.name name))
    in
    match reified with
    | Term (simple, ty) ->
(*
Format.eprintf "returning reified Simple: %a --> %a\n%!"
  Simple.print orig
  Simple.print simple;
*)
      simple, ty
    | Cannot_reify | Lift _ ->
      let name = TE.get_canonical_name typing_env name in
(*
Format.eprintf "Non-reifiable Simple: %a --> %a, env:@ %a\n%!"
  Simple.print orig
  Name.print name
  TE.print typing_env;
*)
      Simple.name name, T.alias_type_of kind (Simple.name name)
    | Invalid -> Simple.name name, T.bottom_like ty

let simplify_simple_and_drop_type dacc simple =
  fst (simplify_simple dacc simple)

let simplify_simples dacc simples =
  List.map (fun simple -> simplify_simple dacc simple) simples

let simplify_simples_and_drop_types dacc simples =
  List.map (fun simple -> simplify_simple_and_drop_type dacc simple) simples
