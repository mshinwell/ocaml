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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

open! Flambda.Import

module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module K = Flambda_kind
module S = Simplify_simple
module T = Flambda_type
module TEE = Flambda_type.Typing_env_extension

let simplify_ternary_primitive dacc (prim : Flambda_primitive.ternary_primitive)
      arg1 arg2 arg3 dbg ~result_var =
  let invalid kind =
    let env_extension =
      TEE.one_equation (Name_in_binding_pos.var result_var) (T.bottom kind)
    in
    Reachable.invalid (), env_extension, dacc
  in
  let min_occurrence_kind = Var_in_binding_pos.occurrence_kind result_var in
  match S.simplify_simple dacc arg1 ~min_occurrence_kind with
  | Bottom kind -> invalid kind
  | Ok (arg1, arg1_ty) ->
    match S.simplify_simple dacc arg2 ~min_occurrence_kind with
    | Bottom kind -> invalid kind
    | Ok (arg2, arg2_ty) ->
      match S.simplify_simple dacc arg3 ~min_occurrence_kind with
      | Bottom kind -> invalid kind
      | Ok (arg3, arg3_ty) ->
        match prim with
        | _ ->
          (* temporary code *)
          let named =
            Named.create_prim (Ternary (prim, arg1, arg2, arg3)) dbg
          in
          let kind = Flambda_primitive.result_kind_of_ternary_primitive' prim in
          let ty = T.unknown kind in
          let env_extension =
            TEE.one_equation (Name_in_binding_pos.var result_var) ty
          in
          Reachable.reachable named, env_extension, dacc
