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

open! Flambda.Import

module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module K = Flambda_kind
module S = Simplify_simple
module T = Flambda_type
module TEE = Flambda_type.Typing_env_extension

let simplify_block_load dacc prim ~block ~block_ty ~index ~index_ty
      dbg ~result_var =
  let original_term = Named.create_prim (Binary (prim, block, index)) dbg in
  let unchanged () =
    let ty = T.any_value () in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Reachable.reachable original_term, env_extension, dacc
  in
  let invalid () =
    let ty = T.bottom K.value in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Reachable.invalid (), env_extension, dacc
  in
(*Format.eprintf "Reifying index type: %a\n%!" T.print index_ty;*)
  let typing_env = DE.typing_env (DA.denv dacc) in
  match T.prove_equals_single_tagged_immediate typing_env index_ty with
  | Invalid -> invalid ()
  | Unknown -> unchanged ()
  | Proved index ->
(*Format.eprintf "The block index is %a\n%!" Immediate.print index;*)
    let n =
      Targetint.OCaml.add (Immediate.to_targetint index) Targetint.OCaml.one
    in
    Simplify_primitive_common.simplify_projection
      dacc ~original_term ~deconstructing:block_ty
      ~shape:(T.immutable_block_with_size_at_least ~n
        ~field_n_minus_one:result_var)
      ~result_var ~result_kind:K.value

let simplify_binary_primitive dacc (prim : Flambda_primitive.binary_primitive)
      arg1 arg2 dbg ~result_var =
  let invalid kind =
    let env_extension =
      TEE.one_equation (Name_in_binding_pos.var result_var) (T.bottom kind)
    in
    Reachable.invalid (), env_extension, dacc
  in
  let min_occurrence_kind = Var_in_binding_pos.occurrence_kind result_var in
  let result_var = Var_in_binding_pos.var result_var in
  match S.simplify_simple dacc arg1 ~min_occurrence_kind with
  | Bottom kind -> invalid kind
  | Ok (arg1, arg1_ty) ->
    match S.simplify_simple dacc arg2 ~min_occurrence_kind with
    | Bottom kind -> invalid kind
    | Ok (arg2, arg2_ty) ->
      match prim with
      | Block_load (Block (Value Anything), Immutable) ->
        (* CR mshinwell: extend to other block access kinds *)
        simplify_block_load dacc prim ~block:arg1 ~block_ty:arg1_ty
          ~index:arg2 ~index_ty:arg2_ty dbg ~result_var
      | _ ->
        (* temporary code *)
        let named = Named.create_prim (Binary (prim, arg1, arg2)) dbg in
        let kind = Flambda_primitive.result_kind_of_binary_primitive' prim in
        let ty = T.unknown kind in
        let env_extension = TEE.one_equation (Name.var result_var) ty in
        Reachable.reachable named, env_extension, dacc
