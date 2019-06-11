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

module K = Flambda_kind
module S = Simplify_simple
module T = Flambda_type
module TEE = Flambda_type.Typing_env_extension

let simplify_block_load dacc prim ~block ~index dbg ~result_var =
  let index, index_ty = S.simplify_simple dacc index in
  let block, block_ty = S.simplify_simple dacc block in
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
  match Reification.reify_to_tagged_immediate dacc index_ty with
  | Bottom -> invalid ()
  | Ok None -> unchanged ()
  | Ok (Some index) ->
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
  match prim with
  | Block_load (Block (Value Anything), Immutable) ->
    (* CR mshinwell: extend to other block access kinds *)
    simplify_block_load dacc prim ~block:arg1 ~index:arg2 dbg ~result_var
  | _ ->
    (* temporary code *)
    let arg1 = Simplify_simple.simplify_simple_and_drop_type dacc arg1 in
    let arg2 = Simplify_simple.simplify_simple_and_drop_type dacc arg2 in
    let named = Named.create_prim (Binary (prim, arg1, arg2)) dbg in
    let kind = Flambda_primitive.result_kind_of_binary_primitive' prim in
    let ty = T.unknown kind in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Reachable.reachable named, env_extension, dacc
