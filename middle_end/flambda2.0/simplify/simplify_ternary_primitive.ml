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

open! Flambda.Import

module E = Simplify_env_and_result.Env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type

let simplify_block_set env r prim dbg ~block_access_kind ~init_or_assign
      ~block ~index ~new_value =
  let block, block_ty = S.simplify_simple env block in
  let index, index_ty = S.simplify_simple env index in
  let new_value, new_value_ty = S.simplify_simple env new_value in
  let original_term =
    Named.prim (Ternary (prim, block, index, new_value)) dbg
  in
  Reachable.reachable term, T.any_value (), r

let simplify_bytes_or_bigstring_set env r prim dbg
      (bytes_like_value : Flambda_primitive.bytes_like_value)
      ~string_accessor_width ~str ~index ~new_value =
  let str, _str_ty = S.simplify_simple env str in
  let index, index_ty = S.simplify_simple env index in
  let new_value, new_value_ty = S.simplify_simple env new_value in
  let term = Named.prim (Ternary (prim, str, index, new_value)) dbg in
  Reachable.reachable term, T.any_value (), r

let simplify_ternary_primitive env r
      (prim : Flambda_primitive.ternary_primitive) arg1 arg2 arg3 dbg =
  match prim with
  | Block_set (block_access_kind, init_or_assign) ->
    simplify_block_set env r prim dbg ~block_access_kind ~init_or_assign
      ~block:arg1 ~index:arg2 ~new_value:arg3
  | Bytes_or_bigstring_set (bytes_like_value, string_accessor_width) ->
    simplify_bytes_or_bigstring_set env r prim dbg bytes_like_value
      ~string_accessor_width ~str:arg1 ~index:arg2 ~new_value:arg3
