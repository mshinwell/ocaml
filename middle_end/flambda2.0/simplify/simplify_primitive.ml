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

module S = Simplify_simple

let simplify_primitive env r (prim : Flambda_primitive.t) dbg ~result_var:_ =
  let named =
    match prim with
    | Unary (prim, arg) ->
      let arg = S.simplify_simple_and_drop_type env arg in
      Named.create_prim (Unary (prim, arg)) dbg
    | Binary (prim, arg1, arg2) ->
      let arg1 = S.simplify_simple_and_drop_type env arg1 in
      let arg2 = S.simplify_simple_and_drop_type env arg2 in
      Named.create_prim (Binary (prim, arg1, arg2)) dbg
    | Ternary (prim, arg1, arg2, arg3) ->
      let arg1 = S.simplify_simple_and_drop_type env arg1 in
      let arg2 = S.simplify_simple_and_drop_type env arg2 in
      let arg3 = S.simplify_simple_and_drop_type env arg3 in
      Named.create_prim (Ternary (prim, arg1, arg2, arg3)) dbg
    | Variadic (prim, args) ->
      let args = S.simplify_simples_and_drop_types env args in
      Named.create_prim (Variadic (prim, args)) dbg
  in
  Reachable.reachable named, Flambda_type.any_value (), r
