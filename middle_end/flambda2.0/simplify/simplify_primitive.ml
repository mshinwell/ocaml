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

let simplify_primitive env r (prim : Flambda_primitive.t) dbg ~result_var =
  match prim with
  | Unary (prim, arg) ->
    let arg, r = S.simplify_simple_and_drop_type env r arg in
    Named.prim (Unary (prim, arg)) dbg
  | Binary (prim, arg1, arg2) ->
    let arg1, r = S.simplify_simple_and_drop_type env r arg1 in
    let arg2, r = S.simplify_simple_and_drop_type env r arg2 in
    Named.prim (Binary (prim, arg1, arg2)) dbg
  | Ternary (prim, arg1, arg2) ->
    let arg1, r = S.simplify_simple_and_drop_type env r arg1 in
    let arg2, r = S.simplify_simple_and_drop_type env r arg2 in
    let arg3, r = S.simplify_simple_and_drop_type env r arg3 in
    Named.prim (Ternary (prim, arg1, arg2, arg3)) dbg
  | Variadic (prim, args) ->
    let args = S.simplify_simples_and_drop_types env r args in
    Named.prim (Variadic (prim, args)) dbg
