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
module T = Flambda_type
module TEE = T.Typing_env_extension

let simplify_primitive dacc (prim : Flambda_primitive.t) dbg ~result_var =
(*Format.eprintf "Simplifying primitive:@ %a\n%!" Flambda_primitive.print prim;*)
  match prim with
  | Unary (prim, arg) ->
    Simplify_unary_primitive.simplify_unary_primitive dacc prim arg dbg
      ~result_var
  | _ ->
    (* CR mshinwell: temporary code *)
    let named =
      match prim with
      | Unary _ -> assert false
      | Binary (prim, arg1, arg2) ->
        let arg1 = S.simplify_simple_and_drop_type dacc arg1 in
        let arg2 = S.simplify_simple_and_drop_type dacc arg2 in
        Named.create_prim (Binary (prim, arg1, arg2)) dbg
      | Ternary (prim, arg1, arg2, arg3) ->
        let arg1 = S.simplify_simple_and_drop_type dacc arg1 in
        let arg2 = S.simplify_simple_and_drop_type dacc arg2 in
        let arg3 = S.simplify_simple_and_drop_type dacc arg3 in
        Named.create_prim (Ternary (prim, arg1, arg2, arg3)) dbg
      | Variadic (prim, args) ->
        let args = S.simplify_simples_and_drop_types dacc args in
        Named.create_prim (Variadic (prim, args)) dbg
    in
    let ty = T.any_value () in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Reachable.reachable named, env_extension, dacc
