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

let simplify_primitive env r (prim : Flambda_primitive.t) dbg ~result_var =
  match prim with
  | Unary (prim, arg) ->
    Simplify_unary_primitive.simplify_unary_primitive env r prim arg dbg
      ~result_var
  | Binary (prim, arg1, arg2) ->
    Simplify_binary_primitive.simplify_binary_primitive env r prim arg1 arg2 dbg
  | Ternary (prim, arg1, arg2, arg3) ->
    Simplify_ternary_primitive.simplify_ternary_primitive env r prim
      arg1 arg2 arg3 dbg
  | Variadic (prim, args) ->
    Simplify_variadic_primitive.simplify_variadic_primitive env r prim args dbg
