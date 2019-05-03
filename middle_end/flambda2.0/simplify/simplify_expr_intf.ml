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

module type S = sig
  val simplify_expr
     : Simplify_env_and_result.Env.t
    -> Simplify_env_and_result.Result.t
    -> Flambda.Expr.t
    -> Flambda.Expr.t * Simplify_env_and_result.Result.t

  val simplify_continuation_use_cannot_inline
     : Simplify_env_and_result.Env.t
    -> Simplify_env_and_result.Result.t
    -> Continuation.t
    -> params:Kinded_parameter.t list
    -> Continuation.t * Simplify_env_and_result.Result.t

  (* CR mshinwell: Rename function (it doesn't take Let_cont_handlers.t) *)
  val simplify_let_cont_handlers
     : Simplify_env_and_result.Env.t
    -> Simplify_env_and_result.Result.t
    -> handlers:Flambda.Continuation_handlers.t
    -> recursive:Recursive.t
    -> Flambda.Let_cont_handlers.t option * Simplify_env_and_result.Result.t
end
