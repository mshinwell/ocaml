(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Unbox parameters of continuations based on the inferred types at the use
    points of such continuations. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val for_non_recursive_continuation
   : name:Continuation.t
  -> handler:Flambda.Continuation_handler.t
  -> env:Simplify_env_and_result.Env.t
  -> arg_tys:Flambda_type.t list
  -> Flambda.Expr.with_wrapper

val for_recursive_continuations
   : handlers:Flambda.Continuation_handlers.t
  -> env:Simplify_env_and_result.Env.t
  -> arg_tys:Flambda_type.t list Continuation.Map.t
  -> Flambda.Expr.with_wrapper Continuation.Map.t
