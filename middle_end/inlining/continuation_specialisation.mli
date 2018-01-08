(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 OCamlPro SAS                                          *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Introduce specialised arguments to continuations. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

val for_toplevel_expression
   : Flambda.Expr.t
  -> vars_in_scope:Variable.Set.t
  -> Simplify_env_and_result.Result.t
  -> simplify_let_cont_handlers:(Simplify_env_and_result.Env.t
    -> Simplify_env_and_result.Result.t
    -> handlers:Flambda.Let_cont_handlers.t
    -> recursive:Flambda.recursive
    -> freshening:Freshening.t
    -> Flambda.Let_cont_handlers.t option * Simplify_env_and_result.Result.t)
  -> backend:(module Backend_intf.S)
  -> Flambda.Expr.t option
