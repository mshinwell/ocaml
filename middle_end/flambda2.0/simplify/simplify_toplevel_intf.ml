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

module type S = sig
  val simplify_toplevel
     : Simplify_env_and_result.Env.t
    -> Simplify_env_and_result.Result.t
    -> Flambda.Expr.t
    -> return_continuation:Continuation.t
    -> Exn_continuation.t
    -> scope_level_for_lifted_constants:Scope_level.t
    -> Flambda.Expr.t * Simplify_env_and_result.Result.t
end
