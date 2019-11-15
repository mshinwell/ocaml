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

type 'a k =
     Continuation_uses_env.t
  -> Code_age_relation.t
  -> Simplify_env_and_result.Result.t
  -> ('a * Upwards_acc.t)

val simplify_expr
   : Downwards_acc.t
  -> Flambda.Expr.t
  -> 'a k
  -> Flambda.Expr.t * 'a * Upwards_acc.t
