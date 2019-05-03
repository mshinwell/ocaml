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
  val simplify_named
     : Simplify_env_and_result.Env.t
    -> Simplify_env_and_result.Result.t
    -> Flambda.Named.t
    -> result_var:Variable.t
    -> (Variable.t * Flambda_kind.t * Flambda.Named.t) list
         * Flambda.Reachable.t
         * Flambda_type.t
         * Simplify_env_and_result.Result.t

  val simplify_set_of_closures
     : Simplify_env_and_result.Env.t
    -> Simplify_env_and_result.Result.t
    -> Flambda.Set_of_closures.t
    -> Flambda.Set_of_closures.t * Flambda_type.t
         * Simplify_env_and_result.Result.t
end
