(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module computes, for each parameter of a continuation, the join of all
    corresponding argument types across the recorded uses; and the environment
    to be used for simplifying the continuation itself. *)

[@@@ocaml.warning "+a-30-40-41-42"]

val compute_handler_env
   : Continuation_uses_env.t
  -> Downwards_acc.t
  -> env_at_fork_plus_params_and_consts:Simplify_envs.Downwards_env.t
  -> consts_lifted_during_body:Simplify_envs.Lifted_constant_state.t
  -> params:Kinded_parameter.t list
  -> code_age_relation_after_body:Code_age_relation.t
  -> Continuation_env_and_param_types.t * Downwards_acc.t
