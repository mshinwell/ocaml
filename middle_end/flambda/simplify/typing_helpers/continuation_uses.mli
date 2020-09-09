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

(** Recording of the uses of a single continuation.  This module also
    computes, for each parameter of the continuation, the join of all
    corresponding argument types across the recorded uses; and the environment
    to be used for simplifying the continuation itself. *)

[@@@ocaml.warning "+a-30-40-41-42"]

type t

val create
   : Continuation.t
  -> Flambda_arity.t
  -> t

val print : Format.formatter -> t -> unit

val add_use
   : t
  -> Continuation_use_kind.t
  -> env_at_use:Simplify_envs.Downwards_env.t
  -> Apply_cont_rewrite_id.t
  -> arg_types:Flambda_type.t list
  -> t

val get_uses : t -> One_continuation_use.t list

val compute_handler_env
   : t
  -> env_at_fork_plus_params_and_consts:Simplify_envs.Downwards_env.t
  -> consts_lifted_during_body:Simplify_envs.Lifted_constant_state.t
  -> params:Kinded_parameter.t list
  -> code_age_relation_after_body:Code_age_relation.t
  -> Continuation_env_and_param_types.t

val number_of_uses : t -> int

val arity : t -> Flambda_arity.t

val get_typing_env_no_more_than_one_use
   : t
  -> Flambda_type.Typing_env.t option

val union : t -> t -> t
