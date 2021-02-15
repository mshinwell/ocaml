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

val compute_handler_env
   : Continuation_uses.t
  -> env_at_fork_plus_params_and_consts:Downwards_env.t
  -> consts_lifted_during_body:Lifted_constant_state.t
  -> Continuation.t
  -> params:Kinded_parameter.t list
  -> code_age_relation_after_body:Code_age_relation.t
  -> Continuation_env_and_param_types.t
