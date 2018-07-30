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

(** Typing of join points---that is to say, the entry points of
    continuations. *)

(* CR mshinwell: Rename [default_env]? *)
val parameters_and_body_env
   : Continuation_uses.t
  -> continuation_env_of_definition:Flambda_type.Typing_env.t
  -> existing_continuation_params:Flambda_type.Parameters.t
  -> Flambda_type.Typing_env.t * Flambda_type.Parameters.t

val parameters
   : Continuation_uses.t
  -> continuation_env_of_definition:Flambda_type.Typing_env.t
  -> existing_continuation_params:Flambda_type.Parameters.t
  -> Flambda_type.Parameters.t
