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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create
   : Flambda_arity.t
  -> definition_scope_level:Scope.t
  -> t

val print : Format.formatter -> t -> unit

val add_use
   : t
  -> Flambda_type.Typing_env.t
  -> arg_types:Flambda_type.t list
  -> t

val env_and_arg_types
   : t
  -> Flambda_type.Typing_env.t
  -> Flambda_type.Typing_env.t * (Flambda_type.t list)
