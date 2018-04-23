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

(** Using information collected across all use sites of a particular
    continuation, calculate:
    - the types of a continuation's parameters; and
    - the typing environment for the body of the continuation (which
      includes bindings for the continuation's parameters).

    In the event that there are no uses then the [default_env] will be used
    as the body environment.  (This environment must include bindings for
    the continuation's parameters.) *)
(* CR mshinwell: Rename [default_env]? *)
val param_types_and_body_env
   : Continuation_uses.t
  -> Freshening.t
  -> arity:Flambda_arity.t
  -> default_env:Flambda_type.Typing_env.t
  -> Flambda_type.t list * Flambda_type.Typing_env.t
