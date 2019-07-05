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
  type t

  (** We don't have an interface that insists on adding continuations before
      seeing their uses. This would be problematic when inserting wrappers,
      where we have already advanced past the point at which such wrappers would
      need to be defined, before knowing that a wrapper is needed. *)

  val record_continuation_use
     : t
    -> Continuation.t
    -> typing_env_at_use:Flambda_type.Typing_env.t
    -> arg_types:Flambda_type.t list
    -> t

  (* CR mshinwell: Add [record_exn_continuation_use]? *)

  val continuation_env_and_arg_types
     : t
    -> definition_typing_env:Flambda_type.Typing_env.t
    -> Continuation.t
    -> Flambda_arity.t
    -> Flambda_type.Typing_env.t * (Flambda_type.t list)

  val num_continuation_uses : t -> Continuation.t -> int
end
