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
    -> Continuation_use_kind.t
    -> typing_env_at_use:Flambda_type.Typing_env.t
    -> inside_handlers_of_recursive_continuations_at_use:Scope.Set.t
    -> arg_types:Flambda_type.t list
    -> t * Apply_cont_rewrite_id.t

  (* CR mshinwell: Add [record_exn_continuation_use]? *)

  val compute_handler_env
     : t
    -> Continuation.t
    -> Recursive.t
    -> definition_typing_env_with_params_defined:Flambda_type.Typing_env.t
    -> params:Kinded_parameter.t list
    -> param_types:Flambda_type.t list
    -> Continuation_env_and_param_types.t

  val num_continuation_uses : t -> Continuation.t -> int
end
