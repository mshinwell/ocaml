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

(** Logic for the simplification of continuation bindings that is parameterised
    over the notions of "continuation handler" and "body" (the latter being the
    scope of a continuation). This enables the code to be reused for simplifying
    [Flambda_static] expressions, where there is no [Expr.t] (and thus no
    [Continuation_handler.t]) associated with the logic of return or exception
    continuations, in addition to normal [Let_cont] constructs.
*)

module Make (Continuation_handler_like : Continuation_handler_like_intf.S) : sig
  type 'body simplify_body = {
    simplify_body : 'a.
         Downwards_acc.t
      -> 'body
      -> (Continuation_uses_env.t
        -> Simplify_env_and_result.Result.t
        -> ('a * Upwards_acc.t))
      -> 'body * 'a * Upwards_acc.t;
  }

  val simplify_body_of_non_recursive_let_cont
     : Downwards_acc.t
    -> Continuation.t
    -> Continuation_handler_like.t
    -> simplify_body:'body simplify_body
    -> body:'body
    -> simplify_continuation_handler_like:(Downwards_acc.t
      -> extra_params_and_args:Continuation_extra_params_and_args.t
      -> Continuation.t
      -> Continuation_handler_like.Opened.t
      -> user_data:'user_data
      -> (Continuation_uses_env.t
        -> Simplify_env_and_result.Result.t
        -> ('a * Upwards_acc.t))
      -> Continuation_handler_like.t * 'a * Upwards_acc.t)
    -> user_data:'user_data
    -> (Continuation_uses_env.t
      -> Simplify_env_and_result.Result.t
      -> ('a * Upwards_acc.t))
    -> 'body * Continuation_handler_like.t * 'a * Upwards_acc.t
end
