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
    over the notion of "continuation handler". This enables the code to be
    reused for simplifying [Flambda_static] expressions, where there is no
    [Expr.t] (and thus no [Continuation_handler.t]) associated with the logic of
    a return continuation, in addition to normal [Let_cont] constructs.
*)

type 'a result = private
  | No_wrapper of 'a
  | With_wrapper of {
      wrapper : Flambda.Continuation_handler.t;
      renamed_original_cont : Continuation.t;
      original : 'a;
    }

module Make (Continuation_handler_like : sig
  type t

  val is_exn_handler : t -> bool

  val stub : t -> bool

  val arity : t -> Flambda_arity.t

  type behaviour = private
    | Unreachable of { arity : Flambda_arity.t; }
    | Alias_for of { arity : Flambda_arity.t; alias_for : Continuation.t; }
    | Unknown of { arity : Flambda_arity.t; }

  val behaviour : t -> behaviour

  val real_handler : t -> Flambda.Continuation_handler.t option
end) : sig
  val simplify_body_of_non_recursive_let_cont
     : Downwards_acc.t
    -> Continuation.t
    -> Continuation_handler_like.t
    -> body:Flambda.Expr.t
    -> (Downwards_acc.t
      -> arg_types:Flambda_type.t list
      -> Continuation.t
      -> Continuation_handler_like.t
      -> (Continuation_uses_env.t
        -> Simplify_env_and_result.Result.t
        -> ('a * Upwards_acc.t))
      -> Continuation_handler_like.t result * 'a * Upwards_acc.t)
    -> (Continuation_uses_env.t
      -> Simplify_env_and_result.Result.t
      -> ('a * Upwards_acc.t))
    -> Flambda.Expr.t * Continuation_handler_like.t result * 'a * Upwards_acc.t
end
