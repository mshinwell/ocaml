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

(** The type of alpha-equivalence classes of expressions. *)
type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

type descr = private
  | Let of Let_expr.t
  (** Bind a variable.  There can be no effect on control flow (save for
      asynchronous operations such as the invocation of finalisers or
      signal handlers as a result of reaching a safe point). *)
  | Let_cont of Let_cont_expr.t
  (** Define one or more continuations. *)
  | Apply of Apply.t
  (** Call an OCaml function, external function or method. *)
  | Apply_cont of Apply_cont.t
  (** Call a continuation, optionally adding or removing exception trap
      frames from the stack, which thus allows for the raising of
      exceptions. *)
  | Switch of Switch.t
  (** Conditional control flow. *)
  | Invalid of Invalid_term_semantics.t
  (** Code proved type-incorrect and therefore unreachable. *)

(** Extract the description of an expression. *)
val descr : t -> descr

type let_creation_result = private
  | Have_deleted of Named.t
  | Nothing_deleted

(** Create a variable binding.  Unnecessary variable bindings will not be
    created and their associated defining expressions will be reported as
    [Have_deleted]. *)
val create_let0
   : Bindable_let_bound.t
  -> Named.t
  -> t
  -> t * let_creation_result

(** Like [create_let0], but for use when the caller isn't interested in
    whether something got deleted. *)
val create_let : Bindable_let_bound.t -> Named.t -> t -> t

(** Create an application expression. *)
val create_apply : Apply.t -> t

(** Create a continuation application (in the zero-arity case, "goto"). *)
val create_apply_cont : Apply_cont.t -> t

type switch_creation_result = private
  | Have_deleted_comparison_but_not_branch
  | Have_deleted_comparison_and_branch
  | Nothing_deleted

(** Create a [Switch] expression, save that zero-arm switches are converted
    to [Invalid], and one-arm switches to [Apply_cont]. *)
val create_switch0
   : scrutinee:Name.t
  -> arms:Continuation.t Discriminant.Map.t
  -> Expr.t * switch_creation_result

(** Like [create_switch0], but for use when the caller isn't interested in
    whether something got deleted. *)
val create_switch
   : scrutinee:Name.t
  -> arms:Continuation.t Discriminant.Map.t
  -> Expr.t

(** Build a [Switch] corresponding to a traditional if-then-else. *)
val create_if_then_else
   : scrutinee:Name.t
  -> if_true:Continuation.t
  -> if_false:Continuation.t
  -> t

(** Create an expression indicating type-incorrect or unreachable code. *)
val create_invalid : unit -> t

(** [bind [var1, expr1; ...; varN, exprN] body] binds using
    [Immutable] [Let] expressions the given [(var, expr)] pairs around the
    body. *)
val bind
   : bindings:(Bindable_let_bound.t * Named.t) list
  -> body:t
  -> t

(** Given lists of kinded parameters [p_1; ...; p_n] and simples
    [s_1; ...; s_n], create an expression that surrounds the given
    expression with bindings of each [p_i] to the corresponding [s_i],
    such as is typically used when performing an inlining transformation. *)
val bind_parameters_to_simples
   : bind:Kinded_parameter.t list
  -> target:Simple.t list
  -> t
  -> t

(** Create an expression that binds, over the given expression, the
    continuation [bind] such that when [bind] is called control is
    transferred to [target].  Both [bind] and [target] must have the
    given [arity]. *)
val link_continuations
   : bind:Continuation.t
  -> target:Continuation.t
  -> arity:Flambda_arity.t
  -> t
  -> t

val create_let_cont : Let_cont_expr.t -> t
