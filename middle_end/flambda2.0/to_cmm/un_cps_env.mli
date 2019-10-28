(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Environment for flambda2 to cmm translation *)


(** {1 Translation environment} *)

type t
(** Environment for flambda2 to cmm translation *)

val mk : Un_cps_closure.env -> Continuation.t -> Continuation.t -> t
(** [mk offsets k k_exn] creates a local environment for translating
    a flambda2 expression, with return continuation [k] and exception
    continuation [k_exn]. *)

val dummy : Un_cps_closure.env -> t
(** Create an environment with dummy return adn exception continuations. *)


(** {2 Continuations} *)

val return_cont : t -> Continuation.t
(** Returns the return continuation of the environment. *)

val exn_cont : t -> Continuation.t
(** Returns the exception continuation of the environment. *)


(** {2 Variable bindings} *)

val create_variable : t -> Variable.t -> t * Backend_var.With_provenance.t
(** Create (and bind) a cmm variable for the given flambda2 variable, and return
    the new environment, and the created variable. Will fail (i.e. assertion
    failure) if the given variable is already bound. *)

val create_variables : t -> Variable.t list -> t * Backend_var.With_provenance.t list
(** Same as {!create_variable} but for a list of variables. *)

val bind_variable : t -> Variable.t -> Effects_and_coeffects.t -> bool -> Cmm.expression -> t
(** Bind a variable to the given cmm expression, to allow for delaying the let-binding. *)

val get_variable : t -> Variable.t -> Cmm.expression
(** Get the cmm variable bound to a flambda2 variable.
    Will fail (i.e. assertion failure) if the variable is not bound. *)

val inline_variable : t -> Variable.t -> Cmm.expression * t * Effects_and_coeffects.t
(** Try and inline an flambda2 variable using the delayed let-bindings. *)

val flush_delayed_lets : t -> (Cmm.expression -> Cmm.expression) * t
(** Wrap the given cmm expression with all the delayed let bindings accumulated
    in the environment. *)


(** {2 Continuation bindings} *)

type cont =
  | Jump of { types: Cmm.machtype list; cont: int; }
  (** Static jump, with the given cmm continuation.
      The list of machtypes represent the types of arguments expected by the
      catch handler. *)
  | Inline of { handler_params: Kinded_parameter.t list;
                handler_body: Flambda.Expr.t;
                types: Cmm.machtype list;
                cont: int; }
  (** Inline the continuation.
      When inlining is not possible, generate a jump *)
(** Translation information for continuations. A continuation may either
    be translated as a static jump, or inlined at its call site. *)

val add_jump_cont : t -> Cmm.machtype list -> Continuation.t -> int * t
(** Bind the given continuation to a jump, creating a fresh jump id for it. *)

val add_inline_cont :
  t -> Cmm.machtype list -> Continuation.t -> Kinded_parameter.t list
  -> Flambda.Expr.t -> int * t
(** Bind the given continuation as an inline continuation, bound over
    the given variables. *)

val get_k : t -> Continuation.t -> cont
(** Return the binding for a given continuation. Will fail
    (i.e. assertion failure) if given an unbound continuation. *)

val get_jump_id : t -> Continuation.t -> int
(** Returns the jump id bound to a continuation. Will fail (assertion failure),
    if the continuation is not bound. *)


(** {2 Sets of closures and offsets} *)

val closure_offset : t -> Closure_id.t -> int
(** Wrapper around {!Un_cps_closure.closure_offset}. *)

val env_var_offset : t -> Var_within_closure.t -> int
(** Wrapper around {!Un_cps_closure.env_var_offset}. *)

val layout :
  t -> Closure_id.t list -> Var_within_closure.t list -> Un_cps_closure.layout
(** Wrapper around {!Un_cps_closure.layout}. *)
