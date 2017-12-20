(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Freshening of various identifiers. *)

(** A table used for freshening variables and continuation identifiers. *)
type t
type subst = t

(** The freshening that does nothing.  This is the unique inactive
    freshening. *)
val empty : t

(** Activate the freshening.  Without activation, operations to request
    freshenings have no effect (cf. the documentation below for
    [add_variable]).  As such, the inactive renaming is unique. *)
val activate : t -> t

(** Given the inactive freshening, return the same; otherwise, return an
    empty active freshening. *)
val empty_preserving_activation_state : t -> t

(** [add_variable t var]
    If [t] is active:
      It returns a fresh variable [new_var] and adds [var] -> [new_var]
      to the freshening.
      If a renaming [other_var] -> [var] or [symbol] -> [var] was already
      present in [t], it will also add [other_var] -> [new_var] and
      [symbol] -> [new_var].
    If [t] is inactive, this is the identity.
*)
val add_variable : t -> Variable.t -> Variable.t * t

(** Like [add_variable], but for multiple variables, each freshened
    separately. *)
val add_variables'
   : t
  -> Variable.t list
  -> Variable.t list * t

(** Like [add_variables'], but passes through the second component of the
    input list unchanged. *)
val add_variables
   : t
  -> (Variable.t * 'a) list
  -> (Variable.t * 'a) list * t

(** Like [add_variable], but for mutable variables. *)
val add_mutable_variable : t -> Mutable_variable.t -> Mutable_variable.t * t

(** As for [add_variable], but for continuation identifiers. *)
val add_continuation : t -> Continuation.t -> Continuation.t * t

(** As for [add_variable], but for exception trap identifiers. *)
val add_trap : t -> Trap_id.t -> Trap_id.t * t

(** [apply_variable t var] applies the freshening [t] to [var].
    If no renaming is specified in [t] for [var] it is returned unchanged. *)
val apply_variable : t -> Variable.t -> Variable.t

(** As for [apply_variable], but for mutable variables. *)
val apply_mutable_variable : t -> Mutable_variable.t -> Mutable_variable.t

(** As for [apply_variable], but for continuation identifiers. *)
val apply_continuation : t -> Continuation.t -> Continuation.t

(** As for [apply_variable], but for exception trap identifiers. *)
val apply_trap : t -> Trap_id.t -> Trap_id.t

(** Replace recursive accesses to the closures in the set through
    [Symbol] by the corresponding [Var]. This is used to recover
    the recursive call when importing code from another compilation unit.

    If the renaming is inactive, this is the identity.
*)
val rewrite_recursive_calls_with_symbols
   : t
  -> Flambda.Function_declarations.t
  -> make_closure_symbol:(Closure_id.t -> Symbol.t)
  -> Flambda.Function_declarations.t

val does_not_freshen : t -> Variable.t list -> bool

val print : Format.formatter -> t -> unit

(*
(** N.B. This does not freshen the domain of the supplied map, only the
    range. *)
(* CR-someday mshinwell: consider fixing that *)
val freshen_free_vars_projection_relation
   : Flambda.Free_var.t Variable.Map.t
  -> freshening:t
  -> Flambda.Free_var.t Variable.Map.t

val freshen_free_vars_projection_relation'
   : (Flambda.Free_var.t * 'a) Variable.Map.t
  -> freshening:t
  -> (Flambda.Free_var.t * 'a) Variable.Map.t
*)

val range_of_continuation_freshening : t -> Continuation.Set.t
