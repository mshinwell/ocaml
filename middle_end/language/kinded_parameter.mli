(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** A parameter (to a function, continuation, etc.) together with its kind. *)
type t

(** Create a kinded parameter. *)
val create : Parameter.t -> Flambda_kind.t -> t

(** The underlying parameter. *)
val param : t -> Parameter.t

(** The underlying variable (cf. [Parameter.var]). *)
val var : t -> Variable.t

val name : t -> Name.t

(** As for [var], but returns a [Simple.t] describing the variable. *)
val simple : t -> Simple.t

(** The kind of the given parameter. *)
val kind : t -> Flambda_kind.t

(** Replace the kind of the given parameter. *)
val with_kind : t -> Flambda_kind.t -> t

(* CR mshinwell: We should use [Name.t] underneath *)

(** Map the underlying variable of the given parameter. *)
val map_var : t -> f:(Variable.t -> Variable.t) -> t

(** Map the kind of the given parameter. *)
val map_kind : t -> f:(Flambda_kind.t -> Flambda_kind.t) -> t

(** Returns [true] iff the provided kinded parameters have the same kind. *)
val equal_kinds : t -> t -> bool

val rename : t -> t

val free_names : t -> Name.Set.t

module List : sig
  type nonrec t = t list

  val create : (Parameter.t * Flambda_kind.t) list -> t

  (** As for [Parameter.List.vars]. *)
  val vars : t -> Variable.t list

  (** As for [vars] but returns a list of [Simple.t] values describing the
      variables. *)
  val simples : t -> Simple.t list

  (** As for [vars] but returns a set. *)
  val var_set : t -> Variable.Set.t

  (** As for [var_set] but returns a set of [Name]s. *)
  val name_set : t -> Name.Set.t

  (** As for [var_set] but returns a set of [Parameter]s. *)
  val param_set : t -> Parameter.Set.t

  val equal_vars : t -> Variable.t list -> bool

  val rename : t -> t

  val arity : t -> Flambda_arity.t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end

include Hashtbl.With_map with type t := t
