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

(** Tracking of the call sites of continuations. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Use : sig
  module Kind : sig
    (** Information about a particular continuation's use which is necessary
        to record a use using [add_use], below. *)
    type t

    (* CR mshinwell: We might be able to do away with the [Flambda_type.t]
       parts here *)

    (** Describe that a continuation's use is eligible neither for inlining
        nor for specialisation (for example, it might be a [Switch] arm).
        The types of the continuation's parameters, as currently known from
        its definition in the term language, must be provided. *)
    val not_inlinable_or_specialisable : param_tys:Flambda_type.t list -> t

    (** Describe that a continuation's use is eligible, in principle, for
        inlining and specialisation.  The arguments and their types seen at
        the use must be provided. *)
    val inlinable_and_specialisable
       : args_with_tys:(Simple.t * Flambda_type.t) list
      -> t

    (** Describe that a continuation's use is eligible, in principle, for
        specialisable (but not inlining).  The arguments and their types seen
        at the use must be provided. *)
    val only_specialisable
       : args_with_tys:(Simple.t * Flambda_type.t) list
      -> t

(*
    (** The arguments seen at the given continuation use.  (If there is no
        information about the arguments, an empty list will be returned.) *)
    val args : t -> Simple.t list
*)
  end

  (** A single use of a continuation. *)
  type t

  (** Print information about the continuation's use to a formatter. *)
  val print : Format.formatter -> t -> unit

(*
  (** The arguments seen at the given continuation use.  (If there is no
      information about the arguments, an empty list will be returned.) *)
  val args : t -> Simple.t list
*)

(*
  (** As for [args], but returns the types of the arguments. *)
  val arg_tys : t -> Flambda_type.t list

  (** The combination of the results of [args] and [arg_tys]. *)
  val args_with_tys : t -> (Simple.t option * Flambda_type.t) list
*)

  (** Whether the given use is a potential inlining site. *)
  val is_inlinable : t -> bool

(*
  (** Whether the given use is a potential specialisation site.  [None] is
      returned if it is not; otherwise [Some] is returned holding the
      use's arguments with types. *)
  val is_specialisable : t -> (Simple.t * Flambda_type.t) list option
*)

  val parameters : t -> Flambda_type.Parameters.t
end

(** A value of type [t] tracks the call sites of a single continuation. *)
type t

(** Create a use information structure given a continuation, its parameters
    and types (as seen in the term language), and the scope level of the
    corresponding [Let_cont] term. *)
val create
   : continuation:Continuation.t
  -> params:Flambda.Typed_parameter.t list
  -> definition_scope_level:Scope_level.t
  -> t

(** Print information about the continuation's uses to a formatter. *)
val print : Format.formatter -> t -> unit

(** Produce a use information structure that holds all information provided
    by both of the given structures, which must be for the same continuation,
    otherwise a fatal error will result. *)
val union : t -> t -> t

(** Note that a use has been seen of the continuation tracked by [t]. *)
val add_use : t -> Flambda_type.Typing_env.t -> Use.Kind.t -> t

(** All uses of the continuation being tracked by [t]. *)
val uses : t -> Use.t list

(** The continuation being tracked by [t]. *)
val continuation : t -> Continuation.t

(** The parameters of the continuation tracked by [t]. *)
val params : t -> Flambda_type.Parameters.t

(** The definition scope level of the continuation tracked by [t]. *)
val definition_scope_level : t -> Scope_level.t

(** How many uses of the continuation being tracked by [t] have been seen. *)
val num_uses : t -> int

(** As for [num_uses] but returns [Zero], [One] or [Many] for convenience. *)
val num_uses' : t -> Num_continuation_uses.t

(** Returns [true] iff the continuation tracked by [t] has zero uses. *)
val unused : t -> bool

(** Returns [true] iff the continuation tracked by [t] has exactly one use. *)
val linearly_used : t -> bool

(** Returns [true] iff the continuation tracked by [t] has exactly one use
    and that use is inlinable. *)
val linearly_used_in_inlinable_position : t -> bool

(* CR mshinwell: check we really need this, and clarify semantics *)
(** Modify the parameter information, without affecting the recorded use
    information, in the given use information structure. *)
val update_parameters : t -> params:Flambda_type.Parameters.t list -> t
