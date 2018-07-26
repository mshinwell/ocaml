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

(** The interface of typing environments. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module Flambda_type : sig
    type t
    type 'a ty
    type 'a unknown_or_join
  end
  module Function_parameters : sig type t end
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  type typing_environment_entry0 = private
    | Definition of Flambda_type.t
    | Equation of Flambda_type.t

  type typing_environment_entry =
    | Definition of Flambda_type.t
    | Equation of Flambda_type.t
    | CSE of Flambda_primitive.With_fixed_value.t

  type t

  (** Perform various invariant checks upon the given environment. *)
  val invariant : t -> unit

  (** Print the given typing environment to a formatter. *)
  val print : Format.formatter -> t -> unit

  (** Create an empty environment using the given [resolver] to locate the
      definitions of export identifiers (e.g. by loading .cmx files). *)
  val create : resolver:(Export_id.t -> Flambda_type.t option) -> t

  (** As for [create] but takes the [resolver] from an existing
      environment. *)
  val create_using_resolver_from : t -> t

  (** The export identifier resolver from the given environment. *)
  val resolver : t -> (Export_id.t -> Flambda_type.t option)

  (** Returns [true] iff the given environment contains no bindings.
      (An environment containing only existential bindings is not deemed
      as empty.) *)
  val is_empty : t -> bool

  val fast_equal : t -> t -> bool

  (** The names for which the given typing environment specifies a type
      assignment, including for names which were once existentially-bound. (Note
      that [domain] returning an empty name occurrences structure does not imply
      that [is_empty] holds; there may still be CSE equations, which [domain]
      does not look at.) *)
  val domain : t -> Name_occurrences.t

  (** The highest scope level for which the given environment contains a
      binding. *)
  val max_level : t -> Scope_level.t

  (** Define the type of a name, add a typing equation on a name, or add a
      CSE equation on one or more names.  Names may not be redefined nor
      equations added involving names that are undefined in the given
      environment.  Neither definitions nor equations may introduce
      circularities.  Added equations are not allowed to specify types for
      (already-bound) names which are less precise than that which the
      given environment knows already.  The specified scoping level, which
      typically corresponds to continuation scoping, will be augmented by a
      scoping sublevel computed by the environment. *)
  (* CR mshinwell: I think maybe this should be add_definition, and
     add_equation should be used otherwise, which meets. *)
  val add : t -> Name.t -> Scope_level.t -> typing_environment_entry -> t

  (** The same as [add] on a newly-[create]d environment. *)
  val singleton
     : resolver:(Export_id.t -> Flambda_type.t option)
    -> Name.t
    -> Scope_level.t
    -> typing_environment_entry
    -> t

  (** Add a new equation for a name already bound by the given typing
      environment.  The actual type of the added equation will be the meet of
      the current best type specified by the environment for the given name
      with the supplied type. *)
  val add_equation : t -> Name.t -> Scope_level.t -> Flambda_type.t -> t

  (** Ensure that a binding is not present in an environment.  This function 
      is idempotent. *)
  val remove : t -> Name.t -> t

  type binding_type = private
    | Normal
    | Was_existential

  (** Determine the most precise type which the environment knows for the
      given name. *)
  (* CR mshinwell: Clean up "T.binding_type" *)
  val find_exn
     : t
    -> Name.t
    -> Flambda_type.t * binding_type

  (** As for [find] but returns the scoping level of the given name as well. *)
  val find_with_scope_level_exn
     : t
    -> Name.t
    -> Flambda_type.t * Scope_level.With_sublevel.t * binding_type

  (** Like [find], but returns [None] iff the given name is not in the
      specified environment. *)
  val find_opt : t -> Name.t -> (Flambda_type.t * binding_type) option

  (** Return a name or constant, if such is available, which may be
      substituted for the given primitive in the fashion of CSE.  (This
      function checks if the primitive is suitable for such an operation.) *)
  val find_cse : t -> Flambda_primitive.t -> Simple.t option

  (** The scoping level known for the given name, which must be bound by the
      given environment. *)
  val scope_level_exn : t -> Name.t -> Scope_level.With_sublevel.t

  (** Whether the given name is bound in the environment (either normally
      or existentially). *)
  val mem : t -> Name.t -> bool

  (* XXX Change to "mode"? *)
  (** Returns [true] if the given name, which must be bound in the given
      environment, is an opened existential. *)
  val was_existential_exn : t -> Name.t -> bool

  (** Fold over entries of the typing environment.  The entries are passed
      to [f] in order of increasing (level, sublevel) order (i.e. outermost
      bindings first). *)
  val fold
     : t
    -> init:'a
    -> f:('a
      -> Name.t
      -> binding_type
      -> Scope_level.With_sublevel.t
      -> Flambda_type.typing_environment_entry0
      -> 'a)
    -> 'a

  (** Like [fold], with the same ordering properties, but just an iterator. *)
  val iter
     : t
    -> f:(Name.t
      -> binding_type
      -> Scope_level.With_sublevel.t
      -> Flambda_type.typing_environment_entry0
      -> unit)
    -> unit

  (** Cut off any portion of the given environment containing definitions
      and/or equations at or after (deeper) the given scope level.

      Such portion is returned as an environment extension; all names defined
      in such extension will henceforth be treated as existentially bound.

      If no such portion exists, an empty extension is returned. *)
  val cut
     : t
    -> existential_if_defined_at_or_later_than:Scope_level.t
    -> Typing_env_extension.t

  (** Adjust the domain of the given typing environment so that it only
      mentions names which are symbols, not variables. *)
  val restrict_to_symbols : t -> t

  (** Like [restrict_to_names] except using a traditional filtering
      predicate.  A name will only be kept if the predicate returns [true]
      for the name. *)
  val filter
     : t
    -> f:(Name.t
      -> (Scope_level.With_sublevel.t * typing_environment_entry0)
      -> bool)
    -> t

  (** Add the given environment extension into the given typing environment.
      During the process, if an attempt is made to add a name which is
      already bound, the given name's type will be determined using a meet
      operation. *)
  (* CR mshinwell: Maybe this (and the next function) doesn't need to take
     a level and instead use the max? *)
  val add_or_meet_env_extension
     : t
    -> Typing_env_extension.t
    -> Scope_level.t
    -> t

  (** As for [add_or_meet_env_extension], but also returns the freshening
      used to open existentials in the supplied extension. *)
  val add_or_meet_env_extension'
     : t
    -> Typing_env_extension.t
    -> Scope_level.t
    -> t * Freshening.t

  (** Follow chains of aliases until either a [No_alias] type is reached
      or a name cannot be resolved.

      This function also returns the "canonical name" for the given type:
      the furthest-away [Name.t] in any chain of aliases leading from the given
      type.  (The chain may also involve [Export_id.t] links either before or
      after any returned canonical name.)

      If this function is being used to resolve aliases for a type bound in
      an environment, or similar, then the corresponding name of such binding
      should be specified as [bound_name].  This enables the bound name to
      be involved in checks for non-circularity.
  *)
  val resolve_aliases
     : ?bound_name:Name.t
    -> Flambda_type.t_in_context
    -> Flambda_type.t * (Simple.t option)

  val resolve_aliases_and_squash_unresolved_names_on_ty'
     : t
    -> ?bound_name:Name.t
    -> print_ty:(Format.formatter -> 'a Flambda_type.ty -> unit)
    -> force_to_kind:(Flambda_type.t -> 'a Flambda_type.ty)
    -> 'a Flambda_type.ty
    -> 'a Flambda_type.unknown_or_join * (Simple.t option)

  (** All names (not including the given name) which are known to be aliases
      of the given [Simple.t] in the given environment.  (For [Name]s this
      is the usual notion of alias; for [Const]s and [Discriminant]s an
      "alias" is just another name known to hold that same value.) *)
  val aliases_of_simple : t -> Simple.t -> Name.Set.t
end
