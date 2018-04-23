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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module type S = sig
  type typing_environment
  type typing_environment_entry
  type typing_environment_entry0
  type env_extension
  type flambda_type
  (* CR mshinwell: rename t_in_context -> flambda_type_in_context *)
  type t_in_context
  type 'a ty
  type 'a unknown_or_join

  type t = typing_environment

  (** Perform various invariant checks upon the given environment. *)
  val invariant : t -> unit

  (** Print the given typing environment to a formatter. *)
  val print : Format.formatter -> t -> unit

  (** Equality on two environments. *)
  val equal : t -> t -> bool

  (** A sound but not complete equality function which is much faster than
      [equal]. *)
  val fast_equal : t -> t -> bool

  (** Create an empty environment using the given [resolver] to locate the
      definitions of export identifiers (e.g. by loading .cmx files). *)
  val create : resolver:(Export_id.t -> flambda_type option) -> t

  (** As for [create] but takes the [resolver] from an existing
      environment. *)
  val create_using_resolver_from : t -> t

  (** The export identifier resolver from the given environment. *)
  val resolver : t -> (Export_id.t -> flambda_type option)

  (** Returns [true] iff the given environment contains no bindings.
      (An environment containing only existential bindings is not deemed
      as empty.) *)
  val is_empty : t -> bool

  (** The names for which the given typing environment specifies a type
      assignment.  (Note that [domain] returning an empty name occurrences
      structure does not imply that [is_empty] holds; there may still be CSE
      equations, which [domain] does not look at.) *)
  val domain : t -> Name_occurrences.t

  (** Define the type of a name, add a typing equation on a name, or add a
      CSE equation on one or more names.  Names may not be redefined nor
      equations added involving names that are undefined in the given
      environment.  Neither definitions nor equations may introduce
      circularities.  Added equations are not allowed to specify types for
      (already-bound) names which are less precise than that which the
      given environment knows already.  The specified scoping level, which
      typically corresponds to continuation scoping, will be augmented by a
      scoping sublevel computed by the environment. *)
  val add : t -> Name.t -> Scope_level.t -> typing_environment_entry -> t

  (** The same as [add] on a newly-[create]d environment. *)
  val singleton
     : resolver:(Export_id.t -> flambda_type option)
    -> Name.t
    -> Scope_level.t
    -> typing_environment_entry
    -> t

  (** Ensure that a binding is not present in an environment.  This function 
      is idempotent. *)
  val remove : t -> Name.t -> t

  (** Determine the most precise type which the environment knows for the
      given name. *)
  (* CR mshinwell: Clean up "Flambda_type0_internal_intf.binding_type" *)
  val find_exn
     : t
    -> Name.t
    -> flambda_type * Flambda_type0_internal_intf.binding_type

  (** As for [find] but returns the scoping level of the given name as well. *)
  val find_with_scope_level_exn
     : t
    -> Name.t
    -> flambda_type * Scope_level.With_sublevel.t
         * Flambda_type0_internal_intf.binding_type

  (** Like [find], but returns [None] iff the given name is not in the
      specified environment. *)
  val find_opt
     : t
    -> Name.t
    -> (flambda_type * Flambda_type0_internal_intf.binding_type) option

  (** Return a name, if such is available, which may be substituted for the
      given primitive in the fashion of CSE.  (This function checks if the
      primitive is suitable for such an operation.) *)
  val find_cse : t -> Flambda_primitive.t -> Name.t option

  (** The scoping level known for the given name, which must be bound by the
      given environment. *)
  val scope_level_exn : t -> Name.t -> Scope_level.With_sublevel.t

  (** Whether the given name is bound in the environment (either normally
      or existentially). *)
  val mem : t -> Name.t -> bool

  (** Returns [true] if the given name, which must be bound in the given
      environment, is existentially bound. *)
  val is_existential_exn : t -> Name.t -> bool

  (** Fold over entries of the typing environment.  The entries are passed
      to [f] in order of increasing (level, sublevel) order (i.e. outermost
      bindings first). *)
  val fold
     : t
    -> init:'a
    -> f:('a
      -> Name.t
      -> Flambda_type0_internal_intf.binding_type
      -> Scope_level.With_sublevel.t
      -> typing_environment_entry0
      -> 'a)
    -> 'a

  (** Like [fold], with the same ordering properties, but just an iterator. *)
  val iter
     : t
    -> f:(Name.t
      -> Flambda_type0_internal_intf.binding_type
      -> Scope_level.With_sublevel.t
      -> typing_environment_entry0
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
    -> env_extension

  (* CR mshinwell: What exactly does "domain" mean here?  (existentials...) *)
  (** Adjust the domain of the given typing environment so that it only
      mentions the names in the given name occurrences structure. *)
  val restrict_to_names : t -> Name_occurrences.t -> t

  (** Adjust the domain of the given typing environment so that it only
      mentions names which are symbols, not variables. *)
  val restrict_to_symbols : t -> t

  (** The equivalent of finding all free names in the given types and then
      calling [restrict_to_names]. *)
  val restrict_names_to_those_occurring_in_types
     : t
    -> flambda_type list
    -> t

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
  val add_or_meet_env_extension
     : t
    -> env_extension
    -> Scope_level.t
    -> t

  (** Like [add_or_meet_env_extension] except uses a join function to
      determine merged types.
      [add_or_join_env_extension t ext1 ext2 ext level] has [ext] as the
      extension being added/joined into the environment [t]; the extensions
      [ext1] and [ext2] are pushed down onto resulting joined types. *)
  val add_or_join_env_extension
     : t
    -> env_extension
    -> env_extension
    -> env_extension
    -> Scope_level.t
    -> t

  (** [diff t1 t2] computes the environment extension whose bindings are
      those in [t1] that:
        - do not occur in [t2]; or
        - do occur in [t2] but where [t1] specifies more precise information
          (which for types, means closer to bottom).
  *)
  val diff : t -> t -> env_extension

  (** Return all names occurring in the type and all types referenced by it. *)
  val free_names_transitive : t -> flambda_type -> Name_occurrences.t

  (** The union of [free_names_transitive] across the given list of types. *)
  val free_names_transitive_list : t -> flambda_type list -> Name_occurrences.t

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
    -> t_in_context
    -> flambda_type * (Name.t option)

  (** Like [resolve_aliases], except working on types statically known to be
      of a particular kind, and returning an [unknown_or_join]. *)
  val resolve_aliases_and_squash_unresolved_names_on_ty'
     : t
    -> ?bound_name:Name.t
    -> print_ty:(Format.formatter -> 'a ty -> unit)
    -> force_to_kind:(flambda_type -> 'a ty)
    -> 'a ty
    -> 'a unknown_or_join * (Name.t option)
end
