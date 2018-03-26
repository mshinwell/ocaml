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

module type S = sig
  type typing_environment
  type env_extension
  type flambda_type
  (* CR mshinwell: rename t_in_context -> flambda_type_in_context *)
  type t_in_context
  type 'a ty
  type 'a unknown_or_join

  type t = typing_environment

  (** Whether a name bound by the environment is normally-accessible or
      has been made existential (as a result of [cut], below). *)
  type binding_type = Normal | Existential

  val invariant : t -> unit

  val create : resolver:(Export_id.t -> flambda_type option) -> t

  val create_using_resolver_from : t -> t

  val add : t -> Name.t -> Scope_level.t -> flambda_type -> t

  (** The same as [add] on a newly-[create]d environment. *)
  val singleton
     : resolver:(Export_id.t -> flambda_type option)
    -> Name.t
    -> Scope_level.t
    -> flambda_type
    -> t

  (** Refine the type of a name that is currently bound in the
      environment.  (It is an error to call this function with a name that
      is not bound in the given environment.) *)
  val replace_meet : t -> Name.t -> t_in_context -> t

  val add_or_replace_meet : t -> Name.t -> Scope_level.t -> flambda_type -> t

  val add_or_replace : t -> Name.t -> Scope_level.t -> flambda_type -> t

  (** Ensure that a binding is not present in an environment.  This function 
      is idempotent. *)
  val remove : t -> Name.t -> t

  (** Perform a lookup in a type environment.  It is an error to provide a
      name which does not occur in the given environment. *)
  val find : t -> Name.t -> flambda_type * binding_type

  val find_with_scope_level
     : t
    -> Name.t
    -> flambda_type * Scope_level.t * binding_type

  (** Like [find], but returns [None] iff the given name is not in the
      specified environment. *)
  val find_opt : t -> Name.t -> (flambda_type * binding_type) option

  val mem : t -> Name.t -> bool

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
      -> flambda_type
      -> 'a)
    -> 'a

  (** Returns [true] if the given name, which must be bound in the given
      environment, is existentially bound. *)
  val is_existential : t -> Name.t -> bool

  (** The continuation scoping level at which the given name, which must
      occur in the given typing context, was declared. *)
  val scope_level : t -> Name.t -> Scope_level.t

  (** Rearrange the given typing environment so that names defined at or
      deeper than the given scope level are made existential.  This means
      that they may be referred to from types but may never occur normally
      in terms (or be produced from a reification of a type, c.f.
      [Flambda_type.reify], etc). *)
  val cut
     : t
    -> existential_if_defined_at_or_later_than:Scope_level.t
    -> t

  (** Least upper bound of two typing environments. *)
  val join : t -> t -> t

  (** Greatest lower bound of two typing environments. *)
  val meet : t -> t -> t

  (** Adjust the domain of the given typing environment so that it only
      mentions the names in the given name occurrences structure. *)
  val restrict_to_names : t -> Name_occurrences.t -> t

  (** Adjust the domain of the given typing environment so that it only
      mentions names which are symbols, not variables. *)
  val restrict_to_symbols : t -> t

  val filter : t -> f:(Name.t -> (Scope_level.t * flambda_type) -> bool) -> t

  (** The names for which the given typing environment specifies a type
      assignment. *)
  val domain : t -> Name_occurrences.t

  val is_empty : t -> bool

  (** Print the given typing environment to a formatter. *)
  val print : Format.formatter -> t -> unit

  val resolver : t -> (Export_id.t -> flambda_type option)

  val aliases : t -> canonical_name:Name.t -> Name.Set.t

  (** By using a [meet] operation add the given environment extension into
      the given typing environment. *)
  val add_env_extension : t -> env_extension -> t

  (** Create an env_extension structure whose typing judgements are those of
      the given typing environment. *)
  val to_env_extension : t -> env_extension

  val diff
     : strictly_more_precise:(t_in_context -> than:t_in_context -> bool)
    -> t
    -> t
    -> env_extension

  val restrict_names_to_those_occurring_in_types
     : t
    -> flambda_type list
    -> t

  (** Return all names occurring in the type and all types referenced by it. *)
  val free_names_transitive : t -> flambda_type -> Name_occurrences.t

  val free_names_transitive_list : t -> flambda_type list -> Name_occurrences.t

  val singleton0
     : resolver:(Export_id.t -> flambda_type option)
    -> Name.t
    -> Scope_level.t
    -> flambda_type
    -> must_be_closed:bool
    -> t

  (** Follow chains of [Alias]es until either a [No_alias] type is reached
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

  val equal
     : equal_type:(flambda_type -> flambda_type -> bool)
    -> t
    -> t
    -> bool

  val phys_equal : t -> t -> bool
end
