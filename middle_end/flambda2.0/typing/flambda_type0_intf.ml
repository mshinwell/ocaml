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

(** The interface to [Flambda_type0] once the type system has been
    instantiated for a particular expression language (typically
    [Flambda0]). *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Add invariant checks, including e.g. on the bodies of
   functions in types. *)

module Float = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module type S = sig
  type term_language_function_declaration

  type t
  type flambda_type = t

  include Expr_std.S with type t := t

  module Parameters : sig
    type t

    include Expr_std.S with type t := t

    val empty : t
  end

  module Typing_env : sig
    type t

    (** Perform various invariant checks upon the given environment. *)
    val invariant : t -> unit

    (** Print the given typing environment to a formatter. *)
    val print : Format.formatter -> t -> unit

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

    val resolve_aliases
       : ?bound_name:Name.t
      -> t
      -> flambda_type
      -> flambda_type * (Simple.t option)

    val find_exn : t -> Name.t -> flambda_type
  end

  (* CR mshinwell: Substitute out this alias once it's finalised *)
  type 'a type_accessor = Typing_env.t -> 'a

  module Typing_env_extension : sig
    type t

    include Expr_std.S with type t := t

    val empty : t

    val is_empty : t -> bool
  end

  type 'a ty
  type 'a unknown_or_join

  type ty_value
  type 'a ty_naked_number
  type ty_fabricated

  type function_declaration

(*
  module rec Join_env : sig
    type t

    (** Perform various invariant checks upon the given join environment. *)
    val invariant : t -> unit

    val create : Meet_env.t -> t
  end and Meet_env : sig
    type t

    val create
       : Typing_env.t
      -> perm_left:Name_permutation.t
      -> perm_right:Name_permutation.t
      -> t

    val print : Format.formatter -> t -> unit

    val env : t -> Typing_env.t

    val perm_left : t -> Name_permutation.t

    val perm_right : t -> Name_permutation.t

    (** Note that we are now in the process of meeting the given two
        [Simple]s. *)
    val now_meeting : t -> Simple.t -> Simple.t -> t

    (** Determine whether we are now in the process of meeting the given two
        [Simple]s.  The arguments do not have to be provided in the same order
        as when [now_meeting] was called. *)
    val already_meeting : t -> Simple.t -> Simple.t -> bool

    val shortcut_precondition : t -> bool

    val with_env : t -> (Typing_env.t -> Typing_env.t) -> t

    val clear_name_permutations : t -> t

    val compose_name_permutations
       : t
      -> perm_left:Name_permutation.t
      -> perm_right:Name_permutation.t
      -> t
  end and Typing_env : sig

(*
    type t

    type binding_type = Normal | Was_existential

    type typing_env_entry0 =
      | Definition of Flambda_kind.t
      | Equation of flambda_type

    type typing_env_entry = private
      | Definition of Flambda_kind.t
      | Equation of flambda_type
      | CSE of Flambda_primitive.With_fixed_value.t
        (* CR mshinwell: Consider removing "of t" for [Definition] (and maybe
           change it to [Introduce_name] -- the "t" would be implicitly
           bottom) *)

    (** Perform various invariant checks upon the given environment. *)
    val invariant : t -> unit

    (** Print the given typing environment to a formatter. *)
    val print : Format.formatter -> t -> unit

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

    val fast_equal : t -> t -> bool

    (** The names for which the given typing environment specifies a type
        assignment, including for names which were once existentially-bound.
        (Note that [domain] returning an empty name occurrences structure does
        not imply that [is_empty] holds; there may still be CSE equations, which
        [domain] does not look at.) *)
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
    val add : t -> Name.t -> Scope_level.t -> typing_env_entry -> t

    (** The same as [add] on a newly-[create]d environment. *)
    val singleton
       : resolver:(Export_id.t -> flambda_type option)
      -> Name.t
      -> Scope_level.t
      -> typing_env_entry
      -> t

    (** Add a new equation for a name already bound by the given typing
        environment.  The actual type of the added equation will be the meet of
        the current best type specified by the environment for the given name
        with the supplied type. *)
    val add_equation : t -> Name.t -> Scope_level.t -> flambda_type -> t

    (** Ensure that a binding is not present in an environment.  This function 
        is idempotent. *)
    val remove : t -> Name.t -> t

    (** Determine the most precise type which the environment knows for the
        given name. *)
    (* CR mshinwell: Clean up "binding_type" *)
    val find_exn
       : t
      -> Name.t
      -> flambda_type * binding_type

    (** As for [find] but returns the scoping level of the given name as
        well. *)
    val find_with_scope_level_exn
       : t
      -> Name.t
      -> flambda_type * Scope_level.With_sublevel.t * binding_type

    (** Like [find], but returns [None] iff the given name is not in the
        specified environment. *)
    val find_opt : t -> Name.t -> (flambda_type * binding_type) option

    (** Return a name or constant, if such is available, which may be
        substituted for the given primitive in the fashion of CSE.  (This
        function checks if the primitive is suitable for such an operation.) *)
    val find_cse : t -> Flambda_primitive.t -> Simple.t option

    (** The scoping level known for the given name, which must be bound by the
        given environment. *)
    val scope_level_exn : t -> Name.t -> Scope_level.t

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
        -> typing_env_entry0
        -> 'a)
      -> 'a

    (** Like [fold], with the same ordering properties, but just an iterator. *)
    val iter
       : t
      -> f:(Name.t
        -> binding_type
        -> Scope_level.With_sublevel.t
        -> typing_env_entry0
        -> unit)
      -> unit

    (** Adjust the domain of the given typing environment so that it only
        mentions names which are symbols, not variables. *)
    val restrict_to_symbols : t -> t

(*
    (** Like [restrict_to_names] except using a traditional filtering
        predicate.  A name will only be kept if the predicate returns [true]
        for the name. *)
    val filter
       : t
      -> f:(Name.t
        -> (Scope_level.With_sublevel.t * typing_env_entry0)
        -> bool)
      -> t
*)

    (** Add the given environment extension into the given typing environment.
        During the process, if an attempt is made to add a name which is
        already bound, the given name's type will be determined using a meet
        operation. *)
    val add_env_extension : t -> Typing_env_extension.t -> t

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
      -> t
      -> flambda_type
      -> flambda_type * (Simple.t option)

    val resolve_aliases_and_squash_unresolved_names_on_ty'
       : t
      -> ?bound_name:Name.t
      -> print_ty:(Format.formatter -> 'a ty -> unit)
      -> force_to_kind:(flambda_type -> 'a ty)
      -> 'a ty
      -> 'a unknown_or_join * (Simple.t option)

    (** All names (not including the given name) which are known to be aliases
        of the given [Simple.t] in the given environment.  (For [Name]s this
        is the usual notion of alias; for [Const]s and [Discriminant]s an
        "alias" is just another name known to hold that same value.) *)
    val aliases_of_simple : t -> Simple.t -> Name.Set.t
*)
    type t

    type binding_type = private
      | Normal
      | Was_existential

    val invariant : t -> unit

    val print : Format.formatter -> t -> unit

    val create : resolver:(Export_id.t -> flambda_type option) -> t

    val create_using_resolver_from : t -> t

    val resolver : t -> (Export_id.t -> flambda_type option)

    val is_empty : t -> bool

    val increment_scope_level_to : t -> Scope_level.t -> t

    val fast_equal : t -> t -> bool

    val domain : t -> Name_occurrences.t

    val add_definition : t -> Name.t -> Flambda_kind.t -> t

    val add_equation : t -> Name.t -> flambda_type -> t

    val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

    val find_exn : t -> Name.t -> flambda_type * binding_type

    val find_opt : t -> Name.t -> (flambda_type * binding_type) option

    val find_cse : t -> Flambda_primitive.t -> Simple.t option

    val mem : t -> Name.t -> bool

    val add_env_extension
       : t
      -> Typing_env_extension.t
      -> t

    val add_opened_env_extension
       : t
      -> Typing_env_level.t
      -> t

    val resolve_aliases
       : ?bound_name:Name.t
      -> t
      -> flambda_type
      -> flambda_type * (Simple.t option)

    val resolve_aliases_and_squash_unresolved_names_on_ty'
       : t
      -> ?bound_name:Name.t
      -> print_ty:(Format.formatter -> 'a ty -> unit)
      -> force_to_kind:(flambda_type -> 'a ty)
      -> 'a ty
      -> 'a unknown_or_join * (Simple.t option)

    val aliases_of_simple : t -> Simple.t -> Name.Set.t

    val cut
       : t
      -> existential_if_defined_at_or_later_than:Scope_level.t
      -> Typing_env_extension.t

    val was_existential_exn : t -> Name.t -> bool
  end and Typing_env_extension : sig
    type t

    include Contains_names.S with type t := t

    val invariant : t -> unit

    val print : Format.formatter -> t -> unit

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val fast_equal : t -> t -> bool

    val empty : unit -> t

    val is_empty : t -> bool

    val create : Typing_env_level.t -> t

    val add_definition : t -> Name.t -> Flambda_kind.t -> t

    val add_equation : t -> Name.t -> flambda_type -> t

    val meet_equation : t -> Typing_env.t -> Name.t -> flambda_type -> t

    val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

    val meet : Meet_env.t -> t -> t -> t

    val join : Join_env.t -> t -> t -> t

(*
    val restrict_to_definitions : t -> t

    val restrict_names_to_those_occurring_in_types
       : t
      -> Typing_env.t
      -> Typing_env.t
      -> flambda_type list
      -> t

    val diff : t -> Typing_env.t -> t
*)

    val pattern_match
       : t
      -> f:(Typing_env_level.t -> 'a)
      -> 'a
(*
    type t

    include Contains_names.S with type t := t

    (** Perform various invariant checks upon the given environment
        extension. *)
    val invariant : t -> unit

    (** Print the given typing environment to a formatter. *)
    val print : Format.formatter -> t -> unit

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    (** The unique environment extension containing no information. *)
    val empty : unit -> t

    (** Whether the given environment extension contains no information. *)
    val is_empty : t -> bool

    (** Cut off any portion of the given environment containing definitions
        and/or equations at or after (deeper) the given scope level.

        Such portion is returned as an environment extension; all names defined
        in such extension will henceforth be treated as existentially bound.

        If no such portion exists, an empty extension is returned. *)
    val create_from_cut
       : Typing_env.t
      -> existential_if_defined_at_or_later_than:Scope_level.t
      -> t

    (** Existentially bind the given name as a definition in the given
        extension.

        To define existentially-bound names with equations, first add the
        equations using [add_equation] (see below), and then use
        [add_definition].  Otherwise the capture of the name will inhibit
        the adding of an equation upon such name. *)
    val add_definition : t -> Name.t -> t

    (** Add an equation on a name in the given extension.

        If the extension already binds the name then a [meet] operation is
        performed to determine the resulting type of the name.  Any equations
        generated by such [meet] will also be added to the extension.

        In the case where a [meet] is required the [env] parameter will be
        used as the context surrounding the extension.  [env] may be
        omitted when it is known that the name does not occur in the given
        extension already; or when it is known that the [meet] operation will
        not examine any names free in the extension. *)
    val add_equation : ?env:Typing_env.t -> t -> Name.t -> flambda_type -> t

    (* CR mshinwell: document semantics if the primitive already exists *)
    (** Add a pure-primitive-CSE equation into the given extension. *)
    val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

    (** Greatest lower bound of two environment extensions in the given
        context. *)
    val meet
       : Meet_env.t
      -> t
      -> t
      -> t

    (** Least upper bound of two environment extensions in the given context. *)
    val join
       : Join_env.t
      -> t
      -> t
      -> t
(*
    val restrict_to_definitions : t -> t

    val restrict_names_to_those_occurring_in_types
       : t
      -> Typing_env.t
      -> Typing_env.t
      -> flambda_type list
      -> t

    (** [diff t env] computes the environment extension whose bindings are
        those in [t], when interpreted in the context of [env], that:
          - do not occur in [env]; or
          - do occur in [env] but where [t] contains a more precise type.
    *)
    val diff : t -> Typing_env.t -> t
*)
*)
  end and Typing_env_level : sig
    include Contains_names.S

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val print : Format.formatter -> t -> unit

    val invariant : t -> unit

    val empty : unit -> t

    val is_empty : t -> bool

    val find_opt : t -> Name.t -> flambda_type option

    val defined_names_set : t -> Bindable_name.Set.t

    val equations_domain : t -> Name.Set.t

    val equations_on_outer_env_domain : t -> Name.Set.t

(*
    val restrict_to_names : t -> Name_occurrences.t -> t


    val add_definition : t -> Name.t -> Flambda_kind.t -> t

    val add_equation : t -> Typing_env.t -> Name.t -> flambda_type -> t

    val meet_equation : t -> Typing_env.t -> Name.t -> flambda_type -> t

    val add_or_replace_equation : t -> Name.t -> flambda_type -> t

    val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

    val meet : Meet_env.t -> t -> t -> t

    val join : Join_env.t -> t -> t -> t

    val defined_names : t -> Flambda_kind.t Name.Map.t

    val defined_names_in_order : t -> Bindable_name.t list

    val equations : t -> flambda_type Name.Map.t

    val cse : t -> Simple.t Flambda_primitive.With_fixed_value.Map.t
*)
  end

  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  (** Greatest lower bound of two types.  The process of meeting may generate
      equations, which are returned as an environment extension. *)
  val meet : Meet_env.t -> t -> t -> t * Typing_env_extension.t

  (** Least upper bound of two types.  This never generates any equations. *)
  val join : ?bound_name:Name.t -> Join_env.t  -> t -> t -> t

  (** Like [strictly_more_precise], but also returns [true] when the two
      input types are equally precise. *)
  val as_or_more_precise : Typing_env.t -> t -> than:t -> bool

  (** Returns [true] if the first type is known to provide strictly more
      information about the corresponding value than the type [than]. *)
  val strictly_more_precise : Typing_env.t -> t -> than:t -> bool

  (** Slow type equality. *)
  val equal
     : bound_name:Name.t option
    -> Typing_env.t
    -> Typing_env.t
    -> flambda_type
    -> flambda_type
    -> bool

  (** Fast type equality---sound but far from complete. *)
  val fast_equal
     : Typing_env.t
    -> Typing_env.t
    -> flambda_type
    -> flambda_type
    -> bool

  val get_alias : t -> Simple.t option

  (* CR mshinwell: Shouldn't this take an environment, so it can
     resolve aliases? *)
  val is_obviously_bottom : t -> bool

  val of_ty_value : ty_value -> t

  val of_ty_naked_number
     : 'kind ty_naked_number
    -> 'kind Flambda_kind.Naked_number.t
    -> t

  val of_ty_fabricated : ty_fabricated -> t

  (** Construction of top types. *)
  val unknown : Flambda_kind.t -> t
*)
  val any_value : unit -> t
(*
  val any_value_as_ty_value : unit -> ty_value
*)

  val any_fabricated : unit -> t
(*
  val any_fabricated_as_ty_fabricated : unit -> ty_fabricated
*)

  val any_tagged_immediate : unit -> t
  val any_tagged_bool : unit -> t

  val any_boxed_float : unit -> t
  val any_boxed_int32 : unit -> t
  val any_boxed_int64 : unit -> t
  val any_boxed_nativeint : unit -> t

  val any_naked_immediate : unit -> t
  val any_naked_float : unit -> t

(*
  val any_naked_float_as_ty_naked_float
    : unit
   -> Numbers.Float_by_bit_pattern.Set.t ty_naked_number

  (** The top type for unboxed 32-bit numbers. *)
  val any_naked_int32 : unit -> t

  (** The top type for unboxed 64-bit numbers. *)
  val any_naked_int64 : unit -> t

  (** The top type for unboxed "nativeint" numbers. *)
  val any_naked_nativeint : unit -> t
*)

  (** Building of types representing tagged / boxed values from specified
      constants. *)
  val this_tagged_immediate : Immediate.t -> t
  val this_boxed_float : Numbers.Float_by_bit_pattern.t -> t
  val this_boxed_int32 : Int32.t -> t
  val this_boxed_int64 : Int64.t -> t
  val this_boxed_nativeint : Targetint.t -> t

(*
  val these_tagged_immediates : Immediate.Set.t -> t
  val these_boxed_floats : Numbers.Float_by_bit_pattern.Set.t -> t
  val these_boxed_int32s : Numbers.Int32.Set.t -> t
  val these_boxed_int64s : Numbers.Int64.Set.t -> t
  val these_boxed_nativeints : Targetint.Set.t -> t

  val this_immutable_string : string -> t
  val this_immutable_float_array
     : Numbers.Float_by_bit_pattern.t array
    -> t

  (** A type representing a set of tagged immediates combined with typing
      judgements that will be used if the set contains, or is subsequently
      refined to contain, only a unique element. *)
  val these_tagged_immediates_with_envs
     : Typing_env_extension.t Immediate.Map.t
    -> t

*)

  (** Building of types representing untagged / unboxed values from
      specified constants. *)
  val this_naked_immediate : Immediate.t -> t
  val this_naked_float : Numbers.Float_by_bit_pattern.t -> t
  val this_naked_int32 : Int32.t -> t
  val this_naked_int64 : Int64.t -> t
  val this_naked_nativeint : Targetint.t -> t

(*
  val this_naked_float_as_ty_naked_float
     : Numbers.Float_by_bit_pattern.t
    -> Numbers.Float_by_bit_pattern.Set.t ty_naked_number
  val these_naked_floats : Numbers.Float_by_bit_pattern.Set.t -> t
  val these_naked_int32s : Numbers.Int32.Set.t -> t
  val these_naked_int64s : Numbers.Int64.Set.t -> t
  val these_naked_nativeints : Targetint.Set.t -> t

  (** Building of types corresponding to immutable values given only the
      size of such values. *)
  val immutable_string : size:Targetint.OCaml.t -> t

  (** The type corresponding to a mutable string of length [size]. *)
  val mutable_string : size:Targetint.OCaml.t -> t

  (** The type corresponding to a mutable float array holding [size]
      naked floats. *)
  val mutable_float_array : size:Targetint.OCaml.t -> t
*)

  (** Building of types corresponding to values that did not exist at
      source level. *)

  (** The given discriminant. *)
  val this_discriminant : Discriminant.t -> t

(*
  (** Like [this_discriminant], but returns the [ty_fabricated], rather than
      a type. *)
  val this_discriminant_as_ty_fabricated
     : Discriminant.t
    -> ty_fabricated

  (** The given block discriminants coupled with the env_extensions that hold
      if the corresponding block can be shown to have one of the
      discriminants. *)
  val these_discriminants
     : Typing_env_extension.t Discriminant.Map.t
    -> t

  (** Like [these_discriminants], but returns the [ty_fabricated], rather than
      a value of type [t]. *)
  val these_discriminants_as_ty_fabricated
     : Typing_env_extension.t Discriminant.Map.t
    -> ty_fabricated

  (** Any discriminant. *)
  val any_discriminant_as_ty_fabricated : unit -> ty_fabricated

  (** Given the type of a naked floating-point number, return the type of the
      corresponding boxed version. *)
  val box_float : t -> t

  (** Given the type of a naked int32 number, return the type of the
      corresponding boxed version. *)
  val box_int32 : t -> t

  (** Given the type of a naked int64 number, return the type of the
      corresponding boxed version. *)
  val box_int64 : t -> t

  (** Given the type of a naked nativeint number, return the type of the
      corresponding boxed version. *)
  val box_nativeint : t -> t

  (** The type of a float array containing the given floating-point numbers. *)
  val immutable_float_array
     : Numbers.Float_by_bit_pattern.Set.t ty_naked_number array
    -> t
*)

  (** The type of a block with a known tag, size and field types. *)
  val block
     : Tag.t
    -> fields:t list
    -> t

(*

  (** Like [block], except that the field types are statically known to be
      of kind [Value]). *)
  val block_of_values
     : Tag.t
    -> fields:ty_value list
    -> t

  (** The type of a block with a known tag and size but unknown content,
      save that the contents are all of kind [Value]. *)
  val block_of_unknown_values
     : Tag.t
    -> size:int
    -> t

  (** The type of a block with at least [n] fields and an unknown tag.
      The type of the [n - 1]th field is taken to be an [Equals] to the
      given variable. *)
  val block_with_size_at_least
     : n:int
    -> field_n_minus_one:Variable.t
    -> t

  (** The bottom type for the given kind ("no value can flow to this point"). *)
  val bottom : Flambda_kind.t -> t

  (** The bottom type for kind [Value] expressed as a type whose kind is
      statically known. *)
  val bottom_as_ty_value : unit -> ty_value

  (** The bottom type for kind [Fabricated] expressed as a type whose kind is
      statically known. *)
  val bottom_as_ty_fabricated : unit -> ty_fabricated
*)

  (** Create an "bottom" type with the same kind as the given type. *)
  val bottom_like : t -> t

  (** Create an "unknown" type with the same kind as the given type. *)
  val unknown_like : t -> t
  
  (** Create a description of a function declaration whose code is known. *)
  val create_inlinable_function_declaration
     : term_language_function_declaration
    -> function_declaration

  (** Create a description of a function declaration whose code is unknown.
      Such declarations cannot be inlined. *)
  val create_non_inlinable_function_declaration
     : unit
    -> function_declaration

  val term_language_function_declaration
     : function_declaration
    -> term_language_function_declaration option

  (** Create a closure type given full information about the closure. *)
  val closure
     : Closure_id.t
    -> function_declaration
    -> ty_value Var_within_closure.Map.t
    -> set_of_closures:ty_fabricated
    -> t

(*
  (** The type of a closure (of kind [Value]) containing at least one
      closure that holds the given closure variable with the given type. *)
  val closure_containing_at_least
     : Var_within_closure.t
    -> t
*)

  (** The type of a set of closures containing exactly those closure IDs
      with the given types. *)
  val set_of_closures
     : closures:t Closure_id.Map.t
    -> t

(*
  (** The type of a set of closures containing at least one closure with
      the given closure ID. *)
  val set_of_closures_containing_at_least : Closure_id.t -> t
*)

  (** Construct a type equal to the type of the given name.  (The name
      must be present in the given environment when calling e.g. [join].) *)
  val alias_type_of : Flambda_kind.t -> Simple.t -> t

(*
  (** Like [alias_type_of], but for types of kind [Value], and returns the
      [ty] rather than a [t]. *)
  val alias_type_of_as_ty_value : Simple.t -> ty_value
*)

  (** Like [alias_type_of_as_ty_value] but for types of [Fabricated] kind. *)
  val alias_type_of_as_ty_fabricated : Simple.t -> ty_fabricated

(*
  (** The type that is equal to another type, found in a .cmx file, named
      by export identifier. *)
  val alias_type : Flambda_kind.t -> Export_id.t -> t
*)

  (** Determine the (unique) kind of a type. *)
  val kind : t -> Flambda_kind.t


  (** Enforce that a type is of kind [Value], returning the corresponding
      [ty]. *)
  val force_to_kind_value : t -> ty_value

(*
  (** Enforce that a type is of a naked number kind, returning the
      corresponding [ty]. *)
  val force_to_kind_naked_number
     : 'kind Flambda_kind.Naked_number.t
    -> t
    -> 'kind ty_naked_number

  (** Enforce that a type is of naked float kind, returning the corresponding
      [ty]. *)
  val force_to_kind_naked_float
     : t
    -> Numbers.Float_by_bit_pattern.Set.t ty_naked_number

  val force_to_kind_naked_int32
     : t
    -> Numbers.Int32.Set.t ty_naked_number

  val force_to_kind_naked_int64
     : t
    -> Numbers.Int64.Set.t ty_naked_number

  val force_to_kind_naked_nativeint
     : t
    -> Targetint.Set.t ty_naked_number

  val force_to_kind_naked_immediate
     : t
    -> Immediate.Set.t ty_naked_number

  (** Enforce that a type is of fabricated kind. *)
  val force_to_kind_fabricated
     : t
    -> ty_fabricated

  (** Enforce that a type is of a given kind. *)
  val check_of_kind : t -> Flambda_kind.t -> unit

  (** For each of the kinds in an arity, create a "bottom" type. *)
  val bottom_types_from_arity : Flambda_arity.t -> t list

  (** For each of the kinds in an arity, create an "unknown" type. *)
  val unknown_types_from_arity : Flambda_arity.t -> t list

  (** Like [unknown_like] but for a array of types. *)
  val unknown_like_array : t array -> t array

  (* CR mshinwell: rename to [unknown_unit]? *)
  val unit : unit -> t

  val unit_bottom : unit -> t
*)

  (** Whether the given type says that a term of that type can never be
      constructed (in other words, it is [Invalid]). *)
  val is_bottom : (t -> bool) type_accessor

(*
  (** Determine whether the given type provides any information about an
      Flambda term of that type. *)
  val is_known : (t -> bool) type_accessor

  (** Determine whether the given type provides useful information about an
      Flambda term of that type. *)
  val is_useful : (t -> bool) type_accessor

  (** Whether all types in the given list do *not* satisfy [useful]. *)
  val all_not_useful : (t list -> bool) type_accessor
*)

(*
  (** Whether values of the given two types will always be physically equal
      to each other. *)
  val values_physically_equal : t -> t -> bool

  (** Whether values of the given two types will always have a different
      structure from each other. *)
  val values_structurally_distinct
     : (Typing_env.t * t)
    -> (Typing_env.t * t)
    -> bool
*)

  type to_lift = private
    | Boxed_float of Float.t
    | Boxed_int32 of Int32.t
    | Boxed_int64 of Int64.t
    | Boxed_nativeint of Targetint.t

  type reification_result = private
    | Term of Simple.t * t
    | Lift of to_lift
    | Cannot_reify
    | Invalid

  (** Try to produce a canonical Flambda term that has the given Flambda type.
      The resulting term will never cause an allocation.  The term will also
      not contain any free variables unless [allow_free_variables] has been set
      to [true].

      This function may be used to turn the types of [Simple] terms into their
      canonical representative terms (as it follows aliases in the environment).

      The returned type will not be an alias type in the case where the type
      completely describes the reified value.  In other cases, aliases will be
      preserved, in case the types in question get refined later.

      If [expected_kind] does not match the kind of the term / type being
      returned then a fatal error will be produced.
  *)
  val reify
     : (allow_free_variables:bool
    -> t
    -> reification_result) type_accessor

(*
  type 'a proof = private
    | Proved of 'a
    | Unknown
    | Invalid

(*
  val unknown_proof : unit -> _ proof
*)

  (* CR mshinwell: Add unit tests to check that the condition about the result
     sets in [Proved] being non-empty holds. *)

  (** Prove that the given type represents exactly some particular set of
      tagged immediates ("Proved (Exactly ...)").  The set is guaranteed to be
      non-empty.  Alternatively, prove that the given type is known to represent
      only tagged immediates, but it is not known which ones
      ("Proved Not_all_values_known").  If neither of these proofs can be given
      then either [Unknown] (stating that the type may yet represent one or more
      tagged immediates, but we don't know) or [Invalid] (stating that the
      type definitely cannot represent any tagged immediate) is returned.
  *)
  val prove_tagged_immediate : (t -> Immediate.Set.t proof) type_accessor


(*
  type tagged_immediate_as_discriminants_proof = private
    | By_discriminant of Typing_env_extension.t Discriminant.Map.t

  val prove_tagged_immediate_as_discriminants
     : (t -> tagged_immediate_as_discriminants_proof proof) type_accessor

  type is_tagged_immediate = private
    | Never_a_tagged_immediate
    | Always_a_tagged_immediate

  val prove_is_tagged_immediate : (t -> is_tagged_immediate proof) type_accessor
*)

  (** Similar to [prove_tagged_immediate], but for naked float values. *)
  val prove_naked_float
     : (t -> Numbers.Float_by_bit_pattern.Set.t proof) type_accessor

  (** Similar to [prove_tagged_immediate], but for naked int32 values. *)
  val prove_naked_int32
     : (t -> Numbers.Int32.Set.t proof) type_accessor

  (** Similar to [prove_tagged_immediate], but for naked int64 values. *)
  val prove_naked_int64
     : (t -> Numbers.Int64.Set.t proof) type_accessor

  (** Similar to [prove_tagged_immediate], but for naked nativeint values. *)
  val prove_naked_nativeint
     : (t -> Targetint.Set.t proof) type_accessor

(*
  val prove_unique_naked_float
     : (t -> Numbers.Float_by_bit_pattern.t proof) type_accessor

  val prove_unique_naked_int32
     : (t -> Numbers.Int32.t proof) type_accessor

  val prove_unique_naked_int64
     : (t -> Numbers.Int64.t proof) type_accessor

  val prove_unique_naked_nativeint
     : (t -> Targetint.t proof) type_accessor

  val prove_get_field_from_block
     : (t
      -> index:Targetint.OCaml.t
      -> field_kind:Flambda_kind.t
      -> (Name.t * Typing_env_extension.t) proof) type_accessor

  val prove_must_be_a_block
     : (t
      -> kind_of_all_fields:Flambda_kind.t
      -> unit proof) type_accessor

  (** As for [prove_tagged_immediate], but for strings. *)
  val prove_string : (t -> String_info.Set.t proof) type_accessor

*)

  (** Prove that the given type represents a boxed float value, returning the
      type of the unboxed number therein.  (That type may in itself specify
      a union, etc.)  This function returns [Unknown] and [Invalid] in
      equivalent situations as for [prove_tagged_immediate]. *)
  val prove_boxed_float
     : (t -> Numbers.Float_by_bit_pattern.Set.t ty_naked_number proof)
         type_accessor

  (** As for [prove_boxed_float], but for boxed int32 values. *)
  val prove_boxed_int32
     : (t -> Numbers.Int32.Set.t ty_naked_number proof)
         type_accessor

  (** As for [prove_boxed_float], but for boxed int64 values. *)
  val prove_boxed_int64
     : (t -> Numbers.Int64.Set.t ty_naked_number proof)
         type_accessor

  (** As for [prove_boxed_float], but for boxed nativeint values. *)
  val prove_boxed_nativeint
     : (t -> Targetint.Set.t ty_naked_number proof)
         type_accessor

(*

  type tags = private
    | Tags of Tag.Set.t

  (** Determine which tags values of the given type may take on. *)
  (* CR mshinwell: duplicate return value (Proved empty_set, and Invalid) *)
  val prove_tags : (t -> tags proof) type_accessor

  (** Determine the set of all possible length(s) of the array(s) or structured
      block(s) (i.e. blocks with tag less than [No_scan_tag]) described by the
      given type. This function correctly handles float arrays (where the length
      of the array, on 32-bit platforms, may differ from the size of the block).
      [Unknown] is returned if a proof cannot be given but the type may yet
      represent array(s) or block(s); [Invalid] is returned if that can never be
      the case.
  *)
  val prove_lengths_of_arrays_or_blocks
     : (t -> Targetint.OCaml.Set.t proof) type_accessor

  (* CR mshinwell: rename "prove" -> "check"?
     In fact, it seems the common use case is for a check on "bottom" to be done
     before this function is called.  We should roll that all into one. *)
  val prove_of_kind_value : t -> ty_value

  val prove_of_kind_naked_float
     : t
    -> Numbers.Float_by_bit_pattern.Set.t ty_naked_number

(*
  val prove_closures : (t -> closures proof) type_accessor

  val prove_sets_of_closures
     : (t -> (Name.t option * set_of_closures) proof) type_accessor
*)

  (*
  val prove_closure : (t -> closure proof) type_accessor
  *)

  (** Evidence that a variant or block (with all components of kind [Value])
      may be unboxed. *)
  type unboxable_variant_or_block_of_values0 = private {
    block_sizes_by_tag : Targetint.OCaml.t Tag.Scannable.Map.t;
    constant_ctors : Immediate.Set.t;
  }

  (** Evidence that a given value may be unboxed. *)
  type unboxable_proof = private
    | Variant_or_block_of_values of unboxable_variant_or_block_of_values0
    | Float_array of { length : Targetint.OCaml.t; }
    | Boxed_float
    | Boxed_int32
    | Boxed_int64
    | Boxed_nativeint
    | Cannot_unbox

  (** Prove that a value is of a type that makes the value eligible for
      being unboxed. *)
  val prove_unboxable : (unboxee_ty:t -> unboxable_proof) type_accessor

  (** Given the type of a [Switch] scrutinee and the arms of the [Switch],
      return which cases may be taken together with, for each such case, the
      destination continuation and any env_extension known to hold at the jump
      from the [Switch] to that continuation. *)
  val switch_arms
     : (t
      -> arms:Continuation.t Discriminant.Map.t
      -> (Typing_env_extension.t * Continuation.t) Discriminant.Map.t)
    type_accessor

*)
*)
end
