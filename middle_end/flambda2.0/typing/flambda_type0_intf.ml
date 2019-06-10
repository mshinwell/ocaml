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

  val print : Format.formatter -> t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val invariant : Invariant_env.t -> t -> unit

  val arity_of_list : t list -> Flambda_arity.t

  val erase_aliases : allowed:Variable.Set.t -> t -> t

  type typing_env
  type typing_env_extension

  module Typing_env_extension : sig
    type t = typing_env_extension

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val print : Format.formatter -> t -> unit

    val invariant : t -> unit

    val empty : t

    val one_equation : Name.t -> flambda_type -> t

    val mem : t -> Name.t -> bool
  end

  module Typing_env : sig
    type t = typing_env

    val invariant : t -> unit

    val print : Format.formatter -> t -> unit

    val create : resolver:(Export_id.t -> flambda_type option) -> t

    val create_using_resolver_from : t -> t

    val create_using_resolver_and_symbol_bindings_from : t -> t

    val resolver : t -> (Export_id.t -> flambda_type option)

    val is_empty : t -> bool

    val current_scope : t -> Scope.t

    val increment_scope : t -> t

    val increment_scope_to : t -> Scope.t -> t

    val domain : t -> Name_occurrences.t

    val var_domain : t -> Variable.Set.t

    val add_definition : t -> Name.t -> Flambda_kind.t -> t

    val add_equation : t -> Name.t -> flambda_type -> t

    val find : t -> Name.t -> flambda_type

    val mem : t -> Name.t -> bool

    val add_env_extension
       : t
      -> Typing_env_extension.t
      -> t

    val get_canonical_name : t -> Name.t -> Name.t

    val aliases_of_simple : t -> Simple.t -> Name.Set.t

    val cut
       : t
      -> unknown_if_defined_at_or_later_than:Scope.t
      -> Typing_env_extension.t * Variable.Set.t
  end

  val meet_shape
     : Typing_env.t
    -> flambda_type
    -> shape:flambda_type
    -> result_var:Variable.t
    -> result_kind:Flambda_kind.t
    -> Typing_env_extension.t Or_bottom.t

  val join : Typing_env.t -> t -> t -> t

  (* CR mshinwell: Substitute out this alias once it's finalised *)
  type 'a type_accessor = Typing_env.t -> 'a

  type 'a ty
  type 'a unknown_or_join

  type ty_value
  type 'a ty_naked_number
  type ty_fabricated

  (* CR mshinwell: The function declaration types should probably be abstract *)

  type inlinable_function_declaration = private {
    function_decl : term_language_function_declaration;
  }

  type function_declaration = private
    | Non_inlinable of {
        param_arity : Flambda_arity.t;
        result_arity : Flambda_arity.t;
      }
    | Inlinable of inlinable_function_declaration

  val erase_aliases_ty_value
     : allowed:Variable.Set.t
    -> ty_value
    -> ty_value

  val bottom : Flambda_kind.t -> t

  (** Construction of top types. *)
  val unknown : Flambda_kind.t -> t

  val unknown_as_ty_fabricated : unit -> ty_fabricated

  val any_value : unit -> t

  val any_fabricated : unit -> t

  val any_tagged_immediate : unit -> t
  val any_tagged_bool : unit -> t

  val any_boxed_float : unit -> t
  val any_boxed_int32 : unit -> t
  val any_boxed_int64 : unit -> t
  val any_boxed_nativeint : unit -> t

  val any_naked_immediate : unit -> t
  val any_naked_float : unit -> t

  (** Building of types representing tagged / boxed values from specified
      constants. *)
  val this_tagged_immediate : Immediate.t -> t
  val this_boxed_float : Numbers.Float_by_bit_pattern.t -> t
  val this_boxed_int32 : Int32.t -> t
  val this_boxed_int64 : Int64.t -> t
  val this_boxed_nativeint : Targetint.t -> t

  (** Building of types representing untagged / unboxed values from
      specified constants. *)
  val this_naked_immediate : Immediate.t -> t
  val this_naked_float : Numbers.Float_by_bit_pattern.t -> t
  val this_naked_int32 : Int32.t -> t
  val this_naked_int64 : Int64.t -> t
  val this_naked_nativeint : Targetint.t -> t

  (** Building of types corresponding to values that did not exist at
      source level. *)

  (** The given discriminant. *)
  val this_discriminant : Discriminant.t -> t

  (* CR mshinwell: decide on exact strategy for mutable blocks *)

  (** The type of an immutable block with a known tag, size and field types. *)
  val immutable_block
     : Tag.t
    -> fields:t list
    -> t

  (** Like [immutable_block], except that the field types are statically
      known to be of kind [Value]). *)
  val immutable_block_of_values
     : Tag.t
    -> fields:ty_value list
    -> t

  (** The type of an immutable block with at least [n] fields and an unknown
      tag.

      The type of the [n - 1]th field is taken to be an [Equals] to the
      given variable. *)
  (* CR mshinwell: Should add "kind" argument?  Implementation assumes
     "value" at the moment *)
  val immutable_block_with_size_at_least
     : n:Targetint.OCaml.t
    -> field_n_minus_one:Variable.t
    -> t

  (** Create an "bottom" type with the same kind as the given type. *)
  val bottom_like : t -> t

  (** Create an "unknown" type with the same kind as the given type. *)
  val unknown_like : t -> t

  (** Create a description of a function declaration whose code is known. *)
  val create_inlinable_function_declaration
     : term_language_function_declaration
    -> function_declaration

  (** Create a description of a function declaration whose code is unknown.
      Such declarations cannot be inlined, but can be direct called. *)
  val create_non_inlinable_function_declaration
     : param_arity:Flambda_arity.t
    -> result_arity:Flambda_arity.t
    -> function_declaration

  (** Create a closure type given full information about the closure. *)
  val closure
     : Closure_id.t
    -> function_declaration
    -> ty_value Var_within_closure.Map.t
    -> set_of_closures:ty_fabricated
    -> t

  (** The type of a closure (of kind [Value]) containing at least one
      closure that holds the given closure variable with the given type. *)
  val closure_containing_at_least
     : Var_within_closure.t
    -> closure_element_var:Variable.t
    -> flambda_type

  (** The type of a set of closures containing exactly those closure IDs
      with the given types. *)
  val set_of_closures
     : closures:t Closure_id.Map.t
    -> t

  (** The type of a set of closures containing at least one closure,
      whose type should be "Equals closure_var", with the given closure ID. *)
  val set_of_closures_containing_at_least
     : Closure_id.t
    -> closure_var:Variable.t
    -> flambda_type

  (** Construct a type equal to the type of the given name.  (The name
      must be present in the given environment when calling e.g. [join].) *)
  val alias_type_of : Flambda_kind.t -> Simple.t -> t

  val alias_type_of_as_ty_value : Simple.t -> ty_value

  (** Like [alias_type_of_as_ty_value] but for types of [Fabricated] kind. *)
  val alias_type_of_as_ty_fabricated : Simple.t -> ty_fabricated

  (** Determine the (unique) kind of a type. *)
  val kind : t -> Flambda_kind.t

  (** Enforce that a type is of kind [Value], returning the corresponding
      [ty]. *)
  val force_to_kind_value : t -> ty_value

  (** For each of the kinds in an arity, create an "unknown" type. *)
  val unknown_types_from_arity : Flambda_arity.t -> t list

  (** Whether the given type says that a term of that type can never be
      constructed (in other words, it is [Invalid]). *)
  val is_bottom : (t -> bool) type_accessor

  val type_for_const : Simple.Const.t -> t

  type to_lift =
    | Immutable_block of Tag.Scannable.t * (Symbol.t list)
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
  *)
  val reify
     : (allow_free_variables:bool
    -> t
    -> reification_result) type_accessor

  type 'a proof = private
    | Proved of 'a
    | Unknown
    | Invalid

  (** Prove that the given type, of kind [Value], is a closures type
      describing exactly one closure.  The function declaration corresponding
      to such closure is returned together with its closure ID, if it is
      known. *)
  val prove_single_closures_entry
     : Typing_env.t
    -> t
    -> (Closure_id.t * function_declaration Or_unknown.t) proof
end
