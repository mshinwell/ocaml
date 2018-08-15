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
  module Expr : sig
    type t
  end

  type t

  type ty_value
  type 'a ty_naked_number
  type ty_fabricated

  type function_declaration

  module Typing_env : sig
    type t
  end

  module Typing_env_extension : sig
    type t
  end

  module Function_type : sig
    type t
  end

  module Join_env : sig
    type t
  end

  module Meet_env : sig
    type t
  end

  val print : Format.formatter -> t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  (** Greatest lower bound of two types.  The process of meeting may generate
      equations, which are returned as an environment extension. *)
  val meet : Meet_env.t -> t -> t -> t * Typing_env_extension.t

  (** Least upper bound of two types.  This never generates any equations. *)
  val join : Join_env.t  -> t -> t -> t

  (** Like [strictly_more_precise], but also returns [true] when the two
      input types are equally precise. *)
  val as_or_more_precise : Typing_env.t -> t -> than:t -> bool

  (** Returns [true] if the first type is known to provide strictly more
      information about the corresponding value than the type [than]. *)
  val strictly_more_precise : Typing_env.t -> t -> than:t -> bool

  (** Slow type equality. *)
  val equal : t -> t -> bool

  (** Fast type equality---sound but far from complete. *)
  val fast_equal : t -> t -> bool

  val get_alias : t -> Simple.t option

  val is_obviously_bottom : t -> bool

  val of_ty_value : ty_value -> t

  val of_ty_naked_number
     : 'kind ty_naked_number
    -> 'kind Flambda_kind.Naked_number.t
    -> t

  val of_ty_fabricated : ty_fabricated -> t

  (** Construction of top types. *)
  val unknown : Flambda_kind.t -> t

  val any_value : unit -> t
  val any_value_as_ty_value : unit -> ty_value

  val any_fabricated : unit -> t
  val any_fabricated_as_ty_fabricated : unit -> ty_fabricated

  val any_tagged_immediate : unit -> t
  val any_tagged_bool : unit -> t

  val any_boxed_float : unit -> t
  val any_boxed_int32 : unit -> t
  val any_boxed_int64 : unit -> t
  val any_boxed_nativeint : unit -> t

  val any_naked_immediate : unit -> t
  val any_naked_float : unit -> t

  val any_naked_float_as_ty_naked_float
    : unit
   -> Numbers.Float_by_bit_pattern.Set.t ty_naked_number

  (** The top type for unboxed 32-bit numbers. *)
  val any_naked_int32 : unit -> t

  (** The top type for unboxed 64-bit numbers. *)
  val any_naked_int64 : unit -> t

  (** The top type for unboxed "nativeint" numbers. *)
  val any_naked_nativeint : unit -> t

  (** Building of types representing tagged / boxed values from specified
      constants. *)
  val this_tagged_immediate : Immediate.t -> t
  val these_tagged_immediates : Immediate.Set.t -> t
  val this_boxed_float : Numbers.Float_by_bit_pattern.t -> t
  val these_boxed_floats : Numbers.Float_by_bit_pattern.Set.t -> t
  val this_boxed_int32 : Int32.t -> t
  val these_boxed_int32s : Numbers.Int32.Set.t -> t
  val this_boxed_int64 : Int64.t -> t
  val these_boxed_int64s : Numbers.Int64.Set.t -> t
  val this_boxed_nativeint : Targetint.t -> t
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

  (** Building of types representing untagged / unboxed values from
      specified constants. *)
  val this_naked_immediate : Immediate.t -> t
  val this_naked_float : Numbers.Float_by_bit_pattern.t -> t
  val this_naked_float_as_ty_naked_float
     : Numbers.Float_by_bit_pattern.t
    -> Numbers.Float_by_bit_pattern.Set.t ty_naked_number
  val these_naked_floats : Numbers.Float_by_bit_pattern.Set.t -> t
  val this_naked_int32 : Int32.t -> t
  val these_naked_int32s : Numbers.Int32.Set.t -> t
  val this_naked_int64 : Int64.t -> t
  val these_naked_int64s : Numbers.Int64.Set.t -> t
  val this_naked_nativeint : Targetint.t -> t
  val these_naked_nativeints : Targetint.Set.t -> t

  (** Building of types corresponding to immutable values given only the
      size of such values. *)
  val immutable_string : size:Targetint.OCaml.t -> t

  (** The type corresponding to a mutable string of length [size]. *)
  val mutable_string : size:Targetint.OCaml.t -> t

  (** The type corresponding to a mutable float array holding [size]
      naked floats. *)
  val mutable_float_array : size:Targetint.OCaml.t -> t

  (** Building of types corresponding to values that did not exist at
      source level. *)

  (** The given discriminant. *)
  val this_discriminant : Discriminant.t -> t

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

  (** The type of a block with a known tag, size and field types. *)
  val block
     : Tag.t
    -> fields:t list
    -> t

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

  (** Create an "bottom" type with the same kind as the given type. *)
  val bottom_like : t -> t

  (** Create an "unknown" type with the same kind as the given type. *)
  val unknown_like : t -> t

  (** Create a description of a function declaration whose code is known. *)
  val create_inlinable_function_declaration
     : is_classic_mode:bool
    -> closure_origin:Closure_origin.t
    -> continuation_param:Continuation.t
    -> exn_continuation_param:Continuation.t
    -> params:Kinded_parameter.t list
    -> body:Expr.t
    -> code_id:Code_id.t
    -> result_arity:Flambda_arity.t
    -> stub:bool
    -> dbg:Debuginfo.t
    -> inline:Inline_attribute.t
    -> specialise:Specialise_attribute.t
    -> is_a_functor:bool
    -> invariant_params:Variable.Set.t lazy_t
    -> size:int option lazy_t
    -> direct_call_surrogate:Closure_id.t option
    -> my_closure:Variable.t
    -> function_declaration

  (** Create a description of a function declaration whose code is unknown.
      Such declarations cannot be inlined. *)
  val create_non_inlinable_function_declaration
     : unit
    -> function_declaration

  (** Create a closure type given full information about the closure. *)
  val closure
     : Closure_id.t
    -> function_declaration
    -> Function_type.t
    -> ty_value Var_within_closure.Map.t
    -> set_of_closures:ty_fabricated
    -> t

  (** The type of a closure (of kind [Value]) containing at least one
      closure that holds the given closure variable with the given type. *)
  val closure_containing_at_least
     : Var_within_closure.t
    -> ty_value
    -> t

  (** The type of a set of closures containing exactly those closure IDs
      with the given types. *)
  val set_of_closures
     : closures:t Closure_id.Map.t
    -> t

  (** The type of a set of closures containing at least one closure with
      the given closure ID. *)
  val set_of_closures_containing_at_least : Closure_id.t -> t

  (** Construct a type equal to the type of the given name.  (The name
      must be present in the given environment when calling e.g. [join].) *)
  val alias_type_of : Flambda_kind.t -> Simple.t -> t

  (** Like [alias_type_of], but for types of kind [Value], and returns the
      [ty] rather than a [t]. *)
  val alias_type_of_as_ty_value : Simple.t -> ty_value

  (** Like [alias_type_of_as_ty_value] but for types of [Fabricated] kind. *)
  val alias_type_of_as_ty_fabricated : Simple.t -> ty_fabricated

  (** The type that is equal to another type, found in a .cmx file, named
      by export identifier. *)
  val alias_type : Flambda_kind.t -> Export_id.t -> t

  (** Determine the (unique) kind of a type. *)
  val kind : t -> Flambda_kind.t

  (** Enforce that a type is of kind [Value], returning the corresponding
      [ty]. *)
  val force_to_kind_value : t -> ty_value

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

  type 'a type_accessor = Typing_env.t -> 'a

  (** For each of the kinds in an arity, create a "bottom" type. *)
  val bottom_types_from_arity : Flambda_arity.t -> t list

  (** For each of the kinds in an arity, create an "unknown" type. *)
  val unknown_types_from_arity : Flambda_arity.t -> t list

  (** Like [unknown_like] but for a array of types. *)
  val unknown_like_array : t array -> t array

  (* CR mshinwell: rename to [unknown_unit]? *)
  val unit : unit -> t

  val unit_bottom : unit -> t

  (** Whether the given type says that a term of that type can never be
      constructed (in other words, it is [Invalid]). *)
  val is_bottom : (t -> bool) type_accessor

  (** Determine whether the given type provides any information about an
      Flambda term of that type. *)
  val is_known : (t -> bool) type_accessor

  (** Determine whether the given type provides useful information about an
      Flambda term of that type. *)
  val is_useful : (t -> bool) type_accessor

  (** Whether all types in the given list do *not* satisfy [useful]. *)
  val all_not_useful : (t list -> bool) type_accessor

  (** Whether values of the given two types will always be physically equal
      to each other. *)
  val values_physically_equal : t -> t -> bool

  (** Whether values of the given two types will always have a different
      structure from each other. *)
  val values_structurally_distinct
     : (Typing_env.t * t)
    -> (Typing_env.t * t)
    -> bool

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

  type 'a proof = private
    | Proved of 'a
    | Unknown
    | Invalid

  val unknown_proof : unit -> _ proof

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

  type tagged_immediate_as_discriminants_proof = private
    | By_discriminant of Typing_env_extension.t Discriminant.Map.t

  val prove_tagged_immediate_as_discriminants
     : (t -> tagged_immediate_as_discriminants_proof proof) type_accessor

  type is_tagged_immediate = private
    | Never_a_tagged_immediate
    | Always_a_tagged_immediate

  val prove_is_tagged_immediate : (t -> is_tagged_immediate proof) type_accessor

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
end
