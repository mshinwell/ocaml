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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module T : Typing_world_abstract.S
  module Functor_T : Typing_world_abstract.Functor_S

  include Contains_names.S with type t := T.Flambda_types.t

  module Closures_entry : sig
    type t = T.Flambda_types.closures_entry

    val bottom : unit -> t

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val add_or_meet_equations
       : t
      -> T.Meet_env.t
      -> T.Typing_env_extension.t
      -> t

    val equal : Type_equality_env.t -> t -> t -> bool

    val meet
       : T.Meet_env.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> (t * T.Typing_env_extension.t) Or_bottom.t

    val join
       : T.Join_env.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> t

    include Contains_names.S with type t := t
  end

  module Set_of_closures_entry : sig
    type t = T.Flambda_types.set_of_closures_entry

    val bottom : unit -> t

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val add_or_meet_equations
       : t
      -> T.Meet_env.t
      -> T.Typing_env_extension.t
      -> t

    val equal : Type_equality_env.t -> t -> t -> bool

    val meet
       : T.Meet_env.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> (t * T.Typing_env_extension.t) Or_bottom.t

    val join
       : T.Join_env.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> t

    include Contains_names.S with type t := t
  end

(*

  val alias_type_of : Flambda_kind.t -> Simple.t -> T.Flambda_types.t

  val any_value_as_ty_value : unit -> T.Flambda_types.ty_value
  val any_fabricated_as_ty_fabricated : unit -> T.Flambda_types.ty_fabricated

  val bottom_as_ty_value : unit -> T.Flambda_types.ty_value
  val bottom_as_ty_fabricated : unit -> T.Flambda_types.ty_fabricated

  val ty_is_obviously_bottom : 'a T.Flambda_types.ty -> bool
*)

  val get_alias : T.Flambda_types.t -> Simple.t option

  val is_obviously_bottom : T.Flambda_types.t -> bool

  val of_ty_value : T.Flambda_types.ty_value -> T.Flambda_types.t

  val of_ty_naked_number
    : 'kind T.Flambda_types.ty_naked_number
    -> 'kind Flambda_kind.Naked_number.t
    -> T.Flambda_types.t

  val of_ty_fabricated : T.Flambda_types.ty_fabricated -> T.Flambda_types.t

  (** Construction of top types. *)
  val unknown : Flambda_kind.t -> T.Flambda_types.t

  val any_value : unit -> T.Flambda_types.t
  val any_value_as_ty_value : unit -> T.Flambda_types.ty_value

  val any_fabricated : unit -> T.Flambda_types.t
  val any_fabricated_as_ty_fabricated : unit -> T.Flambda_types.ty_fabricated

  val any_tagged_immediate : unit -> T.Flambda_types.t
  val any_tagged_bool : unit -> T.Flambda_types.t

  val any_boxed_float : unit -> T.Flambda_types.t
  val any_boxed_int32 : unit -> T.Flambda_types.t
  val any_boxed_int64 : unit -> T.Flambda_types.t
  val any_boxed_nativeint : unit -> T.Flambda_types.t

  val any_naked_immediate : unit -> T.Flambda_types.t
  val any_naked_float : unit -> T.Flambda_types.t

  val any_naked_float_as_ty_naked_float
    : unit
    -> Numbers.Float_by_bit_pattern.Set.t T.Flambda_types.ty_naked_number

  (** The top type for unboxed 32-bit numbers. *)
  val any_naked_int32 : unit -> T.Flambda_types.t

  (** The top type for unboxed 64-bit numbers. *)
  val any_naked_int64 : unit -> T.Flambda_types.t

  (** The top type for unboxed "nativeint" numbers. *)
  val any_naked_nativeint : unit -> T.Flambda_types.t

  (** Building of types representing tagged / boxed values from specified
      constants. *)
  val this_tagged_immediate : Immediate.t -> T.Flambda_types.t
  val these_tagged_immediates : Immediate.Set.t -> T.Flambda_types.t
  val this_boxed_float : Numbers.Float_by_bit_pattern.t -> T.Flambda_types.t
  val these_boxed_floats : Numbers.Float_by_bit_pattern.Set.t -> T.Flambda_types.t
  val this_boxed_int32 : Int32.t -> T.Flambda_types.t
  val these_boxed_int32s : Numbers.Int32.Set.t -> T.Flambda_types.t
  val this_boxed_int64 : Int64.t -> T.Flambda_types.t
  val these_boxed_int64s : Numbers.Int64.Set.t -> T.Flambda_types.t
  val this_boxed_nativeint : Targetint.t -> T.Flambda_types.t
  val these_boxed_nativeints : Targetint.Set.t -> T.Flambda_types.t
  val this_immutable_string : string -> T.Flambda_types.t
  val this_immutable_float_array
     : Numbers.Float_by_bit_pattern.t array
    -> T.Flambda_types.t

  (** A type representing a set of tagged immediates combined with typing
      judgements that will be used if the set contains, or is subsequently
      refined to contain, only a unique element. *)
  val these_tagged_immediates_with_envs
     : T.Typing_env_extension.t Immediate.Map.t
    -> T.Flambda_types.t

  (** Building of types representing untagged / unboxed values from
      specified constants. *)
  val this_naked_immediate : Immediate.t -> T.Flambda_types.t
  val this_naked_float : Numbers.Float_by_bit_pattern.t -> T.Flambda_types.t
  val this_naked_float_as_ty_naked_float
     : Numbers.Float_by_bit_pattern.t
    -> Numbers.Float_by_bit_pattern.Set.t T.Flambda_types.ty_naked_number
  val these_naked_floats : Numbers.Float_by_bit_pattern.Set.t -> T.Flambda_types.t
  val this_naked_int32 : Int32.t -> T.Flambda_types.t
  val these_naked_int32s : Numbers.Int32.Set.t -> T.Flambda_types.t
  val this_naked_int64 : Int64.t -> T.Flambda_types.t
  val these_naked_int64s : Numbers.Int64.Set.t -> T.Flambda_types.t
  val this_naked_nativeint : Targetint.t -> T.Flambda_types.t
  val these_naked_nativeints : Targetint.Set.t -> T.Flambda_types.t

  (** Building of types corresponding to immutable values given only the
      size of such values. *)
  val immutable_string : size:Targetint.OCaml.t -> T.Flambda_types.t

  (** The type corresponding to a mutable string of length [size]. *)
  val mutable_string : size:Targetint.OCaml.t -> T.Flambda_types.t

  (** The type corresponding to a mutable float array holding [size]
      naked floats. *)
  val mutable_float_array : size:Targetint.OCaml.t -> T.Flambda_types.t

  (** Building of types corresponding to values that did not exist at
      source level. *)

  (** The given discriminant. *)
  val this_discriminant : Discriminant.t -> T.Flambda_types.t

  (** Like [this_discriminant], but returns the [ty_fabricated], rather than
      a type. *)
  val this_discriminant_as_ty_fabricated
     : Discriminant.t
    -> T.Flambda_types.ty_fabricated

  (** The given block discriminants coupled with the env_extensions that hold
      if the corresponding block can be shown to have one of the
      discriminants. *)
  val these_discriminants
     : T.Typing_env_extension.t Discriminant.Map.t
    -> T.Flambda_types.t

  (** Like [these_discriminants], but returns the [ty_fabricated], rather than
      a value of type [t]. *)
  val these_discriminants_as_ty_fabricated
     : T.Typing_env_extension.t Discriminant.Map.t
    -> T.Flambda_types.ty_fabricated

  (** Any discriminant. *)
  val any_discriminant_as_ty_fabricated : unit -> T.Flambda_types.ty_fabricated

  (** Given the type of a naked floating-point number, return the type of the
      corresponding boxed version. *)
  val box_float : T.Flambda_types.t -> T.Flambda_types.t

  (** Given the type of a naked int32 number, return the type of the
      corresponding boxed version. *)
  val box_int32 : T.Flambda_types.t -> T.Flambda_types.t

  (** Given the type of a naked int64 number, return the type of the
      corresponding boxed version. *)
  val box_int64 : T.Flambda_types.t -> T.Flambda_types.t

  (** Given the type of a naked nativeint number, return the type of the
      corresponding boxed version. *)
  val box_nativeint : T.Flambda_types.t -> T.Flambda_types.t

  (** The type of a float array containing the given floating-point numbers. *)
  val immutable_float_array
     : Numbers.Float_by_bit_pattern.Set.t T.Flambda_types.ty_naked_number array
    -> T.Flambda_types.t

  (** The type of a block with a known tag, size and field types. *)
  val block
     : Tag.t
    -> fields:T.Flambda_types.t list
    -> T.Flambda_types.t

  (** Like [block], except that the field types are statically known to be
      of kind [Value]). *)
  val block_of_values
     : Tag.t
    -> fields:T.Flambda_types.ty_value list
    -> T.Flambda_types.t

  (** The type of a block with a known tag and size but unknown content,
      save that the contents are all of kind [Value]. *)
  val block_of_unknown_values
     : Tag.t
    -> size:int
    -> T.Flambda_types.t

  (** The type of a block with at least [n] fields and an unknown tag.
      The type of the [n - 1]th field is taken to be an [Equals] to the
      given variable. *)
  val block_with_size_at_least
     : n:int
    -> field_n_minus_one:Variable.t
    -> T.Flambda_types.t

  (** The bottom type for the given kind ("no value can flow to this point"). *)
  val bottom : Flambda_kind.t -> T.Flambda_types.t

  (** The bottom type for kind [Value] expressed as a type whose kind is
      statically known. *)
  val bottom_as_ty_value : unit -> T.Flambda_types.ty_value

  (** The bottom type for kind [Fabricated] expressed as a type whose kind is
      statically known. *)
  val bottom_as_ty_fabricated : unit -> T.Flambda_types.ty_fabricated

  (** Create an "bottom" type with the same kind as the given type. *)
  val bottom_like : T.Flambda_types.t -> T.Flambda_types.t

  (** Create an "unknown" type with the same kind as the given type. *)
  val unknown_like : T.Flambda_types.t -> T.Flambda_types.t

  (** Create a description of a function declaration whose code is known. *)
  val create_inlinable_function_declaration
     : is_classic_mode:bool
    -> closure_origin:Closure_origin.t
    -> continuation_param:Continuation.t
    -> exn_continuation_param:Continuation.t
    -> params:Kinded_parameter.t list
    -> body:T.Expr.t
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
    -> T.Flambda_types.function_declaration

  (** Create a description of a function declaration whose code is unknown.
      Such declarations cannot be inlined. *)
  val create_non_inlinable_function_declaration
     : unit
    -> T.Flambda_types.function_declaration

  (** Create a closure type given full information about the closure. *)
  val closure
     : Closure_id.t
    -> T.Flambda_types.function_declaration
    -> T.Function_type.t
    -> T.Flambda_types.ty_value Var_within_closure.Map.t
    -> set_of_closures:T.Flambda_types.ty_fabricated
    -> T.Flambda_types.t

  (** The type of a closure (of kind [Value]) containing at least one
      closure that holds the given closure variable with the given type. *)
  val closure_containing_at_least
     : Var_within_closure.t
    -> T.Flambda_types.ty_value
    -> T.Flambda_types.t

  (** The type of a set of closures containing exactly those closure IDs
      with the given types. *)
  val set_of_closures
     : closures:T.Flambda_types.t Closure_id.Map.t
    -> T.Flambda_types.t

  (** The type of a set of closures containing at least one closure with
      the given closure ID. *)
  val set_of_closures_containing_at_least : Closure_id.t -> T.Flambda_types.t

  (** Construct a type equal to the type of the given name.  (The name
      must be present in the given environment when calling e.g. [join].) *)
  val alias_type_of : Flambda_kind.t -> Simple.t -> T.Flambda_types.t

  (** Like [alias_type_of], but for types of kind [Value], and returns the
      [ty] rather than a [t]. *)
  val alias_type_of_as_ty_value : Simple.t -> T.Flambda_types.ty_value

  (** Like [alias_type_of_as_ty_value] but for types of [Fabricated] kind. *)
  val alias_type_of_as_ty_fabricated : Simple.t -> T.Flambda_types.ty_fabricated

  (** The type that is equal to another type, found in a .cmx file, named
      by export identifier. *)
  val alias_type : Flambda_kind.t -> Export_id.t -> T.Flambda_types.t

  (** Determine the (unique) kind of a type. *)
  val kind : T.Flambda_types.t -> Flambda_kind.t

  (** Enforce that a type is of kind [Value], returning the corresponding
      [ty]. *)
  val force_to_kind_value : T.Flambda_types.t -> T.Flambda_types.ty_value

  (** Enforce that a type is of a naked number kind, returning the
      corresponding [ty]. *)
  val force_to_kind_naked_number
     : 'kind Flambda_kind.Naked_number.t
    -> T.Flambda_types.t
    -> 'kind T.Flambda_types.ty_naked_number

  (** Enforce that a type is of naked float kind, returning the corresponding
      [ty]. *)
  val force_to_kind_naked_float
     : T.Flambda_types.t
    -> Numbers.Float_by_bit_pattern.Set.t T.Flambda_types.ty_naked_number

  val force_to_kind_naked_int32
     : T.Flambda_types.t
    -> Numbers.Int32.Set.t T.Flambda_types.ty_naked_number

  val force_to_kind_naked_int64
     : T.Flambda_types.t
    -> Numbers.Int64.Set.t T.Flambda_types.ty_naked_number

  val force_to_kind_naked_nativeint
     : T.Flambda_types.t
    -> Targetint.Set.t T.Flambda_types.ty_naked_number

  val force_to_kind_naked_immediate
     : T.Flambda_types.t
    -> Immediate.Set.t T.Flambda_types.ty_naked_number

  (** Enforce that a type is of fabricated kind, returning the corresponding
      [ty]. *)
  val force_to_kind_fabricated
     : T.Flambda_types.t
    -> T.Flambda_types.of_kind_fabricated T.Flambda_types.ty

  (** Enforce that a type is of a given kind. *)
  val check_of_kind : T.Flambda_types.t -> Flambda_kind.t -> unit
end
