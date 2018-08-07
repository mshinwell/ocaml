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
  module Both_meet_and_join : sig end
  module Closure_elements : sig type t end  (* CR mshinwell: remove this *)
  module Expr : sig type t end
  module Flambda_types : sig
    type 'a or_alias
    type t
    type ty_value
    type 'a ty_naked_number
    type ty_fabricated
    type 'a ty
    type 'a unknown_or_join
    type of_kind_value
    type blocks_and_tagged_immediates
    type 'a of_kind_value_boxed_number
    type inlinable_function_declaration
    type function_declaration
    type closures_entry
    type closures
    type 'a of_kind_naked_number
    type of_kind_naked_immediate
    type of_kind_naked_float
    type of_kind_naked_int32
    type of_kind_naked_int64
    type of_kind_naked_nativeint
    type of_kind_fabricated
    type set_of_closures_entry
    type set_of_closures
  end
  module Function_type : sig type t end
  module Join_env : sig type t end
  module Meet_env : sig type t end
  module Type_free_names : sig end
  module Type_printers : sig end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  (* CR mshinwell: These aliases are deprecated *)
  type t = Flambda_types.t
  type set_of_closures_entry = Flambda_types.set_of_closures_entry
  type closures_entry = Flambda_types.closures_entry
  type function_declaration = Flambda_types.function_declaration
  type ty_fabricated = Flambda_types.ty_fabricated
  type 'a ty = 'a Flambda_types.ty
  type 'a unknown_or_join = 'a Flambda_types.unknown_or_join

  include Contains_names.S with type t := Flambda_types.t

  module Closures_entry : sig
    type t = Flambda_types.closures_entry

    val bottom : unit -> t

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val add_or_meet_equations
       : t
      -> Meet_env.t
      -> Typing_env_extension.t
      -> t

    val equal : Type_equality_env.t -> t -> t -> bool

    val meet
       : Meet_env.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    val join
       : Join_env.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> t

    include Contains_names.S with type t := t
  end

  module Set_of_closures_entry : sig
    type t = Flambda_types.set_of_closures_entry

    val bottom : unit -> t

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val add_or_meet_equations
       : t
      -> Meet_env.t
      -> Typing_env_extension.t
      -> t

    val equal : Type_equality_env.t -> t -> t -> bool

    val meet
       : Meet_env.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    val join
       : Join_env.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> t

    include Contains_names.S with type t := t
  end

(*

  val alias_type_of : Flambda_kind.t -> Simple.t -> Flambda_types.t

  val any_value_as_ty_value : unit -> Flambda_types.ty_value
  val any_fabricated_as_ty_fabricated : unit -> Flambda_types.ty_fabricated

  val bottom_as_ty_value : unit -> Flambda_types.ty_value
  val bottom_as_ty_fabricated : unit -> Flambda_types.ty_fabricated

  val ty_is_obviously_bottom : 'a Flambda_types.ty -> bool
*)

  val get_alias : Flambda_types.t -> Simple.t option

  val is_obviously_bottom : Flambda_types.t -> bool

  val of_ty_value : Flambda_types.ty_value -> t

  val of_ty_naked_number
    : 'kind Flambda_types.ty_naked_number
    -> 'kind Flambda_kind.Naked_number.t
    -> t

  val of_ty_fabricated : ty_fabricated -> t

  (** Construction of top types. *)
  val unknown : Flambda_kind.t -> t

  val any_value : unit -> t
  val any_value_as_ty_value : unit -> Flambda_types.ty_value

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
    -> Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number

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
  val this_immutable_float_array : Numbers.Float_by_bit_pattern.t array -> t

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
    -> Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number
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
      a value of type [t]. *)
  val this_discriminant_as_ty_fabricated : Discriminant.t -> ty_fabricated

  (** The given block discriminants coupled with the env_extension that hold
      if the corresponding block can be shown to have one of the
      discriminants. *)
  val these_discriminants : Typing_env_extension.t Discriminant.Map.t -> t

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
     : Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number array
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
    -> fields:Flambda_types.ty_value list
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
  val bottom_as_ty_value : unit -> Flambda_types.ty_value

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
    -> Flambda_types.ty_value Var_within_closure.Map.t
    -> set_of_closures:ty_fabricated
    -> t

  (** The type of a closure (of kind [Value]) containing at least one
      closure that holds the given closure variable with the given type. *)
  val closure_containing_at_least
     : Var_within_closure.t
    -> Flambda_types.ty_value
    -> t

  (** The type of a set of closures containing exactly those closure IDs
      with the given types. *)
  val set_of_closures
     : closures:Flambda_types.t Closure_id.Map.t
    -> t

  (** The type of a set of closures containing at least one closure with
      the given closure ID. *)
  val set_of_closures_containing_at_least : Closure_id.t -> t

  (** Construct a type equal to the type of the given name.  (The name
      must be present in the given environment when calling e.g. [join].) *)
  val alias_type_of : Flambda_kind.t -> Simple.t -> t

  (** Like [alias_type_of], but for types of kind [Value], and returns the
      [ty] rather than a [t]. *)
  val alias_type_of_as_ty_value : Simple.t -> Flambda_types.ty_value

  (** Like [alias_type_of_as_ty_value] but for types of [Fabricated] kind. *)
  val alias_type_of_as_ty_fabricated : Simple.t -> ty_fabricated

  (** The type that is equal to another type, found in a .cmx file, named
      by export identifier. *)
  val alias_type : Flambda_kind.t -> Export_id.t -> t

  (** Determine the (unique) kind of a type. *)
  val kind : t -> Flambda_kind.t

  (** Enforce that a type is of kind [Value], returning the corresponding
      [ty]. *)
  val force_to_kind_value : t -> Flambda_types.ty_value

  (** Enforce that a type is of a naked number kind, returning the
      corresponding [ty]. *)
  val force_to_kind_naked_number
     : 'kind Flambda_kind.Naked_number.t
    -> t
    -> 'kind Flambda_types.ty_naked_number

  (** Enforce that a type is of naked float kind, returning the corresponding
      [ty]. *)
  val force_to_kind_naked_float
     : t
    -> Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number

  val force_to_kind_naked_int32
     : Flambda_types.t
    -> Numbers.Int32.Set.t Flambda_types.ty_naked_number

  val force_to_kind_naked_int64
     : Flambda_types.t
    -> Numbers.Int64.Set.t Flambda_types.ty_naked_number

  val force_to_kind_naked_nativeint
     : Flambda_types.t
    -> Targetint.Set.t Flambda_types.ty_naked_number

  val force_to_kind_naked_immediate
     : Flambda_types.t
    -> Immediate.Set.t Flambda_types.ty_naked_number

  (** Enforce that a type is of fabricated kind, returning the corresponding
      [ty]. *)
  val force_to_kind_fabricated
     : Flambda_types.t
    -> Flambda_types.of_kind_fabricated Flambda_types.ty

  (** Enforce that a type is of a given kind. *)
  val check_of_kind : t -> Flambda_kind.t -> unit
end
