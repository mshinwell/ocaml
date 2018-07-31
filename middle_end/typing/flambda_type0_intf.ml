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

module type S = sig
  module Expr : sig
    type t
  end

  module rec T : sig
    type 'a or_alias = private
      | No_alias of 'a
      | Type of Export_id.t
      | Equals of Simple.t

    type t = private
      | Value of ty_value
      | Naked_number :
          'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> descr
      | Fabricated of ty_fabricated

    and ty_value = of_kind_value ty
    and 'a ty_naked_number = 'a of_kind_naked_number ty
    and ty_fabricated = of_kind_fabricated ty

    and 'a ty = 'a unknown_or_join or_alias

    (** For each kind there is a lattice of types. *)
    and 'a unknown_or_join = private
      | Unknown
      (** "Any value can flow to this point": the top element. *)
      | Join of ('a * Name_permutation.t) list
      (** - The list being empty means bottom, the least element: "no value can
            flow to this point".
          - The list containing a single element is the usual case where there
            is no join between incompatible types.
          - If the list contains more than one element:
            A join, between incompatible types, which has been remembered
            in case it is refined by a subsequent meet.  Joins between
            compatible types are immediately pushed down through the top level
            structure of the type.

          The [Name_permutation.t] is a delayed permutation which must be
          pushed down through the structure of the type as it is examined.

          Invariant: every member of a [Join] is incompatible with the other
          members. *)

    and of_kind_value = private
      | Blocks_and_tagged_immediates of blocks_and_tagged_immediates
      | Boxed_number : _ of_kind_value_boxed_number -> of_kind_value
      | Closures of closures
      | String of String_info.Set.t

    and blocks_and_tagged_immediates = private {
      immediates : T.Immediates.t;
      (** Cases for constant constructors (in the case of variants) and
          arbitrary tagged immediates. *)
      blocks : T.Blocks.t;
      (** Cases for non-constant constructors (in the case of variants) and
          normal blocks. *)
    }

    (** Boxed integer and floating-point numbers together with the types
        of their contents. *)
    and 'a of_kind_value_boxed_number = private
      | Boxed_float
          : Float.Set.t ty_naked_number
          -> Float.Set.t ty_naked_number of_kind_value_boxed_number
      | Boxed_int32
          : Int32.Set.t ty_naked_number
          -> Int32.Set.t ty_naked_number of_kind_value_boxed_number
      | Boxed_int64
          : Int64.Set.t ty_naked_number
          -> Int64.Set.t ty_naked_number of_kind_value_boxed_number
      | Boxed_nativeint
          : Targetint.Set.t ty_naked_number
          -> Targetint.Set.t ty_naked_number of_kind_value_boxed_number

    (** A function declaration which is inlinable (which in particular implies
        that the code of the function's body is known).  Such declarations are
        completely closed entities in terms of names. *)
    and inlinable_function_declaration = private {
      closure_origin : Closure_origin.t;
      continuation_param : Continuation.t;
      exn_continuation_param : Continuation.t;
      is_classic_mode : bool;
      (** Whether the file from which this function declaration originated was
          compiled in classic mode. *)
      params : Kinded_parameter.t list;
      (** The parameters of the function, in order. *)
      body : Expr.t;
      code_id : Code_id.t;
      free_names_in_body : Name_occurrences.t;
      stub : bool;
      result_arity : Flambda_arity.t;
      dbg : Debuginfo.t;
      inline : Inline_attribute.t;
      specialise : Specialise_attribute.t;
      is_a_functor : bool;
      invariant_params : Variable.Set.t lazy_t;
      size : int option lazy_t;
      (** For functions that are very likely to be inlined, the size of the
          function's body. *)
      direct_call_surrogate : Closure_id.t option;
      my_closure : Variable.t;
    }

    and function_declaration = private
      | Non_inlinable
      | Inlinable of inlinable_function_declaration

    and closures_entry = private {
      function_decl : function_declaration;
      (** Information from the term language about the function declaration
          associated with the closure (call it [C]) described by a
          [closures_entry]. *)
      ty : T.Function_type.t;
      (** The type of the function associated with [C].
          Note: function parameter types are covariant! *)
      closure_elements : T.Closure_elements.t;
      (** Relational product describing the variables within a closure and
          equations between them. *)
      set_of_closures : ty_fabricated;
      (** Link back to the type of the set of closures containing [C]. *)
    }

    and closures = private {
      by_closure_id : T.Closures_entry_by_closure_id.t;
      (** Row-like structure that selects [closures_entry] structures based
          on closure ID and the set of variables in the closure. *)
    }

    (** Unboxed ("naked") integer and floating-point numbers together with
        any information known about which particular numbers they might be. *)
    and 'a of_kind_naked_number = private
      | Immediate : Immediate.Set.t -> Immediate.Set.t of_kind_naked_number
      | Float : Float.Set.t -> Float.Set.t of_kind_naked_number
      | Int32 : Int32.Set.t -> Int32.Set.t of_kind_naked_number
      | Int64 : Int64.Set.t -> Int64.Set.t of_kind_naked_number
      | Nativeint : Targetint.Set.t -> Targetint.Set.t of_kind_naked_number

    and of_kind_naked_immediate = Immediate.Set.t of_kind_naked_number
    and of_kind_naked_float = Float.Set.t of_kind_naked_number
    and of_kind_naked_int32 = Int32.Set.t of_kind_naked_number
    and of_kind_naked_int64 = Int64.Set.t of_kind_naked_number
    and of_kind_naked_nativeint = Targetint.Set.t of_kind_naked_number

    and of_kind_fabricated = private
      | Discriminants of T.Discriminants.t
        (** A discriminant is either:
            - a block tag, as returned by the [Get_tag] primitive; or
            - a constant constructor which has undergone a kind-cast to kind
              [Fabricated] using the [Discriminant_of_int] primitive. *)
      | Set_of_closures of set_of_closures
        (** A possibly mutually-recursive collection of closure values, which
            at runtime will be represented by a single block. *)

    and set_of_closures_entry = private {
      by_closure_id : T.Types_by_closure_id.t;
      (** Relational product, indexed by individual closure IDs, that (via
          logical variables) describes the makeup of a set of closures. *)
    }

    and set_of_closures = private {
      closures : T.Closure_ids.t;
      (** Row-like structure that maps _sets_ of [Closure_id.t]s to
          [set_of_closures_entry] structures. *)
    }

    type typing_environment_entry0 =
      | Definition of t
      | Equation of t

    type typing_environment_entry =
      (* CR mshinwell: Consider removing "of t" for [Definition] (and maybe
        change it to [Introduce_name] -- the "t" would be implicitly bottom) *)
      | Definition of t
      | Equation of t
      | CSE of Flambda_primitive.With_fixed_value.t
  end
  and Typing_env : (Typing_env_intf.S with module T := T)
  and Typing_env_extension : (Typing_env_extension_intf.S with module T := T)
  and Join_env : (Join_env_intf.S with module T := T)
  (* ... [should be Typing_world.S pretty much] *)

  include module type of struct include T end

  (** Annotation for functions that may require examination of the current
      environment (in particular to resolve [Type] or [Equals] aliases). *)
  type 'a type_accessor = Typing_env.t -> 'a

  (** Print a type to the given formatter. *)
  val print : Format.formatter -> t -> unit

  (** Like [print] except uses a cache to prevent repeated printing of
      shared sub-components. *)
  val print_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit

  val print_ty_value_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> ty_value
    -> unit

  val print_ty_value_array
     : cache:Printing_cache.t
    -> Format.formatter
    -> ty_value array
    -> unit

  val print_inlinable_function_declaration
     : Format.formatter
    -> inlinable_function_declaration
    -> unit

  (** Type equality---sound, but should not be assumed to be complete. *)
  val equal : t -> t -> bool

  (** Fast type equality---sound but far from complete. *)
  val fast_equal : t -> t -> bool

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
     : Numbers.Float_by_bit_pattern.Set.t ty_naked_number array
    -> t

  (** The type of a block with a known tag, size and field types. *)
  val block
     : Tag.t
    -> fields:t mutable_or_immutable array
    -> t

  (** Like [block], except that the field types are statically known to be
      of kind [Value]). *)
  val block_of_values
     : Tag.t
    -> fields:ty_value mutable_or_immutable array
    -> t

  (** The type of a block with a known tag and size but unknown content,
      save that the contents are all of kind [Value]. *)
  val block_of_unknown_values
     : Tag.t
    -> size:int
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
      The lack of knowledge about the code will prevent inlining. *)
  val create_non_inlinable_function_declaration
     : unit
    -> function_declaration

  (** Create a type of kind [Fabricated] describing a closure in terms of
      either non-inlinable or inlinable function declarations.  Note that
      this concept is different from that of closure types of kind [Value]
      (see [closures], below). *)
  val closure : function_declarations -> ty_fabricated

  (** Create a type of kind [Fabricated] describing the given set(s) of
      closures. *)
  val set_of_closures
     : closures:ty_fabricated Closure_id.Map.t extensibility
    -> closure_elements:ty_value Var_within_closure.Map.t extensibility
    -> t

  (** Used to create the data in the map required for the [closures]
      function. *)
  val closures_entry : set_of_closures:ty_fabricated -> closures_entry

  (** Create a type of kind [Value] corresponding to one or more closures. *)
  val closures
     : dependent_function_type
    -> closures_entry Closure_id.Map.t
    -> t

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

  (** Free names in a type. *)
  val free_names : t -> Name_occurrences.t

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

  (** Enforce that a type is of fabricated kind, returning the corresponding
      [ty]. *)
  val force_to_kind_fabricated : t -> ty_fabricated

  (** Enforce that a type is of a given kind. *)
  val check_of_kind : t -> Flambda_kind.t -> unit

  (** Greatest lower bound of two types.  The process of meeting may generate
      equations, which are returned as an environment extension. *)
  val meet : Typing_env.t -> t -> t -> t * Typing_env_extension.t

  (** Least upper bound of two types.  This never generates any equations. *)
  val join : Join_env.t  -> t -> t -> t

  (** Like [strictly_more_precise], but also returns [true] when the two
      input types are equally precise. *)
  val as_or_more_precise : Typing_env.t -> t -> than:t -> bool

  (** Returns [true] if the first type is known to provide strictly more
      information about the corresponding value than the type [than]. *)
  val strictly_more_precise : Typing_env.t -> t -> than:t -> bool

  (** Application of name permutations to types runs in constant time. *)
  include Contains_names.S with type t := t
end
