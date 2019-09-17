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

(** The interface to the type system once instantiated for a particular
    expression language (typically [Flambda]). *)

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

  type typing_env
  type typing_env_extension

  module Typing_env_extension : sig
    type t = typing_env_extension

    val print : Format.formatter -> t -> unit

    val invariant : t -> unit

    val empty : unit -> t

    val one_equation : Name.t -> flambda_type -> t
  end

  module Typing_env : sig
    type t = typing_env

    val invariant : t -> unit

    val print : Format.formatter -> t -> unit

    val create : resolver:(Export_id.t -> flambda_type option) -> t

    val create_using_resolver_and_symbol_bindings_from : t -> t

    val resolver : t -> (Export_id.t -> flambda_type option)

    val var_domain : t -> Variable.Set.t

    val current_scope : t -> Scope.t

    val increment_scope_to : t -> Scope.t -> t

    val add_definition : t -> Name_in_binding_pos.t -> Flambda_kind.t -> t

    val add_equation : t -> Name.t -> flambda_type -> t

    val add_cse
       : t
      -> Flambda_primitive.Eligible_for_cse.t
      -> bound_to:Simple.t
      -> t

    val mem : t -> Name.t -> bool

    val find : t -> Name.t -> flambda_type

    val find_cse : t -> Flambda_primitive.Eligible_for_cse.t -> Simple.t option

    val find_cse_rev
       : t
      -> bound_to:Simple.t
      -> Flambda_primitive.Eligible_for_cse.t option

    val add_env_extension
       : t
      -> Typing_env_extension.t
      -> t

    val get_canonical_simple
       : t
      -> ?min_occurrence_kind:Name_occurrence_kind.t
      -> Simple.t
      -> Simple.t option Or_bottom.t

    val get_canonical_simple_with_kind
       : t
      -> ?min_occurrence_kind:Name_occurrence_kind.t
      -> Simple.t
      -> Simple.t option Or_bottom.t * Flambda_kind.t

    val cut_and_n_way_join
       : t
      -> (t * Apply_cont_rewrite_id.t * Variable.Set.t) list
      -> unknown_if_defined_at_or_later_than:Scope.t
      -> Typing_env_extension.t * Continuation_extra_params_and_args.t
  end

  val meet : Typing_env.t -> t -> t -> (t * Typing_env_extension.t) Or_bottom.t

  val meet_shape
     : Typing_env.t
    -> t
    -> shape:t
    -> result_var:Var_in_binding_pos.t
    -> result_kind:Flambda_kind.t
    -> Typing_env_extension.t Or_bottom.t

  val join : Typing_env.t -> t -> t -> t

  (* CR mshinwell: Substitute out this alias once it's finalised *)
  type 'a type_accessor = Typing_env.t -> 'a

  (* CR mshinwell: The function declaration types should be abstract *)
  module Function_declaration_type : sig
    type inlinable = private {
      function_decl : term_language_function_declaration;
      rec_info : Rec_info.t;
    }

    type t = private
      | Non_inlinable of {
          param_arity : Flambda_arity.t;
          result_arity : Flambda_arity.t;
          recursive : Recursive.t;
        }
      | Inlinable of inlinable
  end

  (** This function takes a type [t] and an environment [env] that assigns types
      to all the free names of [t].  It also takes an environment, called
      [suitable_for], in which we would like to use [t].  The function
      identifies which free names (if any) of [t] would be unbound in
      [suitable_for].  For each such name a fresh variable is assigned and
      irrelevantly bound in [suitable_for]; the returned type is like [t]
      except that the names that would otherwise be unbound are replaced by
      these fresh variables.  The fresh variables are assigned types in the
      returned environment extension on a best effort basis. *)
  val make_suitable_for_environment
     : t
    -> Typing_env.t
    -> suitable_for:Typing_env.t
    -> Typing_env_extension.t * t

  val apply_rec_info : flambda_type -> Rec_info.t -> flambda_type Or_bottom.t

  (** Construct a bottom type of the given kind. *)
  val bottom : Flambda_kind.t -> t

  (** Construct a top ("unknown") type of the given kind. *)
  val unknown : Flambda_kind.t -> t

  (** Create an bottom type with the same kind as the given type. *)
  val bottom_like : t -> t

  (** Create an "unknown" type with the same kind as the given type. *)
  val unknown_like : t -> t

  val any_value : t

  val any_fabricated : t

  val any_tagged_immediate : t
  val any_tagged_bool : t

  val any_boxed_float : t
  val any_boxed_int32 : t
  val any_boxed_int64 : t
  val any_boxed_nativeint : t

  val any_naked_immediate : t
  val any_naked_float : t

  (** Building of types representing tagged / boxed values from specified
      constants. *)
  val this_tagged_immediate : Immediate.t -> t
  val this_boxed_float : Numbers.Float_by_bit_pattern.t -> t
  val this_boxed_int32 : Int32.t -> t
  val this_boxed_int64 : Int64.t -> t
  val this_boxed_nativeint : Targetint.t -> t

  val these_tagged_immediates : Immediate.Set.t -> t
  val these_boxed_floats : Numbers.Float_by_bit_pattern.Set.t -> t
  val these_boxed_int32s : Int32.Set.t -> t
  val these_boxed_int64s : Int64.Set.t -> t
  val these_boxed_nativeints : Targetint.Set.t -> t

  (** Building of types representing untagged / unboxed values from
      specified constants. *)
  val this_naked_immediate : Immediate.t -> t
  val this_naked_float : Numbers.Float_by_bit_pattern.t -> t
  val this_naked_int32 : Int32.t -> t
  val this_naked_int64 : Int64.t -> t
  val this_naked_nativeint : Targetint.t -> t

  val these_naked_immediates : Immediate.Set.t -> t
  val these_naked_floats : Numbers.Float_by_bit_pattern.Set.t -> t
  val these_naked_int32s : Int32.Set.t -> t
  val these_naked_int64s : Int64.Set.t -> t
  val these_naked_nativeints : Targetint.Set.t -> t

  val boxed_float_alias_to : naked_float:Variable.t -> t
  val boxed_int32_alias_to : naked_int32:Variable.t -> t
  val boxed_int64_alias_to : naked_int64:Variable.t -> t
  val boxed_nativeint_alias_to : naked_nativeint:Variable.t -> t

  val box_float : t -> t
  val box_int32 : t -> t
  val box_int64 : t -> t
  val box_nativeint : t -> t

  (** Building of types corresponding to values that did not exist at
      source level. *)

  (** The given discriminant. *)
  val this_discriminant : Discriminant.t -> t

  val these_discriminants : Discriminant.Set.t -> t

  (* CR mshinwell: decide on exact strategy for mutable blocks *)

  (** The type of an immutable block with a known tag, size and field types. *)
  val immutable_block : Tag.t -> fields:t list -> t

  (** The type of an immutable block with at least [n] fields and an unknown
      tag. The type of the [n - 1]th field is taken to be an [Equals] to the
      given variable. *)
  (* CR mshinwell: Should add "kind" argument?  Implementation assumes
     "value" at the moment *)
  val immutable_block_with_size_at_least
     : n:Targetint.OCaml.t
    -> field_n_minus_one:Variable.t
    -> t

  val this_immutable_string : string -> t

  val mutable_string : size:int -> t

  (** Create a description of a function declaration whose code is known. *)
  val create_inlinable_function_declaration
     : term_language_function_declaration
    -> Rec_info.t
    -> Function_declaration_type.t

  (** Create a description of a function declaration whose code is unknown.
      Such declarations cannot be inlined, but can be direct called. *)
  val create_non_inlinable_function_declaration
     : param_arity:Flambda_arity.t
    -> result_arity:Flambda_arity.t
    -> recursive:Recursive.t
    -> Function_declaration_type.t

  val exactly_this_closure
     : Closure_id.t
    -> all_function_decls_in_set:Function_declaration_type.t Closure_id.Map.t
    -> all_closures_in_set:t Closure_id.Map.t
    -> all_closure_vars_in_set:flambda_type Var_within_closure.Map.t
    -> flambda_type

  val at_least_the_closures_with_ids
     : this_closure:Closure_id.t
    -> Simple.t Closure_id.Map.t
    -> flambda_type

  val closure_with_at_least_this_closure_var
     : Var_within_closure.t
    -> closure_element_var:Variable.t
    -> flambda_type

  val array_of_length : length:flambda_type -> flambda_type

  (** Construct a type equal to the type of the given name.  (The name
      must be present in the given environment when calling e.g. [join].) *)
  val alias_type_of : Flambda_kind.t -> Simple.t -> t

  (** Determine the (unique) kind of a type. *)
  val kind : t -> Flambda_kind.t

  (** For each of the kinds in an arity, create an "unknown" type. *)
  val unknown_types_from_arity : Flambda_arity.t -> t list

  (** Whether the given type says that a term of that type can never be
      constructed (in other words, it is [Invalid]). *)
  val is_bottom : (t -> bool) type_accessor

  val type_for_const : Simple.Const.t -> t
  val kind_for_const : Simple.Const.t -> Flambda_kind.t

  type 'a proof = private
    | Proved of 'a
    | Unknown
    | Invalid

  type 'a proof_allowing_kind_mismatch = private
    | Proved of 'a
    | Unknown
    | Invalid
    | Wrong_kind

  (* CR mshinwell: Should remove "_equals_" from these names *)
  val prove_equals_tagged_immediates
     : Typing_env.t
    -> t
    -> Immediate.Set.t proof

  val prove_equals_single_tagged_immediate
     : Typing_env.t
    -> t
    -> Immediate.t proof

  val prove_equals_discriminants
     : Typing_env.t
    -> t
    -> Discriminant.Set.t proof

  val prove_naked_floats
     : Typing_env.t
    -> t
    -> Numbers.Float_by_bit_pattern.Set.t proof

  val prove_naked_int32s : Typing_env.t -> t -> Numbers.Int32.Set.t proof

  val prove_naked_int64s : Typing_env.t -> t -> Numbers.Int64.Set.t proof

  val prove_naked_nativeints : Typing_env.t -> t -> Targetint.Set.t proof

  val prove_is_a_boxed_float
     : Typing_env.t
    -> t
    -> unit proof_allowing_kind_mismatch

  val prove_is_a_boxed_int32
     : Typing_env.t
    -> t
    -> unit proof_allowing_kind_mismatch

  val prove_is_a_boxed_int64
     : Typing_env.t
    -> t
    -> unit proof_allowing_kind_mismatch

  val prove_is_a_boxed_nativeint
     : Typing_env.t
    -> t
    -> unit proof_allowing_kind_mismatch

  val prove_boxed_floats : Typing_env.t -> t -> Float.Set.t proof
  val prove_boxed_int32s : Typing_env.t -> t -> Numbers.Int32.Set.t proof
  val prove_boxed_int64s : Typing_env.t -> t -> Numbers.Int64.Set.t proof
  val prove_boxed_nativeints : Typing_env.t -> t -> Targetint.Set.t proof

  val prove_tags_and_sizes
     : Typing_env.t
    -> t
    -> Targetint.OCaml.t Tag.Map.t proof

  val prove_unique_tag_and_size
     : Typing_env.t
    -> t
    -> (Tag.t * Targetint.OCaml.t) proof_allowing_kind_mismatch

  val prove_is_int : Typing_env.t -> t -> bool proof

  (** Prove that the given type, of kind [Value], is a closures type
      describing exactly one closure.  The function declaration corresponding
      to such closure is returned together with its closure ID, if it is
      known. *)
  val prove_single_closures_entry
     : Typing_env.t
    -> t
    -> (Closure_id.t * Function_declaration_type.t Or_unknown.t) proof

  val prove_strings : Typing_env.t -> t -> String_info.Set.t proof

  type symbol_or_tagged_immediate = private
    | Symbol of Symbol.t
    | Tagged_immediate of Immediate.t

  type to_lift = (* private *) (* CR mshinwell: resurrect *)
    | Immutable_block of Tag.Scannable.t * (symbol_or_tagged_immediate list)
    | Boxed_float of Float.t
    | Boxed_int32 of Int32.t
    | Boxed_int64 of Int64.t
    | Boxed_nativeint of Targetint.t

  type reification_result = private
    | Lift of to_lift
    | Simple of Simple.t
    | Cannot_reify
    | Invalid

  val reify
     : Typing_env.t
    -> min_occurrence_kind:Name_occurrence_kind.t
    -> t
    -> reification_result
end
