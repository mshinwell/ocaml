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

  include Expr_std.S with type t := t

  val arity_of_list : t list -> Flambda_arity.t

  val erase_aliases : t -> allowed:Variable.Set.t -> t

  module Typing_env_extension : sig
    type t

    include Expr_std.S with type t := t

    val empty : t

    val is_empty : t -> bool

    val add_equation : t -> Name.t -> flambda_type -> t
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

    val find : t -> Name.t -> flambda_type

    val defined_variables
       : t
      -> at_or_previous_to:Scope_level.t
      -> Variable.Set.t

    (** Adjust the domain of the given typing environment so that it only
        mentions names which are symbols, not variables. *)
    val restrict_to_symbols : t -> t

    val add_definition : t -> Name.t -> Scope_level.t -> flambda_type -> t

    val add_equation : t -> Name.t -> Scope_level.t -> flambda_type -> t

    val mem : t -> Name.t -> bool

    (** Add the given environment extension into the given typing environment.
        During the process, if an attempt is made to add a name which is
        already bound, the given name's type will be determined using a meet
        operation. *)
    val add_env_extension : t -> Typing_env_extension.t -> t

    val get_canonical_name : t -> Name.t -> Name.t
  end

  val join : Typing_env.t -> t -> t -> t

  (* CR mshinwell: Substitute out this alias once it's finalised *)
  type 'a type_accessor = Typing_env.t -> 'a

  type 'a ty
  type 'a unknown_or_join

  type ty_value
  type 'a ty_naked_number
  type ty_fabricated

  type function_declaration

  (** Construction of top types. *)
  val unknown : Flambda_kind.t -> t

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

  (** The type of a block with a known tag, size and field types. *)
  val block
     : Tag.t
    -> fields:t list
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

  (** The type of a set of closures containing exactly those closure IDs
      with the given types. *)
  val set_of_closures
     : closures:t Closure_id.Map.t
    -> t

  (** Construct a type equal to the type of the given name.  (The name
      must be present in the given environment when calling e.g. [join].) *)
  val alias_type_of : Flambda_kind.t -> Simple.t -> t

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

  (** Prove that the given type, of kind [Value], is a closures type
      describing exactly one closure.  The function declaration corresponding
      to such closure is returned together with its closure ID. *)
  val prove_single_closures_entry
     : Typing_env.t
    -> t
    -> (Closure_id.t * function_declaration) proof
end
