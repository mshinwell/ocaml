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

(** The type system of Flambda including various functions to analyse types.
    (The basic definitions are in [Flambda type0], which does not
    depend on [Flambda].) *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Basic definitions and constructors. *)
include module type of struct include Flambda0.Flambda_type end

(** For each of the kinds in an arity, create a "bottom" type. *)
val bottom_types_from_arity : Flambda_arity.t -> t list

(** For each of the kinds in an arity, create an "unknown" type. *)
val unknown_types_from_arity : Flambda_arity.t -> t list

(** Like [unknown_like] but for a array of types. *)
val unknown_like_array : t array -> t array

(** Rename free variables in a type. *)
(* CR mshinwell: and bound ones too, now... *)
val rename_variables : t -> f:(Variable.t -> Variable.t) -> t

(** Building of types and terms representing tagged / boxed values from
    specified constants. *)
val this_tagged_bool_named : bool -> Flambda0.Named.t * t
val this_tagged_immediate_named : Immediate.t -> Flambda0.Named.t * t

(** Building of types and terms representing untagged / unboxed values from
    specified constants. *)
val this_untagged_immediate_named : Immediate.t -> Flambda0.Named.t * t
val this_naked_float_named
   : Numbers.Float_by_bit_pattern.t
  -> Flambda0.Named.t * t
val this_naked_int32_named : Int32.t -> Flambda0.Named.t * t
val this_naked_int64_named : Int64.t -> Flambda0.Named.t * t
val this_naked_nativeint_named : Targetint.t -> Flambda0.Named.t * t

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

(** Type equality. *)
val equal : t -> t -> bool

(** Returns [true] if the given type is known to provide strictly more
    information about the corresponding value than the supplied type [than]. *)
val strictly_more_precise : t_in_context -> than:t_in_context -> bool

(** Whether values of the given two types will always be physically equal
    to each other. *)
val values_physically_equal : t -> t -> bool

(** Whether values of the given two types will always have a different
    structure from each other. *)
val values_structurally_distinct : t_in_context -> t_in_context -> bool

type reification_result = private
  | Term of Simple.t * t
  | Lift of Flambda_static0.Static_part.t
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
  | Answer_given_by of Name.t

val prove_tagged_immediate_as_discriminants
   : (t -> tagged_immediate_as_discriminants_proof proof) type_accessor

type is_tagged_immediate = private
  | Never_a_tagged_immediate
  | Always_a_tagged_immediate
  | Answer_given_by of Name.t

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
    -> t proof) type_accessor

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
  | Answer_given_by of Name.t

(** Determine which tags values of the given type may take on. *)
(* CR mshinwell: duplicate return value (Proved empty_set, and Invalid) *)
val prove_tags : (t -> tags proof) type_accessor

(** Determine the set of all possible length(s) of the array(s) or structured
    block(s) (i.e. blocks with tag less than [No_scan_tag]) described by the
    given type.  This function correctly handles float arrays (where the length
    of the array, on 32-bit platforms, may differ from the size of the block).
    [Unknown] is returned if a proof cannot be given but the type may yet
    represent array(s) or block(s); [Invalid] is returned if that can never
    be the case.
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

val prove_closures : (t -> closures proof) type_accessor

val prove_sets_of_closures
   : (t -> (Name.t option * set_of_closures) proof) type_accessor

val prove_closure : (t -> closure proof) type_accessor

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

(** Given the type of a [Switch] scrutinee and the arms of the [Switch], return
    which cases may be taken together with, for each such case, the destination
    continuation and any equations known to hold at the jump from the [Switch]
    to that continuation. *)
val switch_arms
   : (t
    -> arms:Continuation.t Discriminant.Map.t
    -> (Typing_env_extension.t * Continuation.t) Discriminant.Map.t)
  type_accessor

module Typing_env : sig
  include module type of struct include Typing_env0 end

  (** [diff t1 t2] returns judgements from the environment [t1] excepting any
      for which [t2] is known to specify an equally precise, or less precise,
      type. *)
  val diff : t -> t -> Typing_env_extension.t
end
