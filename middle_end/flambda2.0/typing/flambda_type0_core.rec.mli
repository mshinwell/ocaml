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

(** For documentation on this module please see [Flambda_type0_intf]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val get_alias : Flambda_types.t -> Simple.t option
val get_alias_ty : _ Flambda_types.ty -> Simple.t option

val is_obviously_bottom : Flambda_types.t -> bool

val ty_is_obviously_bottom : _ Flambda_types.ty -> bool

val unknown : Flambda_kind.t -> Flambda_types.t

val any_value : unit -> Flambda_types.t

val any_fabricated : unit -> Flambda_types.t

val any_tagged_immediate : unit -> Flambda_types.t
val any_tagged_bool : unit -> Flambda_types.t

val any_boxed_float : unit -> Flambda_types.t
val any_boxed_int32 : unit -> Flambda_types.t
val any_boxed_int64 : unit -> Flambda_types.t
val any_boxed_nativeint : unit -> Flambda_types.t

val any_naked_immediate : unit -> Flambda_types.t
val any_naked_float : unit -> Flambda_types.t

val this_tagged_immediate : Immediate.t -> Flambda_types.t
val this_boxed_float : Numbers.Float_by_bit_pattern.t -> Flambda_types.t
val this_boxed_int32 : Int32.t -> Flambda_types.t
val this_boxed_int64 : Int64.t -> Flambda_types.t
val this_boxed_nativeint : Targetint.t -> Flambda_types.t

val these_tagged_immediates : Immediate.Set.t -> Flambda_types.t
val these_boxed_floats : Numbers.Float_by_bit_pattern.Set.t -> Flambda_types.t
val these_boxed_int32s : Int32.Set.t -> Flambda_types.t
val these_boxed_int64s : Int64.Set.t -> Flambda_types.t
val these_boxed_nativeints : Targetint.Set.t -> Flambda_types.t

val this_naked_immediate : Immediate.t -> Flambda_types.t
val this_naked_float : Numbers.Float_by_bit_pattern.t -> Flambda_types.t
val this_naked_int32 : Int32.t -> Flambda_types.t
val this_naked_int64 : Int64.t -> Flambda_types.t
val this_naked_nativeint : Targetint.t -> Flambda_types.t

val this_tagged_immediate_without_alias : Immediate.t -> Flambda_types.t
val this_naked_immediate_without_alias : Immediate.t -> Flambda_types.t
val this_naked_float_without_alias
   : Numbers.Float_by_bit_pattern.t
  -> Flambda_types.t
val this_naked_int32_without_alias : Int32.t -> Flambda_types.t
val this_naked_int64_without_alias : Int64.t -> Flambda_types.t
val this_naked_nativeint_without_alias : Targetint.t -> Flambda_types.t

val these_naked_immediates : Immediate.Set.t -> Flambda_types.t
val these_naked_floats : Numbers.Float_by_bit_pattern.Set.t -> Flambda_types.t
val these_naked_int32s : Int32.Set.t -> Flambda_types.t
val these_naked_int64s : Int64.Set.t -> Flambda_types.t
val these_naked_nativeints : Targetint.Set.t -> Flambda_types.t

val boxed_float_alias_to : naked_float:Variable.t -> Flambda_types.t
val boxed_int32_alias_to : naked_int32:Variable.t -> Flambda_types.t
val boxed_int64_alias_to : naked_int64:Variable.t -> Flambda_types.t
val boxed_nativeint_alias_to : naked_nativeint:Variable.t -> Flambda_types.t

val box_float : Flambda_types.t -> Flambda_types.t
val box_int32 : Flambda_types.t -> Flambda_types.t
val box_int64 : Flambda_types.t -> Flambda_types.t
val box_nativeint : Flambda_types.t -> Flambda_types.t

val this_discriminant : Discriminant.t -> Flambda_types.t
val this_discriminant_without_alias : Discriminant.t -> Flambda_types.t
val these_discriminants : Discriminant.Set.t -> Flambda_types.t

val immutable_block
   : Tag.t
  -> fields:Flambda_types.t list
  -> Flambda_types.t

val immutable_block_of_values
   : Tag.t
  -> fields:Flambda_types.ty_value list
  -> Flambda_types.t

val immutable_block_with_size_at_least
   : n:Targetint.OCaml.t
  -> field_n_minus_one:Variable.t
  -> Flambda_types.t

val bottom : Flambda_kind.t -> Flambda_types.t
val bottom_as_ty_fabricated : unit -> Flambda_types.ty_fabricated
val bottom_like : Flambda_types.t -> Flambda_types.t

val unknown_like : Flambda_types.t -> Flambda_types.t
val unknown_as_ty_fabricated : unit -> Flambda_types.ty_fabricated

val type_for_const : Simple.Const.t -> Flambda_types.t
val kind_for_const : Simple.Const.t -> Flambda_kind.t

val create_inlinable_function_declaration
   : Term_language_function_declaration.t
  -> Rec_info.t
  -> Flambda_types.function_declaration

val create_non_inlinable_function_declaration
   : param_arity:Flambda_arity.t
  -> result_arity:Flambda_arity.t
  -> recursive:Recursive.t
  -> Flambda_types.function_declaration

val exactly_this_closure
   : Closure_id.t
  -> Flambda_types.function_declaration
  -> Flambda_types.ty_value Var_within_closure.Map.t
  -> bound_to:Variable.t
  -> Flambda_types.t

val at_least_the_closures_with_ids
   : Simple.t Closure_id.Map.t
  -> Flambda_types.t

val closure_with_at_least_this_closure_var
   : Var_within_closure.t
  -> closure_element_var:Variable.t
  -> Flambda_types.t

val array_of_length : length:Flambda_types.ty_value -> Flambda_types.t

val alias_type_of : Flambda_kind.t -> Simple.t -> Flambda_types.t
val alias_type_of_as_ty_value : Simple.t -> Flambda_types.ty_value
val alias_type_of_as_ty_fabricated : Simple.t -> Flambda_types.ty_fabricated

val kind : Flambda_types.t -> Flambda_kind.t

val force_to_kind_value : Flambda_types.t -> Flambda_types.ty_value

val force_to_kind_naked_number
   : 'kind Flambda_kind.Naked_number.t
  -> Flambda_types.t
  -> 'kind Flambda_types.ty_naked_number

val force_to_kind_naked_float
   : Flambda_types.t
  -> Flambda_kind.naked_float Flambda_types.ty_naked_number

val force_to_kind_naked_int32
   : Flambda_types.t
  -> Flambda_kind.naked_int32 Flambda_types.ty_naked_number

val force_to_kind_naked_int64
   : Flambda_types.t
  -> Flambda_kind.naked_int64 Flambda_types.ty_naked_number

val force_to_kind_naked_nativeint
   : Flambda_types.t
  -> Flambda_kind.naked_nativeint Flambda_types.ty_naked_number

val force_to_kind_naked_immediate
   : Flambda_types.t
  -> Flambda_kind.naked_immediate Flambda_types.ty_naked_number

val force_to_kind_fabricated : Flambda_types.t -> Flambda_types.ty_fabricated

val apply_rec_info_of_kind_value
   : Flambda_types.of_kind_value
  -> Rec_info.t
  -> Flambda_types.of_kind_value Or_bottom.t

val apply_rec_info_of_kind_naked_number
   : 'a Flambda_types.of_kind_naked_number
  -> Rec_info.t
  -> 'a Flambda_types.of_kind_naked_number Or_bottom.t

val apply_rec_info_of_kind_fabricated
   : Flambda_types.of_kind_fabricated
  -> Rec_info.t
  -> Flambda_types.of_kind_fabricated Or_bottom.t

val apply_rec_info
   : Flambda_types.t
  -> Rec_info.t
  -> Flambda_types.t Or_bottom.t

val apply_name_permutation
   : Flambda_types.t
  -> Name_permutation.t
  -> Flambda_types.t

val apply_name_permutation_ty_fabricated
   : Flambda_types.ty_fabricated
  -> Name_permutation.t
  -> Flambda_types.ty_fabricated
