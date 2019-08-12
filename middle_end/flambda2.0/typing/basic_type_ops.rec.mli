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

(** For documentation on this module please see [Type_system_intf]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val get_alias : Type_grammar.t -> Simple.t option
val get_alias_ty : _ Type_grammar.ty -> Simple.t option

val is_obviously_bottom : Type_grammar.t -> bool

val ty_is_obviously_bottom : _ Type_grammar.ty -> bool

val unknown : Flambda_kind.t -> Type_grammar.t

val any_value : unit -> Type_grammar.t

val any_fabricated : unit -> Type_grammar.t

val any_tagged_immediate : unit -> Type_grammar.t
val any_tagged_bool : unit -> Type_grammar.t

val any_boxed_float : unit -> Type_grammar.t
val any_boxed_int32 : unit -> Type_grammar.t
val any_boxed_int64 : unit -> Type_grammar.t
val any_boxed_nativeint : unit -> Type_grammar.t

val any_naked_immediate : unit -> Type_grammar.t
val any_naked_float : unit -> Type_grammar.t

val this_tagged_immediate : Immediate.t -> Type_grammar.t
val this_boxed_float : Numbers.Float_by_bit_pattern.t -> Type_grammar.t
val this_boxed_int32 : Int32.t -> Type_grammar.t
val this_boxed_int64 : Int64.t -> Type_grammar.t
val this_boxed_nativeint : Targetint.t -> Type_grammar.t

val these_tagged_immediates : Immediate.Set.t -> Type_grammar.t
val these_boxed_floats : Numbers.Float_by_bit_pattern.Set.t -> Type_grammar.t
val these_boxed_int32s : Int32.Set.t -> Type_grammar.t
val these_boxed_int64s : Int64.Set.t -> Type_grammar.t
val these_boxed_nativeints : Targetint.Set.t -> Type_grammar.t

val this_naked_immediate : Immediate.t -> Type_grammar.t
val this_naked_float : Numbers.Float_by_bit_pattern.t -> Type_grammar.t
val this_naked_int32 : Int32.t -> Type_grammar.t
val this_naked_int64 : Int64.t -> Type_grammar.t
val this_naked_nativeint : Targetint.t -> Type_grammar.t

val this_tagged_immediate_without_alias : Immediate.t -> Type_grammar.t
val this_naked_immediate_without_alias : Immediate.t -> Type_grammar.t
val this_naked_float_without_alias
   : Numbers.Float_by_bit_pattern.t
  -> Type_grammar.t
val this_naked_int32_without_alias : Int32.t -> Type_grammar.t
val this_naked_int64_without_alias : Int64.t -> Type_grammar.t
val this_naked_nativeint_without_alias : Targetint.t -> Type_grammar.t

val these_naked_immediates : Immediate.Set.t -> Type_grammar.t
val these_naked_floats : Numbers.Float_by_bit_pattern.Set.t -> Type_grammar.t
val these_naked_int32s : Int32.Set.t -> Type_grammar.t
val these_naked_int64s : Int64.Set.t -> Type_grammar.t
val these_naked_nativeints : Targetint.Set.t -> Type_grammar.t

val boxed_float_alias_to : naked_float:Variable.t -> Type_grammar.t
val boxed_int32_alias_to : naked_int32:Variable.t -> Type_grammar.t
val boxed_int64_alias_to : naked_int64:Variable.t -> Type_grammar.t
val boxed_nativeint_alias_to : naked_nativeint:Variable.t -> Type_grammar.t

val box_float : Type_grammar.t -> Type_grammar.t
val box_int32 : Type_grammar.t -> Type_grammar.t
val box_int64 : Type_grammar.t -> Type_grammar.t
val box_nativeint : Type_grammar.t -> Type_grammar.t

val this_discriminant : Discriminant.t -> Type_grammar.t
val this_discriminant_without_alias : Discriminant.t -> Type_grammar.t
val these_discriminants : Discriminant.Set.t -> Type_grammar.t

val immutable_block
   : Tag.t
  -> fields:Type_grammar.t list
  -> Type_grammar.t

val immutable_block_of_values
   : Tag.t
  -> fields:Type_grammar.ty_value list
  -> Type_grammar.t

val immutable_block_with_size_at_least
   : n:Targetint.OCaml.t
  -> field_n_minus_one:Variable.t
  -> Type_grammar.t

val this_immutable_string : string -> Type_grammar.t

val mutable_string : size:int -> Type_grammar.t

val bottom : Flambda_kind.t -> Type_grammar.t
val bottom_like : Type_grammar.t -> Type_grammar.t

val unknown_like : Type_grammar.t -> Type_grammar.t
val unknown_as_ty_fabricated : unit -> Type_grammar.ty_fabricated

val type_for_const : Reg_width_const.t -> Type_grammar.t
val kind_for_const : Reg_width_const.t -> Flambda_kind.t

val create_inlinable_function_declaration
   : Term_language_function_declaration.t
  -> Rec_info.t
  -> Type_grammar.function_declaration

val create_non_inlinable_function_declaration
   : param_arity:Flambda_arity.t
  -> result_arity:Flambda_arity.t
  -> recursive:Recursive.t
  -> Type_grammar.function_declaration

val exactly_this_closure
   : Closure_id.t
  -> all_function_decls_in_set:
       Type_grammar.function_declaration Closure_id.Map.t
  -> all_closures_in_set:Type_grammar.t Closure_id.Map.t
  -> all_closure_vars_in_set:Type_grammar.ty_value Var_within_closure.Map.t
  -> Type_grammar.t

val at_least_the_closures_with_ids
   : this_closure:Closure_id.t
  -> Simple.t Closure_id.Map.t
  -> Type_grammar.t

val closure_with_at_least_this_closure_var
   : Var_within_closure.t
  -> closure_element_var:Variable.t
  -> Type_grammar.t

val array_of_length : length:Type_grammar.ty_value -> Type_grammar.t

val alias_type_of : Flambda_kind.t -> Simple.t -> Type_grammar.t
val alias_type_of_as_ty_value : Simple.t -> Type_grammar.ty_value
val alias_type_of_as_ty_fabricated : Simple.t -> Type_grammar.ty_fabricated

val kind : Type_grammar.t -> Flambda_kind.t

val force_to_kind_value : Type_grammar.t -> Type_grammar.ty_value

val force_to_kind_naked_number
   : 'kind Flambda_kind.Naked_number.t
  -> Type_grammar.t
  -> 'kind Type_grammar.ty_naked_number

val force_to_kind_naked_float
   : Type_grammar.t
  -> Flambda_kind.naked_float Type_grammar.ty_naked_number

val force_to_kind_naked_int32
   : Type_grammar.t
  -> Flambda_kind.naked_int32 Type_grammar.ty_naked_number

val force_to_kind_naked_int64
   : Type_grammar.t
  -> Flambda_kind.naked_int64 Type_grammar.ty_naked_number

val force_to_kind_naked_nativeint
   : Type_grammar.t
  -> Flambda_kind.naked_nativeint Type_grammar.ty_naked_number

val force_to_kind_naked_immediate
   : Type_grammar.t
  -> Flambda_kind.naked_immediate Type_grammar.ty_naked_number

val force_to_kind_fabricated : Type_grammar.t -> Type_grammar.ty_fabricated

val apply_rec_info_of_kind_value
   : Type_grammar.of_kind_value
  -> Rec_info.t
  -> Type_grammar.of_kind_value Or_bottom.t

val apply_rec_info_of_kind_naked_number
   : 'a Type_grammar.of_kind_naked_number
  -> Rec_info.t
  -> 'a Type_grammar.of_kind_naked_number Or_bottom.t

val apply_rec_info_of_kind_fabricated
   : Type_grammar.of_kind_fabricated
  -> Rec_info.t
  -> Type_grammar.of_kind_fabricated Or_bottom.t

val apply_rec_info
   : Type_grammar.t
  -> Rec_info.t
  -> Type_grammar.t Or_bottom.t

val apply_name_permutation
   : Type_grammar.t
  -> Name_permutation.t
  -> Type_grammar.t
