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

val this_naked_immediate : Immediate.t -> Flambda_types.t
val this_naked_float : Numbers.Float_by_bit_pattern.t -> Flambda_types.t
val this_naked_int32 : Int32.t -> Flambda_types.t
val this_naked_int64 : Int64.t -> Flambda_types.t
val this_naked_nativeint : Targetint.t -> Flambda_types.t

val this_discriminant : Discriminant.t -> Flambda_types.t

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

val create_inlinable_function_declaration
   : Term_language_function_declaration.t
  -> Flambda_types.function_declaration

val create_non_inlinable_function_declaration
   : param_arity:Flambda_arity.t
  -> result_arity:Flambda_arity.t
  -> Flambda_types.function_declaration

val closure
   : Closure_id.t
  -> Flambda_types.function_declaration
  -> Flambda_types.ty_value Var_within_closure.Map.t
  -> set_of_closures:Flambda_types.ty_fabricated
  -> Flambda_types.t

val closure_containing_at_least
   : Var_within_closure.t
  -> closure_element_var:Variable.t
  -> Flambda_types.t

val set_of_closures
   : closures:Flambda_types.t Closure_id.Map.t
  -> Flambda_types.t

val set_of_closures_containing_at_least
   : Closure_id.t
  -> closure_var:Variable.t
  -> Flambda_types.t

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
