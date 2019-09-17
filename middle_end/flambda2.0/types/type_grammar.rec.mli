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

[@@@ocaml.warning "+a-30-40-41-42"]

(** The definition of types together with their constructors and operations
    upon them. *)

type t = private
  | Value of Type_of_kind_value.t
  | Naked_immediate of Type_of_kind_naked_immediate.t
  | Naked_float of Type_of_kind_naked_float.t
  | Naked_int32 of Type_of_kind_naked_int32.t
  | Naked_int64 of Type_of_kind_naked_int64.t
  | Naked_nativeint of Type_of_kind_naked_nativeint.t
  | Fabricated of Type_of_kind_fabricated.t

val print : Format.formatter -> t -> unit

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

include Contains_names.S with type t := t

val free_variables_transitive : t -> Variable.Set.t

val kind : t -> Flambda_kind.t

val alias_type_of : Flambda_kind.t -> Simple.t -> t

val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t

val get_alias : t -> Simple.t option

val is_obviously_bottom : t -> bool

val bottom : Flambda_kind.t -> t
val bottom_like : t -> t

val unknown : Flambda_kind.t -> t
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

val this_naked_immediate : Immediate.t -> t
val this_naked_float : Numbers.Float_by_bit_pattern.t -> t
val this_naked_int32 : Int32.t -> t
val this_naked_int64 : Int64.t -> t
val this_naked_nativeint : Targetint.t -> t

val this_tagged_immediate_without_alias : Immediate.t -> t
val this_naked_immediate_without_alias : Immediate.t -> t
val this_naked_float_without_alias : Numbers.Float_by_bit_pattern.t -> t
val this_naked_int32_without_alias : Int32.t -> t
val this_naked_int64_without_alias : Int64.t -> t
val this_naked_nativeint_without_alias : Targetint.t -> t

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

val this_discriminant : Discriminant.t -> t
val this_discriminant_without_alias : Discriminant.t -> t
val these_discriminants : Discriminant.Set.t -> t

val immutable_block : Tag.t -> fields:t list -> t

val immutable_block_of_values : Tag.t -> fields:Type_of_kind_value.t list -> t

val immutable_block_with_size_at_least
   : n:Targetint.OCaml.t
  -> field_n_minus_one:Variable.t
  -> t

val this_immutable_string : string -> t

val mutable_string : size:int -> t

val type_for_const : Simple.Const.t -> t
val kind_for_const : Simple.Const.t -> Flambda_kind.t

val create_inlinable_function_declaration
   : Term_language_function_declaration.t
  -> Rec_info.t
  -> Function_declaration_type.t

val create_non_inlinable_function_declaration
   : param_arity:Flambda_arity.t
  -> result_arity:Flambda_arity.t
  -> recursive:Recursive.t
  -> Function_declaration_type.t

val exactly_this_closure
   : Closure_id.t
  -> all_function_decls_in_set:Function_declaration_type.t Closure_id.Map.t
  -> all_closures_in_set:t Closure_id.Map.t
  -> all_closure_vars_in_set:Type_of_kind_value.t Var_within_closure.Map.t
  -> t

val at_least_the_closures_with_ids
   : this_closure:Closure_id.t
  -> Simple.t Closure_id.Map.t
  -> t

val closure_with_at_least_this_closure_var
   : Var_within_closure.t
  -> closure_element_var:Variable.t
  -> t

val array_of_length : length:t -> t

val make_suitable_for_environment
   : t
  -> Typing_env.t
  -> suitable_for:Typing_env.t
  -> Typing_env_extension.t * t

val expand_head : t -> Typing_env.t -> Resolved_type.t

(** Greatest lower bound of two types. *)
val meet : Meet_env.t -> t -> t -> t * Typing_env_extension.t

(** Least upper bound of two types. *)
val join : ?bound_name:Name.t -> Typing_env.t -> t -> t -> t

val force_to_kind_naked_float : t -> Type_of_kind_naked_float.t

module T_V_ops : Type_descr_intf.S_ops
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_env := Meet_env.t
  with type descr := Type_of_kind_value.t
  with type head := Type_of_kind_value0.t

module T_NI_ops : Type_descr_intf.S_ops
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_env := Meet_env.t
  with type descr := Type_of_kind_naked_immediate.t
  with type head := Type_of_kind_naked_immediate0.t

module T_Nf_ops : Type_descr_intf.S_ops
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_env := Meet_env.t
  with type descr := Type_of_kind_naked_float.t
  with type head := Type_of_kind_naked_float0.t

module T_N32_ops : Type_descr_intf.S_ops
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_env := Meet_env.t
  with type descr := Type_of_kind_naked_int32.t
  with type head := Type_of_kind_naked_int32_0.t

module T_N64_ops : Type_descr_intf.S_ops
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_env := Meet_env.t
  with type descr := Type_of_kind_naked_int64.t
  with type head := Type_of_kind_naked_int64_0.t

module T_NN_ops : Type_descr_intf.S_ops
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_env := Meet_env.t
  with type descr := Type_of_kind_naked_nativeint.t
  with type head := Type_of_kind_naked_nativeint0.t

module T_F_ops : Type_descr_intf.S_ops
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_env := Meet_env.t
  with type descr := Type_of_kind_fabricated.t
  with type head := Type_of_kind_fabricated0.t
