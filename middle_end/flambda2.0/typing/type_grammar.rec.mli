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

type t =
  | Value of Type_of_kind_value.t
  | Naked_number of Type_of_kind_naked_number.t
  | Fabricated of Type_of_kind_fabricated.t

include Contains_names.S with type t := t

val print : Format.formatter -> t -> unit

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

val erase_aliases : t -> allowed:Variable.Set.t -> t

val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t

(** Greatest lower bound of two types. *)
val meet : Meet_env.t -> t -> t -> t * Typing_env_extension.t

(** Least upper bound of two types. *)
val join : ?bound_name:Name.t -> Typing_env.t -> t -> t -> t

val get_alias : Type_grammar.t -> Simple.t option

val is_obviously_bottom : Type_grammar.t -> bool

val bottom : Flambda_kind.t -> t
val bottom_like : t -> t

val unknown : Flambda_kind.t -> t
val unknown_like : t -> t
val unknown_as_ty_fabricated : unit -> Type_of_kind_fabricated.t

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

val box_float : Type_grammar.t -> t
val box_int32 : Type_grammar.t -> t
val box_int64 : Type_grammar.t -> t
val box_nativeint : Type_grammar.t -> t

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
  -> Closures_entry.function_declaration

val create_non_inlinable_function_declaration
   : param_arity:Flambda_arity.t
  -> result_arity:Flambda_arity.t
  -> recursive:Recursive.t
  -> Closures_entry.function_declaration

val exactly_this_closure
   : Closure_id.t
  -> all_function_decls_in_set:
       Type_grammar.function_declaration Closure_id.Map.t
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

val array_of_length : length:Type_grammar.ty_value -> t

val alias_type_of : Flambda_kind.t -> Simple.t -> t

val kind : Type_grammar.t -> Flambda_kind.t

val force_to_kind_value : Type_grammar.t -> Type_grammar.ty_value

val force_to_kind_naked_number
   : 'kind Flambda_kind.Naked_number.t
  -> t
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
