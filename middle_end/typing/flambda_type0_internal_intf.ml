(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Float = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module Blocks : sig type t end
module Discriminants : sig type t end
module Expr : sig type t end
module Function_parameters : sig type t end
module Immediates : sig type t end
module Join_env : sig type t end
module Typing_env : sig type t end
module Typing_env_extension : sig type t end

type 'a or_alias =
  | No_alias of 'a
  | Type of Export_id.t
  | Equals of Simple.t

type t =
  | Value of ty_value
  | Naked_number :
      'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> descr
  | Fabricated of ty_fabricated

and ty_value = of_kind_value ty
and 'a ty_naked_number = 'a of_kind_naked_number ty
and ty_fabricated = of_kind_fabricated ty

and 'a ty = 'a unknown_or_join or_alias

and 'a unknown_or_join =
  | Unknown
  | Join of ('a * Name_permutation.t) list

and of_kind_value =
  | Blocks_and_tagged_immediates of blocks_and_tagged_immediates
  | Boxed_number : _ of_kind_value_boxed_number -> of_kind_value
  | Closures of closures
  | String of String_info.Set.t

and blocks_and_tagged_immediates = {
  immediates : Immediates.t;
  blocks : Blocks.t;
}

and 'a of_kind_value_boxed_number =
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

and inlinable_function_declaration = {
  closure_origin : Closure_origin.t;
  continuation_param : Continuation.t;
  exn_continuation_param : Continuation.t;
  is_classic_mode : bool;
  params : Function_parameters.t;
  body : Expr.t;
  code_id : Code_id.t;
  free_names_in_body : Name_occurrences.t;
  stub : bool;
  result_arity : Flambda_arity.t;
  dbg : Debuginfo.t;
  inline : inline_attribute;
  specialise : specialise_attribute;
  is_a_functor : bool;
  invariant_params : Variable.Set.t lazy_t;
  size : int option lazy_t;
  direct_call_surrogate : Closure_id.t option;
  my_closure : Variable.t;
}

and non_inlinable_function_declarations = {
  direct_call_surrogate : Closure_id.t option;
}

and function_declarations =
  | Non_inlinable of non_inlinable_function_declarations Or_unknown.t;
  | Inlinable of inlinable_function_declaration list

and closure = {
  ty : T.Function_type.t;
  function_decls : function_declarations;
  closure_elements : T.Closure_elements.t;
  set_of_closures : ty_fabricated;
}

and closures = {
  by_closure_id : T.Closure_ids_with_elements.t;
}

and 'a of_kind_naked_number =
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

and of_kind_fabricated =
  | Discriminant of Discriminants.t
  | Set_of_closures of set_of_closures

and set_of_closures = {
  closures : ty_value T.Closure_ids.t;
}

module Closure : sig
  type t = closure

  val add_or_meet_equations
     : t
    -> Typing_env.t
    -> Typing_env_extension.t
    -> t

  include Contains_names.S with type t := t
end

module Ty_value : sig
  type t = ty_value

  val add_or_meet_equations
     : t
    -> Typing_env.t
    -> Typing_env_extension.t
    -> t

  include Contains_names.S with type t := t
end

val print : Format.formatter -> t -> unit

val print_typing_environment_entry
   : Format.formatter
  -> typing_environment_entry
  -> unit

val print_typing_environment
   : Format.formatter
  -> typing_environment
  -> unit

val print_typing_env_extension
   : Format.formatter
  -> env_extension
  -> unit

val print_parameters
   : cache:Printing_cache.t
  -> Format.formatter
  -> parameters
  -> unit

val bottom : Flambda_kind.t -> t

val alias_type_of : Flambda_kind.t -> Simple.t -> t

val free_names : flambda_type -> Name_occurrences.t

val free_names_set : flambda_type -> Name.Set.t

val unknown : Flambda_kind.t -> t

val force_to_kind_value : t -> of_kind_value ty

val force_to_kind_naked_number
   : 'a Flambda_kind.Naked_number.t
  -> t
  -> 'a of_kind_naked_number ty

val force_to_kind_naked_int32 : t -> Int32.Set.t ty_naked_number

val force_to_kind_naked_int64 : t -> Int64.Set.t ty_naked_number

val force_to_kind_naked_nativeint : t -> Targetint.Set.t ty_naked_number

val force_to_kind_naked_float
   : t
  -> Numbers.Float_by_bit_pattern.Set.t ty_naked_number

val force_to_kind_naked_immediate : t -> Immediate.Set.t ty_naked_number

val force_to_kind_fabricated : t -> of_kind_fabricated ty

val kind : flambda_type -> Flambda_kind.t

val is_empty_typing_environment : typing_environment -> bool

val any_value_as_ty_value : unit -> ty_value

val any_fabricated_as_ty_fabricated : unit -> ty_fabricated

val bottom_as_ty_value : unit -> ty_value

val bottom_as_ty_fabricated : unit -> ty_fabricated

val get_alias : flambda_type -> Simple.t option

val print_ty_value
   : Format.formatter
  -> ty_value
  -> unit

val ty_is_obviously_bottom : 'a ty -> bool

val print_ty_naked_number
   : Format.formatter
  -> 'a ty_naked_number
  -> unit

val print_ty_fabricated
   : Format.formatter
  -> ty_fabricated
  -> unit

val is_obviously_bottom : flambda_type -> bool

val create_parameters_from_types : flambda_type list -> parameters

val apply_name_permutation : t -> Name_permutation.t -> t

val apply_freshening : t -> Freshening.t -> t
