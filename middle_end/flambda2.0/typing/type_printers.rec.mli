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

[@@@ocaml.warning "+a-4-30-40-41-42"]

val print : Format.formatter -> Type_grammar.t -> unit

val print_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Type_grammar.t
  -> unit

val print_ty_value
   : Format.formatter
  -> Type_grammar.ty_value
  -> unit

val print_ty_naked_number
   : Format.formatter
  -> _ Type_grammar.ty_naked_number
  -> unit

val print_ty_naked_float
   : Format.formatter
  -> Flambda_kind.naked_float Type_grammar.ty_naked_number
  -> unit

val print_ty_naked_int32
   : Format.formatter
  -> Flambda_kind.naked_int32 Type_grammar.ty_naked_number
  -> unit

val print_ty_naked_int64
   : Format.formatter
  -> Flambda_kind.naked_int64 Type_grammar.ty_naked_number
  -> unit

val print_ty_naked_nativeint
   : Format.formatter
  -> Flambda_kind.naked_nativeint Type_grammar.ty_naked_number
  -> unit

val print_ty_fabricated
   : Format.formatter
  -> Type_grammar.ty_fabricated
  -> unit

val print_ty_value_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Type_grammar.ty_value
  -> unit

val print_ty_naked_immediate_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Flambda_kind.naked_immediate Type_grammar.ty_naked_number
  -> unit

val print_ty_naked_float_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Flambda_kind.naked_float Type_grammar.ty_naked_number
  -> unit

val print_ty_naked_int32_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Flambda_kind.naked_int32 Type_grammar.ty_naked_number
  -> unit

val print_ty_naked_int64_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Flambda_kind.naked_int64 Type_grammar.ty_naked_number
  -> unit

val print_ty_naked_nativeint_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Flambda_kind.naked_nativeint Type_grammar.ty_naked_number
  -> unit

val print_ty_fabricated_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Type_grammar.ty_fabricated
  -> unit

val print_function_declaration_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Type_grammar.function_declaration
  -> unit
