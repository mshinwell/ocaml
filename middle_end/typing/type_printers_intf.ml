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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module T : Typing_world_abstract.S
  module Functor_T : Typing_world_abstract.Functor_S

  val print : Format.formatter -> T.Flambda_types.t -> unit

  val print_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> T.Flambda_types.t
    -> unit

  val print_ty_value_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> T.Flambda_types.ty_value
    -> unit

  val print_ty_naked_immediate_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Immediate.Set.t T.Flambda_types.ty_naked_number
    -> unit

  val print_ty_naked_float_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Numbers.Float_by_bit_pattern.Set.t T.Flambda_types.ty_naked_number
    -> unit

  val print_ty_naked_int32_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Numbers.Int32.Set.t T.Flambda_types.ty_naked_number
    -> unit

  val print_ty_naked_int64_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Numbers.Int64.Set.t T.Flambda_types.ty_naked_number
    -> unit

  val print_ty_naked_nativeint_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Targetint.Set.t T.Flambda_types.ty_naked_number
    -> unit

  val print_ty_fabricated_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> T.Flambda_types.ty_fabricated
    -> unit

  val print_function_declaration_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> T.Flambda_types.function_declaration
    -> unit
end
