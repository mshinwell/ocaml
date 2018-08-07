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
  module Flambda_types : sig
    type t
    type ty_value
    type 'a ty_naked_number
    type ty_fabricated
    type function_declaration
  end

  val print : Format.formatter -> Flambda_types.t -> unit

  val print_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Flambda_types.t
    -> unit

  val print_ty_value_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Flambda_types.ty_value
    -> unit

  val print_ty_naked_immediate_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Immediate.Set.t Flambda_types.ty_naked_number
    -> unit

  val print_ty_naked_float_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number
    -> unit

  val print_ty_naked_int32_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Numbers.Int32.Set.t Flambda_types.ty_naked_number
    -> unit

  val print_ty_naked_int64_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Numbers.Int64.Set.t Flambda_types.ty_naked_number
    -> unit

  val print_ty_naked_nativeint_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Targetint.Set.t Flambda_types.ty_naked_number
    -> unit

  val print_ty_fabricated_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Flambda_types.ty_fabricated
    -> unit

  val print_function_declaration_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Flambda_types.function_declaration
    -> unit
end
