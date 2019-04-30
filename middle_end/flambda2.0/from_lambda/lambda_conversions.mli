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

(** Conversions of basic Lambda data types to their Flambda equivalents. *)

val flambda_type_of_lambda_value_kind : Lambda.value_kind -> Flambda_type.t

val inline_attribute
   : Lambda.inline_attribute
  -> Flambda.inline_attribute

val specialise_attribute
   : Lambda.specialise_attribute
  -> Flambda.specialise_attribute

val kind_of_primitive_native_repr : Primitive.native_repr -> Flambda_kind.t

val method_kind : Lambda.method_kind -> Flambda.Call_kind.method_kind
