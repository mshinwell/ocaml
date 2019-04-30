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

let flambda_type_of_lambda_value_kind (value_kind : Lambda.value_kind) =
  match value_kind with
  | Pgenval -> Flambda_type.any_value ()
  | Pfloatval -> Flambda_type.any_boxed_float ()
  | Pboxedintval Pint32 -> Flambda_type.any_boxed_int32 ()
  | Pboxedintval Pint64 -> Flambda_type.any_boxed_int64 ()
  | Pboxedintval Pnativeint -> Flambda_type.any_boxed_nativeint ()
  | Pintval -> Flambda_type.any_tagged_immediate ()

let inline_attribute (attr : Lambda.inline_attribute)
      : Flambda.inline_attribute =
  match attr with
  | Always_inline -> Always_inline
  | Never_inline -> Never_inline
  | Unroll i -> Unroll i
  | Default_inline -> Default_inline

let specialise_attribute (attr : Lambda.specialise_attribute)
      : Flambda.specialise_attribute =
  match attr with
  | Always_specialise -> Always_specialise
  | Never_specialise -> Never_specialise
  | Default_specialise -> Default_specialise

let kind_of_primitive_native_repr (repr : Primitive.native_repr) =
  match repr with
  | Same_as_ocaml_repr -> K.value ()
  | Unboxed_float -> K.naked_float ()
  | Unboxed_integer Pnativeint -> K.naked_nativeint ()
  | Unboxed_integer Pint32 -> K.naked_int32 ()
  | Unboxed_integer Pint64 -> K.naked_int64 ()
  | Untagged_int -> K.naked_immediate ()

let method_kind (kind : Lambda.method_kind) =
  match kind with
  | Self -> Self
  | Public -> Public
  | Cached -> Cached
