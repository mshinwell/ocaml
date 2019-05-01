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

val value_kind : Lambda.value_kind -> Flambda_kind.t

val inline_attribute
   : Lambda.inline_attribute
  -> Inline_attribute.t

val specialise_attribute
   : Lambda.specialise_attribute
  -> Specialise_attribute.t

val kind_of_primitive_native_repr : Primitive.native_repr -> Flambda_kind.t

val method_kind : Lambda.meth_kind -> Call_kind.method_kind

val raise_kind : Lambda.raise_kind -> Trap_action.raise_kind
