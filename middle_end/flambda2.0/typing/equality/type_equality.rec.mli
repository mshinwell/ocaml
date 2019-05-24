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

val equal
   : bound_name:Name.t option
  -> Typing_env.t
  -> Typing_env.t
  -> Flambda_types.t
  -> Flambda_types.t
  -> bool

val equal_with_env
   : ?bound_name:Name.t
  -> Type_equality_env.t
  -> Flambda_types.t
  -> Flambda_types.t
  -> bool

val equal_closures_entry
   : Type_equality_env.t
  -> Flambda_types.closures_entry
  -> Flambda_types.closures_entry
  -> bool

val equal_set_of_closures_entry
   : Type_equality_env.t
  -> Flambda_types.set_of_closures_entry
  -> Flambda_types.set_of_closures_entry
  -> bool
