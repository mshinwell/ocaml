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

(** Erase aliases to all except a given set of variables throughout types. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val erase_aliases
   : Typing_env.t
  -> allowed:Variable.Set.t
  -> Flambda_types.t
  -> Flambda_types.t

val erase_aliases_ty_value
   : Typing_env.t
  -> allowed:Variable.Set.t
  -> Flambda_types.ty_value
  -> Flambda_types.ty_value

val erase_aliases_ty_fabricated
   : Typing_env.t
  -> allowed:Variable.Set.t
  -> Flambda_types.ty_fabricated
  -> Flambda_types.ty_fabricated
