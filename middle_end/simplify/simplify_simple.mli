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

(** Basic simplification functions on [Simple.t], [Name.t], etc. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

val simplify_name
   : Simplify_env_and_result.Env.t
  -> Name.t
  -> Name.t * Flambda_type.t

val simplify_simple_for_let
   : Simplify_env_and_result.Env.t
  -> Simple.t
  -> Simple.t * Flambda_type.t

val simplify_simple
   : Simplify_env_and_result.Env.t
  -> Simple.t
  -> Simple.t * Flambda_type.t

val simplify_simples
   : Simplify_env_and_result.Env.t
  -> Simple.t list
  -> (Simple.t * Flambda_type.t) list
