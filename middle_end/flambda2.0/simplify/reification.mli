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

(** Construct terms using only information from types. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module E = Simplify_env_and_result.Env
module R = Simplify_env_and_result.Result
module T = Flambda_type

val try_to_reify
   : Simplify_env_and_result.Env.t
  -> Simplify_env_and_result.Result.t
  -> Reachable.t
  -> bound_to:Variable.t
  -> cannot_lift:bool
  -> Reachable.t * Simplify_env_and_result.Env.t * Flambda_type.t
       * Simplify_env_and_result.Result.t
