(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Simplification of primitive applications. *)

val simplify_primitive
   : Simplify_env_and_result.Env.t
  -> Simplify_env_and_result.Result.t
  -> Flambda_primitive.t
  -> Debuginfo.t
  -> result_var:Variable.t
  -> Flambda.Reachable.t * Flambda_type.t * Simplify_env_and_result.Result.t
