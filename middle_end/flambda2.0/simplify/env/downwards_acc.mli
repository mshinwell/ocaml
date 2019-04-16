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

type t

(** Print a downwards accumulator to a formatter. *)
val print : Format.formatter -> t -> unit

(** Create a downwards accumulator. *)
val create
   : Simplify_env_and_result.Downwards_env.t
  -> Continuation_uses_env.t
  -> Simplify_env_and_result.Result.t
  -> t

(** Extract the environment component of the given downwards accumulator. *)
val denv : t -> Simplify_env_and_result.Downwards_env.t

(** Map the environment component of the given downwards accumulator. *)
val map_denv
   : t
  -> f:(Simplify_env_and_result.Downwards_env.t
    -> Simplify_env_and_result.Downwards_env.t)
  -> t

(** Replace the environment component of the given downwards accumulator. *)
val with_denv : t -> Simplify_env_and_result.Downwards_env.t -> t

(** The result structure of the given downwards accumulator. *)
val r : t -> Simplify_env_and_result.Result.t

(** Map the result structure of the given downwards accumulator. *)
val map_r
   : t
  -> f:(Simplify_env_and_result.Result.t
    -> Simplify_env_and_result.Result.t)
  -> t

(** Replace the result structure of the given downwards accumulator. *)
val with_r : t -> Simplify_env_and_result.Result.t -> t

(* CR mshinwell: Why do these take scope arguments when [DE] knows the
   current scope level? *)
include Continuation_uses_env_intf.S with type t := t

val continuation_uses_env : t -> Continuation_uses_env.t
