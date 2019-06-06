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

(** Extract the environment component of the given downwards accumulator. *)
val denv : t -> Downwards_env.t

(** Map the environment component of the given downwards accumulator. *)
val map_denv : t -> f:(Downwards_env.t -> Downwards_env.t) -> t

(** Replace the environment component of the given downwards accumulator. *)
val with_denv : t -> Downwards_env.t -> t

(** The result structure of the given downwards accumulator. *)
val r : t -> Simplify_result.t
