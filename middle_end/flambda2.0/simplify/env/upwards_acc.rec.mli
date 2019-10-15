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

(** Create an upwards accumulator by copying the result structure out of
    the given downwards accumulator. *)
val of_dacc : Downwards_acc.t -> t

(** Extract the environment component of the given upwards accumulator. *)
val uenv : t -> Upwards_env.t

(** Map the environment component of the given upwards accumulator. *)
val map_uenv : t -> f:(Upwards_env.t -> Upwards_env.t) -> t

(** Replace the environment component of the given upwards accumulator. *)
val with_uenv : t -> Upwards_env.t -> t

(** The result structure of the given upwards accumulator. *)
val r : t -> Simplify_result.t
