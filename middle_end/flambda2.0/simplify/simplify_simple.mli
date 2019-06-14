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

(** Simplification functions on [Simple.t]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type result = private
  | Ok of Simple.t * Flambda_type.t
  | Bottom of Flambda_kind.t

val simplify_simple
   : Downwards_acc.t
  -> Simple.t
  -> result

val simplify_simples
   : Downwards_acc.t
  -> Simple.t list
  -> (Simple.t * Flambda_type.t) list Or_bottom.t
