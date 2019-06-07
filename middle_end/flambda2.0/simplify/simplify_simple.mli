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

(** Basic simplification functions on [Simple.t], [Name.t], etc. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Rename "simplify_simple" -> "simplify" *)

val simplify_simple
   : Downwards_acc.t
  -> Simple.t
  -> Simple.t * Flambda_type.t

val simplify_simple_and_drop_type
   : Downwards_acc.t
  -> Simple.t
  -> Simple.t

val simplify_simples
   : Downwards_acc.t
  -> Simple.t list
  -> (Simple.t * Flambda_type.t) list

val simplify_simples_and_drop_types
   : Downwards_acc.t
  -> Simple.t list
  -> Simple.t list
