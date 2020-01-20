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

module T = Flambda_type
module TE = Flambda_type.Typing_env

type t

val create
   : Continuation_use_kind.t
  -> typing_env_at_use:TE.t
  -> inside_handlers_of_recursive_continuations_at_use:Scope.Set.t
  -> Apply_cont_rewrite_id.t
  -> arg_types:T.t list
  -> t

val print : Format.formatter -> t -> unit

val id : t -> Apply_cont_rewrite_id.t
val use_kind : t -> Continuation_use_kind.t
val arg_types : t -> T.t list
val typing_env_at_use : t -> TE.t
val inside_handlers_of_recursive_continuations_at_use : t -> Scope.Set.t
