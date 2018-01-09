(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2017 OCamlPro SAS                                    *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Approximations of continuations.  Used during simplification. *)

type t

type continuation_handlers =
  | Non_recursive of Flambda.Continuation_handler.t
  | Recursive of Flambda.Continuation_handlers.t

val create
   : name:Continuation.t
  -> handlers:continuation_handlers
  -> arity:Flambda_arity.t
  -> t

val create_unknown : name:Continuation.t -> arity:Flambda_arity.t -> t

val name : t -> Continuation.t

val arity : t -> Flambda_arity.t
val handlers : t -> continuation_handlers option

val is_alias : t -> Continuation.t option

val print : Format.formatter -> t -> unit
