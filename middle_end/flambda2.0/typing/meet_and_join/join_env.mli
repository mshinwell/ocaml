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

(** Environments used for join operations. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Perform various invariant checks upon the given join environment. *)
val invariant : t -> unit

val print : Format.formatter -> t -> unit

val create : Meet_env.t -> t

val add_definition
   : t
  -> Name.t
  -> Flambda_types.t
  -> t

(* CR mshinwell: Rename to [add_or_meet_extensions] *)
val add_extensions
   : t
  -> holds_on_left:Typing_env_extension.t
  -> holds_on_right:Typing_env_extension.t
  -> t

val add_opened_extensions
   : t
  -> holds_on_left:Typing_env_level.t
  -> holds_on_right:Typing_env_level.t
  -> t

(*
val add_extensions_and_extend_central_environment
   : t
  -> holds_on_left:Typing_env_extension.t
  -> holds_on_right:Typing_env_extension.t
  -> central_extension:Typing_env_extension.t
  -> t
*)

val environment : t -> Meet_env.t

val typing_environment : t -> Typing_env.t
