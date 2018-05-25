(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** The interface of "join environments": structures which keep track of
    the various environments and environment extensions that are required
    whilst carrying out join operations (on types, etc). *)

module type S = sig
  type env_extension
  type typing_environment
  type join_env
  type flambda_type

  type t = join_env

  (** Perform various invariant checks upon the given join environment. *)
  val invariant : t -> unit

  val create : typing_environment -> t

  val add_extensions
     : t
    -> holds_for_meet_or_join:env_extension
    -> holds_on_left:env_extension
    -> holds_on_right:env_extension
    -> t

  val add_definition_all_environments : t -> Name.t -> flambda_type -> t

  val add_definition_left_environment : t -> Name.t -> flambda_type -> t

  val add_definition_right_environment : t -> Name.t -> flambda_type -> t

  val joined_environment : t -> typing_environment

  val environment_on_left : t -> typing_environment

  val environment_on_right : t -> typing_environment

  val holds_on_left : t -> env_extension

  val holds_on_right : t -> env_extension

  val fast_check_extensions_same_both_sides : t -> bool
end
