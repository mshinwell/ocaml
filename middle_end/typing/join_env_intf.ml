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
  module T : sig
    type env_extension
    type typing_environment
    type join_env
    type flambda_type
  end

  type t = T.join_env

  (** Perform various invariant checks upon the given join environment. *)
  val invariant : t -> unit

  val create : T.typing_environment -> t

  val add_definition_central_environment : t -> Name.t -> T.flambda_type -> t

  (** E + [X1 | X2] *)
  val add_extensions
     : t
    -> holds_on_left:T.env_extension
    -> holds_on_right:T.env_extension
    -> t

  val add_extensions_and_return_meet
     : t
    -> holds_on_left:T.env_extension
    -> holds_on_right:T.env_extension
    -> t * T.env_extension

  val add_extensions_and_return_join
     : t
    -> holds_on_left:T.env_extension
    -> holds_on_right:T.env_extension
    -> t * T.env_extension

  val central_environment : t -> T.typing_environment

  val environment_on_left : t -> T.typing_environment

  val environment_on_right : t -> T.typing_environment

  val holds_on_left : t -> T.env_extension

  val holds_on_right : t -> T.env_extension

  val fast_check_extensions_same_both_sides : t -> bool
end
