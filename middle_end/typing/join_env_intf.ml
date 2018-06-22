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
  module T : Flambda_type0_internal_intf.S

  type t

  (** Perform various invariant checks upon the given join environment. *)
  val invariant : t -> unit

  val create : T.Typing_env.t -> t

  val add_definition_central_environment : t -> Name.t -> T.t -> t

  (** E + [X1 | X2] *)
  val add_extensions
     : t
    -> holds_on_left:T.Typing_env_extension.t
    -> holds_on_right:T.Typing_env_extension.t
    -> t

  val add_extensions_and_extend_central_environment
     : t
    -> holds_on_left:T.Typing_env_extension.t
    -> holds_on_right:T.Typing_env_extension.t
    -> central_extension:T.Typing_env_extension.t
    -> t

  val central_environment : t -> T.Typing_env.t

  val environment_on_left : t -> T.Typing_env.t

  val environment_on_right : t -> T.Typing_env.t

  val holds_on_left : t -> T.Typing_env_extension.t

  val holds_on_right : t -> T.Typing_env_extension.t

  val fast_check_extensions_same_both_sides : t -> bool
end
