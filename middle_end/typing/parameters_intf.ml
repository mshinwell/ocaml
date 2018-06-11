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

(** The abstraction used to represent parameters of functions, return
    continuations, and so forth. *)

module type S = sig
  type env_extension
  type typing_environment
  type join_env
  type flambda_type
  type parameters

  type t = parameters

  val create : Kinded_parameter.t list -> t

  val create_with_env_extension
     : Kinded_parameter.t list
    -> env_extension
    -> t

  val kinded_params : t -> Kinded_parameter.t

  val env_extension : t -> env_extension

  val use_the_same_fresh_names
     : t
    -> t
    -> (Kinded_parameter.t list
      * env_extension
      * env_extension) option

  val introduce_definitions
     : ?freshening:Freshening.t
    -> t
    -> typing_environment
    -> t

  (** At the highest scope level in the given environment, introduce
      definitions for the parameters inside [t]; and then add the
      environment extension (using "meet") to yield another environment.
      (This will open any existentials in the extension.)
      Parameters and types will be freshened according to the provided
      [Freshening.t]. *)
  val introduce : ?freshening:Freshening.t -> t -> typing_environment -> t

  val freshened_params : t -> Freshening.t -> t

  val print : Format.formatter -> t -> unit

  val meet : join_env -> t -> t -> t option

  val join : join_env -> t -> t -> t option

  val join_and_introduce : join_env -> t -> t -> (t * join_env) option
end
