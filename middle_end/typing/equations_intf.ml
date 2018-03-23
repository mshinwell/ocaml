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

module type S = sig
  type equations
  type typing_environment
  type flambda_type

  type t = equations

  val invariant : t -> unit

  val create : unit -> t

  val add
     : resolver:(Export_id.t -> flambda_type option)
    -> t
    -> Name.t
    -> Scope_level.t
    -> flambda_type
    -> t

  val add_or_replace_meet
     : resolver:(Export_id.t -> flambda_type option)
    -> t
    -> Name.t
    -> Scope_level.t
    -> flambda_type
    -> t

  val singleton
     : resolver:(Export_id.t -> flambda_type option)
    -> Name.t
    -> Scope_level.t
    -> flambda_type
    -> t

  val fold
     : t
    -> init:'a
    -> f:('a -> Name.t -> Scope_level.t -> flambda_type -> 'a)
    -> 'a

  val domain : t -> Name.Set.t

  val meet : resolver:(Export_id.t -> flambda_type option) -> t -> t -> t

  val equal
     : equal_type:(flambda_type -> flambda_type -> bool)
    -> t
    -> t
    -> bool

  val remove : t -> Name.t -> t

  val to_typing_environment
     : resolver:(Export_id.t -> flambda_type option)
    -> t
    -> typing_environment

  val print : Format.formatter -> t -> unit
end
