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
  type env_extension
  type typing_environment
  type flambda_type

  type t = env_extension

  val invariant : t -> unit

  val create : unit -> t

  val add
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

  type binding_type = Normal | Existential

  val fold
     : t
    -> init:'a
    -> f:('a
      -> Name.t
      -> binding_type
      -> Scope_level.With_sublevel.t
      -> flambda_type
      -> 'a)
    -> 'a

  val iter
     : t
    -> f:(Name.t
      -> binding_type
      -> Scope_level.With_sublevel.t
      -> flambda_type
      -> unit)
    -> unit

  val domain : t -> Name.Set.t

  val meet : resolver:(Export_id.t -> flambda_type option) -> t -> t -> t

  val equal
     : equal_type:(flambda_type -> flambda_type -> bool)
    -> t
    -> t
    -> bool

  val phys_equal : t -> t -> bool

  val remove : t -> Name.t -> t

  val to_typing_environment
     : resolver:(Export_id.t -> flambda_type option)
    -> t
    -> typing_environment

  val print : Format.formatter -> t -> unit
end
