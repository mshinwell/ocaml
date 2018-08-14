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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S_types = sig
  module T : Typing_world_abstract.S
  module Functor_T : Typing_world_abstract.Functor_S
  type t
end

module type S = sig
  module T : Typing_world_abstract.S
  module Functor_T : Typing_world_abstract.Functor_S
  include module type of struct include T.Blocks end

  type open_or_closed = Open | Closed of Tag.t

  (** Create a value which describes that there are exactly no blocks. *)
  val create_bottom : unit -> t

  val create : field_tys:T.Flambda_types.t list -> open_or_closed -> t

  val invariant : t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val equal : Type_equality_env.t -> t -> t -> bool

  val meet
     : T.Meet_env.t
    -> t
    -> t
    -> (t * T.Typing_env_extension.t) Or_bottom.t

  val join
     : T.Join_env.t
    -> t
    -> t
    -> t

  include Contains_names.S with type t := t
end
