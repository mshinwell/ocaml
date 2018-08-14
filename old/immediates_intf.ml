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
  include module type of struct include T.Immediates end

  (* CR mshinwell: Work out how to remove duplication between here,
     [Immediates] and [Trivial_row_like_intf]. *)

  val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

  (** Create a value which describes the presence of exactly no things. *)
  val create_bottom : unit -> t

  (** Create a value which describes the presence of an unknown set of
      things. *)
  val create_unknown : unit -> t

  val create : Immediate.Set.t -> t

  val create_with_equations
     : T.Typing_env_extension.t Immediate.Map.t
    -> t

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
