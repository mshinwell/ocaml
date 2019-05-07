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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module Thing_without_names : Identifiable.S

  type t

  val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

  (** Create a value which describes the presence of exactly no things. *)
  val create_bottom : unit -> t

  (** Create a value which describes the presence of an unknown set of
      things. *)
  val create_unknown : unit -> t

  val create : Thing_without_names.Set.t -> t

  val create_with_equations
     : Typing_env_extension.t Thing_without_names.Map.t
    -> t

  val equal
     : Type_equality_env.t
    -> Type_equality_result.t
    -> t
    -> t
    -> Type_equality_result.t

  val meet
     : Meet_env.t
    -> t
    -> t
    -> (t * Typing_env_extension.t) Or_bottom.t

  val join
     : Join_env.t
    -> t
    -> t
    -> t

  val all : t -> Thing_without_names.Set.t Or_unknown.t

  val get_singleton
     : t
    -> (Thing_without_names.t * Typing_env_extension.t) option

  val classify : t -> unit Or_unknown_or_bottom.t

  include Contains_names.S with type t := t
end
