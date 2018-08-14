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
  include module type of struct include T.Closures_entry_by_closure_id end

  val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

  (** Describe one or more closures by giving for each one the closure ID
      and the set of variables in the closure. *)
  val create_exactly_multiple
     : T.Flambda_types.closures_entry
         Closure_id_and_var_within_closure_set.Map.t
    -> t

  (** Describe one or more closures that contain at least the given closure
      variables. *)
  val create_at_least_multiple
     : T.Flambda_types.closures_entry Var_within_closure_set.Map.t
    -> t

  val equal : Type_equality_env.t -> t -> t -> bool

  (** Greatest lower bound of two values of type [t]. *)
  val meet
     : T.Meet_env.t
    -> t
    -> t
    -> (t * T.Typing_env_extension.t) Or_bottom.t

  (** Least upper bound of two values of type [t]. *)
  val join
     : T.Join_env.t
    -> t
    -> t
    -> t

  include Contains_names.S with type t := t
end
