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
  module Make_types (Tag: sig end) (Index : sig end) (Maps_to : sig end) : sig
    type t
  end
end

module type S = sig
  module T : Typing_world_abstract.S
  module Functor_T : Typing_world_abstract.Functor_S
  include module type of struct include Functor_T.Row_like end

  module Make
    (Tag : sig
      (** These values may not contain names. *)
      type t
      include Hashtbl.With_map with type t := t
    end)
    (Index : sig
      (** These values may not contain names. *)
      type t
      include Hashtbl.With_map with type t := t
    end)
    (Maps_to : sig
      type t

      val bottom : unit -> t

      val print_with_cache
         : cache:Printing_cache.t
        -> Format.formatter
        -> t
        -> unit

      val equal : Type_equality_env.t -> t -> t -> bool

      val add_or_meet_equations
         : t
        -> Meet_env.t
        -> Typing_env_extension.t
        -> t

      val meet
         : Meet_env.t
        -> Relational_product_intf.fresh_component_semantics
        -> t
        -> t
        -> (t * Typing_env_extension.t) Or_bottom.t

      val join
         : Join_env.t
        -> Relational_product_intf.fresh_component_semantics
        -> t
        -> t
        -> t

      include Contains_names.S with type t := t
    end) :
  sig
    include module type of Make_types (Tag) (Index) (Maps_to)

    module Tag_and_index : sig
      (** These values will not contain any names. *)
      type t = Tag.t * Index.t

      include Map.With_set with type t := t
    end

    val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

    val create : unit -> t

    val create_exactly : Tag.t -> Index.t -> Maps_to.t -> t

    val create_exactly_multiple : Maps_to.t Tag_and_index.Map.t -> t

    val create_at_least : Index.t -> Maps_to.t -> t

    val create_at_least_multiple : Maps_to.t Index.Map.t -> t

    val is_bottom : t -> bool

    val equal : Type_equality_env.t -> t -> t -> bool

    (** The [Maps_to] value which [meet] returns contains the join of all
        [Maps_to] values in the range of the row-like structure after the meet
        operation has been completed. *)
    val meet
       : T.Meet_env.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> (t * Maps_to.t) Or_bottom.t

    val join
       : T.Join_env.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> t

    include Contains_names.S with type t := t
  end
end
