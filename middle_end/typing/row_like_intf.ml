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

module type S_applied = sig
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  module Tag : Hashtbl.With_map
  module Index : Hashtbl.With_map

  module Tag_and_index : sig
    type t = Tag.t * Index.t

    include Map.With_set with type t := t
    include Contains_names.S with type t := t
  end

  module Maps_to : sig type t end

  type t

  val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val create : unit -> t

  val create_exactly : Tag.t -> Index.t -> Maps_to.t -> t

  val create_exactly_multiple : Maps_to.t Tag_and_index.Map.t -> t

  val create_at_least : Index.t -> Maps_to.t -> t

  val create_at_least_multiple : Maps_to.t Index.Map.t -> t

  val is_bottom : t -> bool

  val get_singleton : t -> Maps_to.t option

  (** The [Maps_to] value which [meet] returns contains the join of all
      [Maps_to] values in the range of the row-like structure after the meet
      operation has been completed. *)
  val meet
     : Typing_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> Relational_product_intf.fresh_component_semantics
    -> t
    -> t
    -> (t * Maps_to.t) Or_bottom.t

  val join
     : Join_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> Relational_product_intf.fresh_component_semantics
    -> t
    -> t
    -> t

  include Contains_names.S with type t := t
end

module type S = sig
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  module Make
    (Tag : sig
      type t
      include Hashtbl.With_map with type t := t
      include Contains_names.S with type t := t
    end)
    (Index : sig
      type t
      include Hashtbl.With_map with type t := t
      include Contains_names.S with type t := t
    end)
    (Maps_to : sig
      type t

      val print_with_cache
         : cache:Printing_cache.t
        -> Format.formatter
        -> t
        -> unit

      val add_or_meet_equations
         : t
        -> Typing_env.t
        -> Typing_env_extension.t
        -> t

      val meet
         : Typing_env.t
        -> Name_permutation.t
        -> Name_permutation.t
        -> Relational_product_intf.fresh_component_semantics
        -> t
        -> t
        -> t Or_bottom.t * Typing_env_extension.t

      val join
         : Join_env.t
        -> Name_permutation.t
        -> Name_permutation.t
        -> Relational_product_intf.fresh_component_semantics
        -> t
        -> t
        -> t

      include Contains_names.S with type t := t
    end) :
    S_applied
      with module Tag := Tag
      with module Index := Index
      with module Maps_to := Maps_to
      with module Join_env := Join_env
      with module Typing_env := Typing_env
      with module Typing_env_extension := Typing_env_extension
end
