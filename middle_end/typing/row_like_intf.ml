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

module type S = sig
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  module Tag : sig
    type t
  end

  module Index : sig
    type t

    val equal : t -> t -> bool
    include Map.With_set with type t := t
  end

  module Tag_and_index : sig
    type t = Tag.t * Index.t

    include Map.With_set with type t := t
    include Contains_names.S with type t := t
  end

  module Maps_to : sig
    type t

    val add_or_meet_equations
       : t
      -> Typing_env.t
      -> Typing_env_extension.t
      -> t

    include Contains_names.S with type t := t
  end

  type t

  val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val create : unit -> t

  val create_exactly : Tag.t -> Index.t -> Maps_to.t -> t

  val create_exactly_multiple : Maps_to.t Tag_and_index.Map.t -> t

  val create_at_least : Index.t -> Maps_to.t -> t

  val create_at_least_multiple : Maps_to.t Index.Map.t -> t

  val create_unknown : unit -> t

  val is_bottom : t -> bool

  val get_singleton : t -> Maps_to.t option

  val meet
     : Typing_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> t
    -> t
    -> t Or_bottom.t

  val join
     : Join_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> t
    -> t
    -> t

  include Contains_names.S with type t := t
end
