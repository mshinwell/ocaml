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
  module Flambda_type0_core : sig type t end
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end
  module Thing_without_names : Map.With_set

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

module type S = sig
  module Flambda_type0_core : sig type t end
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  module Make (Thing_without_names : Hashtbl.With_map)
    : S_applied
        with module Flambda_type0_core := Flambda_type0_core
        with module Join_env := Join_env
        with module Thing_without_names := Thing_without_names
        with module Typing_env := Typing_env
        with module Typing_env_extension := Typing_env_extension
end
