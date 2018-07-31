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
  module Flambda_type : sig type t end
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end
  module Thing_without_names : Map.With_set

  type t

  val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val create : Flambda_type.t Closure_id.Map.t -> t

  (** Greatest lower bound of two values of type [t]. *)
  val meet
     : Typing_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> t
    -> t
    -> t Or_bottom.t

  (** Least upper bound of two values of type [t]. *)
  val join
     : Join_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> t
    -> t
    -> t

  include Contains_names.S with type t := t
end
