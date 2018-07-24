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
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end
  module Join_env : sig type t end

  module Size = Targetint.OCaml

  type t

  val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val create : unit -> t

  val create_with_known_types : Tag.t -> Flambda_type.t list -> t

  val create_size_at_least : Size.t -> t

  val is_empty : t -> bool

  val meet
     : Typing_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> t
    -> t
    -> t

  val join
     : Join_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> t
    -> t
    -> t

  include Contains_names.S with type t := t
end
