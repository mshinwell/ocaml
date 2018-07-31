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
  module Flambda_type0_core : sig type t end
  module Join_env : sig type t end
  module Typing_env : sig type t end

  type t

  include Contains_names.S with type t := t

  (** Perform invariant checks upon the given function type. *)
  val invariant : t -> unit

  (** Create a representation of the names, order and type of a function's
      parameters. *)
  val create : (Kinded_parameter.t * Flambda_type0_core.t) list -> t

  (** A conservative approximation to equality. *)
  val equal : t -> t -> bool

  (** Greatest lower bound of two parameter lists. *)
  val meet : Typing_env.t -> t -> t -> t Or_bottom.t

  (** Least upper bound of two parameter lists. *)
  val join : Join_env.t -> t -> t -> t
end
