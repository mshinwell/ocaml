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

(** Dependent function types.

    Logical variables introduced by the parameter types may be used in the
    result types.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module Flambda_type0_core : sig type t end
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  type t

  include Contains_names.S with type t := t

  (** Perform invariant checks upon the given function type. *)
  val invariant : t -> unit

  (** Create a function type from parameter and result types. *)
  val create
     : parameters:Flambda_type0_core.t list
    -> results:Flambda_type0_core.t list
    -> t

  (** A conservative approximation to equality. *)
  val equal : t -> t -> bool

  (** Greatest lower bound of two function types.
      [fresh_component_semantics] is as for [Relational_product.meet]. *)
  val meet
     : Typing_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> Relational_product_intf.fresh_component_semantics
    -> t
    -> t
    -> t Or_bottom.t * Typing_env_extension.t

  (** Least upper bound of two function types. *)
  val join
     : Join_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> Relational_product_intf.fresh_component_semantics
    -> t
    -> t
    -> t

  (** Add or meet the definitions and equations from the given function type
      into the given typing environment. *)
  val introduce : t -> Typing_env.t -> Typing_env.t
end
