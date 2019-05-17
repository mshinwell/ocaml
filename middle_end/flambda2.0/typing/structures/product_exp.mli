(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make
  (Index : Name_like_intf.S)
  (Component : sig
    include Name_like_intf.S
    val create : Flambda_kind.t -> t
    val equal
       : Type_equality_env.t
      -> t
      -> t
      -> bool
    val name : t -> Name.t
    val kind : t -> Flambda_kind.t
  end) :
sig
  type t

  include Contains_names.S with type t := t

  (** Perform invariant checks upon the given product. *)
  val invariant : t -> unit

  (** Format the given product value as an s-expression. *)
  val print : Format.formatter -> t -> unit

  val print_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit

  (** Create a product value given the indexes with associated components. *)
  val create : Component.t Index.Map.t -> t

  (** The least product value. *)
  val create_bottom : unit -> t

  (** A conservative approximation to equality. *)
  val equal
     : Type_equality_env.t
    -> Type_equality_result.t
    -> t
    -> t
    -> Type_equality_result.t

  (** Ensure that the given product contains components for
      all indexes less than or equal to the given index. *)
  val widen : t -> to_match:t -> t

  (** Greatest lower bound of two products. *)
  val meet
     : Meet_env.t
    -> t
    -> t
    -> (t * Typing_env_extension.t) Or_bottom.t

  (** Least upper bound of two products. *)
  val join : Join_env.t -> t -> t -> t
end
