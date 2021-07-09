(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module type S = sig
  type variable

  (** An expression for the state of recursive inlining at a given occurrence.
      Forms the right-hand side of a [Let_expr] binding for a depth variable. Will
      evaluate (given a suitable environment) to a [Rec_info.t]. *)
  type t =
    | Initial
      (** The initial recursion depth. In user code, all occurrences have depth
          zero. *)
    | Var of variable
      (** A variable of kind [Flambda_kind.rec_info]. *)
    | Succ of t
      (** The next depth. If we inline an occurrence with depth [d], then in the
          inlined body, recursive references will have depth [succ d]. *)
    | Unroll_to of int * t
      (** Indicate the depth to which unrolling should proceed. The unroll depth
          is decremented by [Succ] until it reaches zero, at which
          point all unrolling should stop. *)

  val initial : t
  val var : variable -> t
  val succ : t -> t
  val unroll_to : int -> t -> t

  val is_obviously_initial : t -> bool

  val equal : t -> t -> bool
end

module Make(Variable : Container_types.S) : S with type variable = Variable.t
