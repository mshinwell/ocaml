(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

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

module Make(Variable : Container_types.S)
  : S with type variable = Variable.t
= struct
  type variable = Variable.t
  type t =
    | Initial
    | Var of Variable.t
    | Succ of t
    | Unroll_to of int * t

  let initial = Initial
  let var dv = Var dv
  let succ t = Succ t
  let unroll_to unroll_depth t = Unroll_to (unroll_depth, t)

  let is_obviously_initial = function
    | Initial -> true
    | _ -> false

  let rec equal t1 t2 =
    match t1, t2 with
    | Initial, Initial -> true
    | Var dv1, Var dv2 ->
      Variable.equal dv1 dv2
    | Succ t1, Succ t2 ->
      equal t1 t2
    | Unroll_to (unroll_depth1, t1), Unroll_to (unroll_depth2, t2) ->
      unroll_depth1 = unroll_depth2 && equal t1 t2
    | (Initial | Var _ | Succ _ | Unroll_to _), _ -> false
end
