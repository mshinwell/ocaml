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

(** Defines coercions and their basic operations. For us, a {i coercion} is a
    compile-time function on simple expressions that serves to add information
    to a type. A coercion must:

    + have an inverse (which is also a coercion), and
    + not alter the run-time value of its argument.

    Rule #1 allows us to treat a coerced name as an alias of the original name,
    since "is an alias of" remains an equivalence relation. Rule #2 means that a
    coerced [Simple.t] is still a valid [Simple.t], since it's still a register-
    sized value that we can substitute for free.

    Currently, we only use coercions to track the recursion depths of closures,
    but any type-level tweak that we want to attach to a term could be made a
    constructor of [t], so long as it follows the rules. *)

[@@@ocaml.warning "+a-30-40-41-42"]

module type S = sig
  type variable (* currently unused *)
  type rec_info_expr (* currently unused *)

  type t = private
    | Id
    | Non_id of {
        from_depth : int;
        to_depth : int;
      }

  val change_depth : from:int -> to_:int -> t

  val id : t

  val is_id : t -> bool

  val inverse : t -> t

  val compose : t -> then_:t -> t option

  val compose_exn : t -> then_:t -> t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val hash : t -> int

  val apply_to_rec_info : t -> Rec_info.t -> Rec_info.t
end

module Make(Rec_info_expr : Rec_info_expr0.S)
  : S with type variable = Rec_info_expr.variable
       and type rec_info_expr = Rec_info_expr.t
