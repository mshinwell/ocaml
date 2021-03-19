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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Computes an approximation for the code size corresponding to flambda terms.
   The code size of a given term should be a rough estimate of the size of
   the generated machine code.
*)

type t

val expr_size : find_code:(Code_id.t -> Code.t) -> Expr.t -> t

val zero : t
val to_int : t -> int
val (+) : t -> t -> t
val smaller_than_threshold : t -> threshold:int -> bool
val equal : t -> t -> bool
val print : Format.formatter -> t -> unit

val prim : Flambda_primitive.t -> t
val simple : Simple.t -> t
val set_of_closures : find_cost_metrics:(Code_id.t -> Cost_metrics.t Or_unknown.t) -> Set_of_closures.t -> t

val apply : Apply.t -> t
val apply_cont : Apply_cont.t -> t
val switch : Switch.t -> t
val invalid : unit -> t
val increase_due_to_let_expr : is_phantom:bool -> cost_metrics_of_defining_expr:t -> t
val increase_due_to_let_cont_non_recursive : cost_metrics_of_handler:t -> t
val increase_due_to_let_cont_recursive : cost_metrics_of_handlers:t -> t
