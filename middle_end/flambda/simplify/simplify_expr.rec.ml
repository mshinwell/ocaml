(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

(* CR mshinwell: Need to simplify each [dbg] we come across. *)
(* CR mshinwell: Consider defunctionalising to remove the [k]. *)
(* CR mshinwell: May in any case be able to remove the polymorphic recursion. *)
(* CR mshinwell: See whether resolution of continuation aliases can be made
   more transparent (e.g. through [find_continuation]).  Tricky potentially in
   conjunction with the rewrites. *)

let simplify_expr dacc expr k =
  match Expr.descr expr with
  | Let let_expr ->
    Simplify_let_expr.simplify_let dacc let_expr k
  | Let_cont let_cont ->
    Simplify_let_cont_expr.simplify_let_cont dacc let_cont k
  | Apply apply ->
    Simplify_apply_expr.simplify_apply dacc apply k
  | Apply_cont apply_cont ->
    Simplify_apply_cont_expr.simplify_apply_cont dacc apply_cont k
  | Switch switch ->
    Simplify_switch_expr.simplify_switch dacc switch k
  | Invalid _ ->
    (* CR mshinwell: Make sure that a program can be simplified to just
       [Invalid].  [Un_cps] should translate any [Invalid] that it sees as if
       it were [Halt_and_catch_fire]. *)
    let user_data, uacc = k dacc in
    expr, user_data, uacc
