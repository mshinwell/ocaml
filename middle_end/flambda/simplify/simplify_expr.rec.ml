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

let simplify_expr dacc expr ~down_to_up =
  match Expr.descr expr with
  | Let let_expr ->
    Simplify_let_expr.simplify_let dacc let_expr ~down_to_up
  | Let_cont let_cont ->
    Simplify_let_cont_expr.simplify_let_cont dacc let_cont ~down_to_up
  | Apply apply ->
    Simplify_apply_expr.simplify_apply dacc apply ~down_to_up
  | Apply_cont apply_cont ->
    Simplify_apply_cont_expr.simplify_apply_cont dacc apply_cont ~down_to_up
  | Switch switch ->
    Simplify_switch_expr.simplify_switch dacc switch ~down_to_up
  | Invalid _ ->
    (* CR mshinwell: Make sure that a program can be simplified to just
       [Invalid].  [Un_cps] should translate any [Invalid] that it sees as if
       it were [Halt_and_catch_fire]. *)
    down_to_up dacc ~rebuild:Simplify_common.rebuild_invalid

let simplify_expr dacc expr ~down_to_up =
  (* XXX Temporary debugging code, to be removed *)
  match Sys.getenv "FREE_NAMES" with
  | exception Not_found -> simplify_expr dacc expr ~down_to_up
  | _ ->
    simplify_expr dacc expr
      ~down_to_up:(fun dacc ~rebuild ->
        down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
          rebuild uacc ~after_rebuild:(fun expr uacc ->
            let free_names_uacc =
              UA.name_occurrences uacc
              |> Name_occurrences.without_closure_vars
            in
            let free_names =
              Expr.free_names expr
              |> Name_occurrences.without_closure_vars
            in
            if not (Name_occurrences.equal free_names free_names_uacc)
            then begin
              Misc.fatal_errorf "Mismatch on free names:@ \n\
                  From UA:@ %a@ \n\
                  From expr:@ %a@ \n\
                  Expression:@ %a"
                Name_occurrences.print free_names_uacc
                Name_occurrences.print free_names
                Expr.print expr
            end;
            after_rebuild expr uacc)))
