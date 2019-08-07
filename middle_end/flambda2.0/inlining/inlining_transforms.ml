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

open! Simplify_import

let inline dacc ~callee ~args closure_id function_decl
      ~apply_return_continuation ~apply_exn_continuation
      ~apply_inlining_depth ~unroll_to dbg =
  let newer_rec_info = Some (Rec_info.create ~depth:1 ~unroll_to) in
Format.eprintf "About to inline: %a, newer_rec_info=%a\n%!"
  Simple.print callee
  (Misc.Stdlib.Option.print Rec_info.print) newer_rec_info;
  match Simple.merge_rec_info callee ~newer_rec_info with
  | None -> dacc, Expr.create_invalid ()
  | Some callee_with_rec_info ->
Format.eprintf "callee_with_rec_info now %a\n%!"
  Simple.print callee_with_rec_info;
    Function_params_and_body.pattern_match
      (Function_declaration.params_and_body function_decl)
      ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure
              ~irrelevant_closure_vars ~rec_info_var ->
        let denv =
          DE.set_inlining_depth_increment
            (DE.add_inlined_debuginfo (DA.denv dacc) dbg)
            apply_inlining_depth
        in
        let perm =
          Name_permutation.add_continuation
            (Name_permutation.add_continuation Name_permutation.empty
              return_continuation apply_return_continuation)
            (Exn_continuation.exn_handler exn_continuation)
            (Exn_continuation.exn_handler apply_exn_continuation)
        in
        let irrelevant_closure_bindings =
          List.map (fun (move_to, closure_var) ->
              let var = VB.create closure_var Name_occurrence_kind.in_types in
              let prim : P.t =
                Unary (Select_closure { move_from = closure_id; move_to; },
                  Simple.var my_closure)
              in
              var, Named.create_prim prim dbg)
            (Closure_id.Map.bindings irrelevant_closure_vars)
        in
        let get_rec_info =
          Named.create_prim (Unary (Get_rec_info, my_closure)) dbg
        in
        let expr =
          Expr.apply_name_permutation
            (Expr.bind_parameters_to_simples ~bind:params ~target:args
              (Expr.create_let
                (VB.create my_closure Name_occurrence_kind.normal)
                (Named.create_simple callee_with_rec_info)
                (Expr.create_let
                  (VB.create rec_info_var Name_occurrence_kind.in_types)
                  get_rec_info
                  (Expr.bind ~bindings:irrelevant_closure_bindings ~body))))
            perm
        in
Format.eprintf "Inlined body to be simplified:@ %a\n%!" Expr.print expr;
        DA.with_denv dacc denv, expr)
