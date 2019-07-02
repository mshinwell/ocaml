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

open! Flambda.Import

module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module VB = Var_in_binding_pos

let inline dacc ~callee ~args function_decl
      ~apply_return_continuation ~apply_exn_continuation
      ~apply_inlining_depth ~unroll_to dbg =
  let newer_rec_info = Some (Rec_info.create ~depth:1 ~unroll_to) in
Format.eprintf "Inlining callee %a\n%!" Simple.print callee;
  match Simple.merge_rec_info callee ~newer_rec_info with
  | None -> dacc, Expr.create_invalid ()
  | Some callee_with_rec_info ->
    Function_params_and_body.pattern_match
      (Function_declaration.params_and_body function_decl)
      ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure ->
        let denv =
          DE.set_inlining_depth_increment
            (DE.add_inlined_debuginfo (DA.denv dacc) dbg)
            apply_inlining_depth
        in
        let expr =
          Expr.link_continuations
            ~bind:return_continuation
            ~target:apply_return_continuation
            ~arity:(Function_declaration.result_arity function_decl)
            (Expr.link_continuations
              ~bind:(Exn_continuation.exn_handler exn_continuation)
              ~target:(Exn_continuation.exn_handler apply_exn_continuation)
              ~arity:(Exn_continuation.arity exn_continuation)
              (Expr.bind_parameters_to_simples ~bind:params ~target:args
                (Expr.create_let
                  (VB.create my_closure Name_occurrence_kind.normal)
                  (Named.create_simple callee_with_rec_info)
                  body)))
        in
        DA.with_denv dacc denv, expr)
