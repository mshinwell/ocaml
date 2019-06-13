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
module K = Flambda_kind

let inline dacc ~callee ~args function_decl ~unroll_to
      ~apply_return_continuation ~apply_exn_continuation
      ~apply_inlining_depth dbg =
  let denv = DA.denv dacc in
  Function_params_and_body.pattern_match
    (Function_declaration.params_and_body function_decl)
    ~f:(fun ~return_continuation exn_continuation params ~body
            ~my_closure ->
      let denv = DE.add_inlined_debuginfo denv dbg in
      let denv =
        DE.set_inlining_depth_increment denv apply_inlining_depth
      in
      let rec_info = Rec_info.create ~depth:1 ~unroll_to in
      let my_closure =
        Simple.rec_info (Named.create_simple callee) rec_info
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
              (Expr.create_let my_closure K.value my_closure body)))
      in
      Some (DA.with_denv dacc denv, expr))
