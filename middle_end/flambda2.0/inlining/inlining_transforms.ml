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

module E = Simplify_env_and_result.Env
module T = Flambda_type

let inline env ~callee ~args function_decl_type
      ~apply_return_continuation ~apply_exn_continuation
      dbg (inline : Inline_attribute.t) =
  match inline with
  | Never_inline -> None
  | Default_inline | Unroll _ | Always_inline ->
    match T.term_language_function_declaration function_decl_type with
    | None -> None
    | Some function_decl ->
      Function_params_and_body.pattern_match
        (Function_declaration.params_and_body function_decl)
        ~f:(fun ~return_continuation exn_continuation params ~body
                ~my_closure ->
          let env =
            E.disable_function_inlining (E.add_inlined_debuginfo env dbg)
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
                  (Expr.create_let my_closure K.value (Named.simple callee)
                    body)))
          in
          Some (env, expr))
