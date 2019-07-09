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
module T = Flambda_type
module VB = Var_in_binding_pos

let inline dacc ~callee ~args function_decl
      ~apply_return_continuation ~apply_exn_continuation
      ~apply_inlining_depth ~unroll_to dbg =
  Function_params_and_body.pattern_match
    (Function_declaration.params_and_body function_decl)
    ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure ->
      let denv = DA.denv dacc in
      let typing_env = DE.typing_env denv in
      let canonical_callee, _ =
        T.Typing_env.get_canonical_simple typing_env callee
          ~min_occurrence_kind:Name_occurrence_kind.normal
      in
      (* CR mshinwell: Shouldn't need this---instead set the type during
         lifting of the closure symbol to have [Rec_info] on it *)
      match canonical_callee with
      | Bottom -> dacc, Expr.create_invalid ()
      | Ok canonical_callee ->
        (* CR mshinwell: Move to [Typing_env.map_type_of_canonical] or
           similar.  (Can we actually hide [find] from the Typing_env
           external interface?) *)
        let typing_env : _ Or_bottom.t =
          match Simple.to_name canonical_callee with
          | None -> Ok typing_env
          | Some (rec_info, callee_name) ->
            let callee_ty = T.Typing_env.find typing_env callee_name in
            let newer_rec_info = Rec_info.create ~depth:1 ~unroll_to in
            let rec_info =
              match rec_info with
              | None -> newer_rec_info
              | Some rec_info -> Rec_info.merge rec_info ~newer:newer_rec_info
            in
            Or_bottom.map (T.apply_rec_info callee_ty rec_info)
              ~f:(fun callee_ty ->
                T.Typing_env.add_equation typing_env callee_name callee_ty)
        in
        match typing_env with
        | Bottom -> dacc, Expr.create_invalid ()
        | Ok typing_env ->
          let denv = DE.with_typing_environment denv typing_env in
          let denv =
            DE.set_inlining_depth_increment
              (DE.add_inlined_debuginfo denv dbg)
              apply_inlining_depth
          in

    (*
            T.Typing_env.merge_rec_info_for_simple_and_all_aliases
              (DE.typing_env denv)
              (Simple.descr callee)
              ~newer_rec_info:(Rec_info.create ~depth:1 ~unroll_to)
    *)
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
                    (Named.create_simple callee)
                    body)))
          in
          DA.with_denv dacc denv, expr)
