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
      (* XXX The following is a hack until we work out more of the theory
         about [Rec_info] *)
      let canonical_callee =
        T.Typing_env.get_canonical_simple typing_env callee
          ~min_name_mode:Name_mode.normal
      in
      match canonical_callee with
      | Bottom -> dacc, Expr.create_invalid ()
      | Ok None ->
        Misc.fatal_errorf "No canonical callee for %a" Simple.print callee
      | Ok (Some canonical_callee) ->
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
          let denv = DE.with_typing_env denv typing_env in
          let denv =
            DE.set_inlining_depth_increment
              (DE.add_inlined_debuginfo denv dbg)
              apply_inlining_depth
          in
          let perm =
            Name_permutation.add_continuation
              (Name_permutation.add_continuation Name_permutation.empty
                return_continuation apply_return_continuation)
              (Exn_continuation.exn_handler exn_continuation)
              (Exn_continuation.exn_handler apply_exn_continuation)
          in
          let expr =
            Expr.apply_name_permutation
              (Expr.bind_parameters_to_simples ~bind:params ~target:args
                (Expr.create_let
                  (VB.create my_closure Name_mode.normal)
                  (Named.create_simple callee)
                  body))
              perm
          in
(*
  Format.eprintf "Inlined body to be simplified:@ %a\n%!" Expr.print expr;
*)
          DA.with_denv dacc denv, expr)
