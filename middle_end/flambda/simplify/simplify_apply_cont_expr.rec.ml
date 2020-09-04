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

let rebuild_apply_cont apply_cont ~args ~rewrite_id uacc
      ~after_rebuild =
  let uenv = UA.uenv uacc in
  let rewrite =
    UE.find_apply_cont_rewrite uenv (AC.continuation apply_cont)
  in
  let cont =
    UE.resolve_continuation_aliases uenv (AC.continuation apply_cont)
  in
  let rewrite_use_result =
    let apply_cont = AC.update_continuation_and_args apply_cont cont ~args in
    let apply_cont =
      match AC.trap_action apply_cont with
      | None -> apply_cont
      | Some (Push { exn_handler; } | Pop { exn_handler; _ }) ->
        if UE.mem_continuation uenv exn_handler then apply_cont
        else AC.clear_trap_action apply_cont
    in
    match rewrite with
    | None -> Apply_cont_rewrite.no_rewrite apply_cont
    | Some rewrite ->
      Apply_cont_rewrite.rewrite_use rewrite rewrite_id apply_cont
    in
  let check_arity_against_args ~arity:_ = () in
  let normal_case () =
    match rewrite_use_result with
    | Apply_cont apply_cont ->
      after_rebuild (Expr.create_apply_cont apply_cont) uacc
    | Expr expr -> after_rebuild expr uacc
  in
  match UE.find_continuation uenv cont with
  | Unknown { arity; handler = _; } ->
    check_arity_against_args ~arity;
    normal_case ()
  | Unreachable { arity; } ->
    check_arity_against_args ~arity;
    (* N.B. We allow this transformation even if there is a trap action,
        on the basis that there wouldn't be any opportunity to collect any
        backtrace, even if the [Apply_cont] were compiled as "raise". *)
    Expr.create_invalid (), uacc
  | Inline { arity; handler; } ->
    match rewrite_use_result with
    | Expr _ ->
      (* CR-someday mshinwell: Consider supporting inlining in the case of
         a non-trivial wrapper. *)
      normal_case ()
    | Apply_cont apply_cont ->
      (* CR mshinwell: With -g, we can end up with continuations that are
         just a sequence of phantom lets then "goto".  These would normally
         be treated as aliases, but of course aren't in this scenario,
         unless the continuations are used linearly. *)
      (* CR mshinwell: maybe instead of [Inline] it should say "linearly used"
         or "stub" -- could avoid resimplification of linearly used ones
         maybe, although this wouldn't remove any parameter-to-argument
         [Let]s. However perhaps [Flambda_to_cmm] could deal with these. *)
      check_arity_against_args ~arity;
      match AC.trap_action apply_cont with
      | Some _ ->
        (* Until such time as we can manually add to the backtrace buffer,
           never substitute a "raise" for the body of an exception handler. *)
        normal_case ()
      | None ->
        Flambda.Continuation_params_and_handler.pattern_match
          (Flambda.Continuation_handler.params_and_handler handler)
          ~f:(fun params ~handler ->
            (* CR mshinwell: Why does [New_let_binding] have a [Variable]? *)
            (* CR mshinwell: Should verify that names in the
               [Apply_cont_rewrite] are in scope. *)
            (* We can't easily call [simplify_expr] on the inlined body since
               [dacc] isn't the correct accumulator and environment any more.
               However there's no need to simplify the inlined body except to
               make use of parameter-to-argument bindings; we just leave them
               for a subsequent round of [Simplify] or [Un_cps] to clean
               up. *)
            let args = Apply_cont.args apply_cont in
            let params_and_args =
              assert (List.compare_lengths params args = 0);
              List.map (fun (param, arg) -> param, Named.create_simple arg)
                (List.combine params args)
            in
            let expr =
              Expr.bind_parameters ~bindings:params_and_args ~body:handler
            in
            after_rebuild expr uacc)

let simplify_apply_cont dacc apply_cont ~down_to_up =
  let min_name_mode = Name_mode.normal in
  match S.simplify_simples dacc (AC.args apply_cont) ~min_name_mode with
  | _, Bottom ->
    down_to_up dacc ~rebuild:Simplify_common.rebuild_invalid
  | _changed, Ok args_with_types ->
    let args, arg_types = List.split args_with_types in
(* CR mshinwell: Resurrect arity checks
    let args_arity = T.arity_of_list arg_types in
*)
    let use_kind : Continuation_use_kind.t =
      (* CR mshinwell: Is [Continuation.sort] reliable enough to detect
         the toplevel continuation?  Probably not -- we should store it in
         the environment. *)
      match Continuation.sort (AC.continuation apply_cont) with
      | Normal ->
        if Option.is_none (Apply_cont.trap_action apply_cont) then Inlinable
        else Non_inlinable
      | Return | Toplevel_return | Exn -> Non_inlinable
      | Define_root_symbol ->
        assert (Option.is_none (Apply_cont.trap_action apply_cont));
        Inlinable
    in
    let dacc, rewrite_id =
      DA.record_continuation_use dacc
        (AC.continuation apply_cont)
        use_kind
        ~env_at_use:(DA.denv dacc)
        ~arg_types
    in
    down_to_up dacc ~rebuild:(rebuild_apply_cont apply_cont ~args ~rewrite_id)
