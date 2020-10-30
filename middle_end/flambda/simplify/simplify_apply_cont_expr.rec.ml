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

let rebuild_apply_cont apply_cont ~args ~rewrite_id uacc ~after_rebuild =
  let uenv = UA.uenv uacc in
  let cont = AC.continuation apply_cont in
  let rewrite = UE.find_apply_cont_rewrite uenv cont in
  let cont = UE.resolve_continuation_aliases uenv cont in
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
  let expr_and_uacc =
    match UE.find_continuation uenv cont with
    | Linearly_used_and_inlinable { arity; params; handler;
        free_names_of_handler; } ->
      begin match rewrite_use_result with
      | Expr _ ->
        (* CR-someday mshinwell: Consider supporting inlining in the case of
           a non-trivial wrapper. *)
        None
      | Apply_cont apply_cont ->
        (* CR mshinwell: With -g, we can end up with continuations that are
           just a sequence of phantom lets then "goto".  These would normally
           be treated as aliases, but of course aren't in this scenario,
           unless the continuations are used linearly. *)
        match AC.trap_action apply_cont with
        | Some _ ->
          (* Until such time as we can manually add to the backtrace buffer,
             never substitute a "raise" for the body of an exception handler. *)
          None
        | None ->
          (* We can't easily call [simplify_expr] on the inlined body since
             [dacc] isn't the correct accumulator and environment any more.
             However there's no need to simplify the inlined body except to
             make use of parameter-to-argument bindings; we just leave them for
             a subsequent round of [Simplify] or [Un_cps] to clean up. *)
          let bindings_outermost_first =
            ListLabels.map2 params (AC.args apply_cont)
              ~f:(fun param arg ->
                let bound =
                  Var_in_binding_pos.create (KP.var param) Name_mode.normal
                  |> Bindable_let_bound.singleton
                in
                bound, Simplified_named.reachable (Named.create_simple arg))
          in
          let name_occurrences = UA.name_occurrences uacc in
          let expr, uacc =
            let uacc =
              UA.with_name_occurrences uacc
                ~name_occurrences:free_names_of_handler
            in
            Expr_builder.make_new_let_bindings uacc ~bindings_outermost_first
              ~body:handler
          in
          let name_occurrences =
            Name_occurrences.union name_occurrences (UA.name_occurrences uacc)
          in
          let uacc = UA.with_name_occurrences uacc ~name_occurrences in
          Some (expr, uacc)
      end
    | Other { arity; handler = _; } -> None
    | Unreachable { arity; } ->
      (* N.B. We allow this transformation even if there is a trap action,
        on the basis that there wouldn't be any opportunity to collect any
        backtrace, even if the [Apply_cont] were compiled as "raise". *)
      Some (Expr.create_invalid (), uacc)
  in
  match expr_and_uacc with
  | Some (expr, uacc) -> after_rebuild expr uacc
  | None ->
    match rewrite_use_result with
    | Apply_cont apply_cont ->
      let uacc = UA.add_free_names uacc (AC.free_names apply_cont) in
      after_rebuild (Expr.create_apply_cont apply_cont) uacc
    | Expr (expr, free_names) ->
      let uacc = UA.add_free_names uacc free_names in
      after_rebuild expr uacc

let simplify_apply_cont dacc apply_cont ~down_to_up =
  let min_name_mode = Name_mode.normal in
  match S.simplify_simples dacc (AC.args apply_cont) ~min_name_mode with
  | _, Bottom ->
    down_to_up dacc ~rebuild:Simplify_common.rebuild_invalid
  | _changed, Ok args_with_types ->
    let args, arg_types = List.split args_with_types in
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
      DA.record_continuation_use dacc (AC.continuation apply_cont)
        use_kind ~env_at_use:(DA.denv dacc) ~arg_types
    in
    down_to_up dacc ~rebuild:(rebuild_apply_cont apply_cont ~args ~rewrite_id)
