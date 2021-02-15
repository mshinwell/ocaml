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

let rebuild_one_continuation_handler cont ~at_unit_toplevel
      (recursive : Recursive.t) (cont_handler : CH.t) ~params
      ~(extra_params_and_args : EPA.t) ~is_single_inlinable_use handler uacc
      ~after_rebuild =
  let handler, uacc =
    let params = params @ extra_params_and_args.extra_params in
    (* We might need to place lifted constants now, as they could
       depend on continuation parameters.  As such we must also compute
       the unused parameters after placing any constants! *)
    if (not at_unit_toplevel)
      || List.compare_length_with params 0 = 0
    then handler, uacc
    else
      EB.place_lifted_constants uacc
        Dominator
        ~lifted_constants_from_defining_expr:LCS.empty
        ~lifted_constants_from_body:(UA.lifted_constants uacc)
        ~put_bindings_around_body:(fun uacc ~body -> body, uacc)
        ~body:handler
        ~critical_deps_of_bindings:(KP.List.free_names params)
  in
  let free_names = UA.name_occurrences uacc in
  let used_params =
    (* Removal of unused parameters of recursive continuations is not
       currently supported. *)
    match recursive with
    | Recursive -> params
    | Non_recursive ->
      (* If the continuation is going to be inlined out, we don't need to
         spend time here calculating unused parameters, since the creation of
         [Let]-expressions around the continuation's handler will do that
         anyway. *)
      if is_single_inlinable_use then params
      else
        let first = ref true in
        List.filter (fun param ->
            (* CR mshinwell: We should have a robust means of propagating which
               parameter is the exception bucket.  Then this hack can be
               removed. *)
            if !first && Continuation.is_exn cont then begin
              first := false;
              true
            end else begin
              first := false;
              Name_occurrences.mem_var free_names (KP.var param)
            end)
          params
  in
  let used_extra_params =
    if is_single_inlinable_use then extra_params_and_args.extra_params
    else
      List.filter (fun extra_param ->
          Name_occurrences.mem_var free_names (KP.var extra_param))
        extra_params_and_args.extra_params
  in
  let params' = used_params @ used_extra_params in
  let cont_handler =
    CH.create params' ~handler ~free_names_of_handler:(Known free_names)
      ~is_exn_handler:(CH.is_exn_handler cont_handler)
  in
  let rewrite =
    Apply_cont_rewrite.create ~original_params:params
      ~used_params:(KP.Set.of_list used_params)
      ~extra_params:extra_params_and_args.extra_params
      ~extra_args:extra_params_and_args.extra_args
      ~used_extra_params:(KP.Set.of_list used_extra_params)
  in
  let uacc =
    UA.map_uenv uacc ~f:(fun uenv ->
      UE.add_apply_cont_rewrite uenv cont rewrite)
  in
  after_rebuild cont_handler ~params:params' ~handler
    ~free_names_of_handler:free_names uacc

let simplify_one_continuation_handler dacc cont ~at_unit_toplevel recursive
      cont_handler ~params ~handler ~extra_params_and_args
      ~is_single_inlinable_use ~down_to_up =
  Simplify_expr.simplify_expr dacc handler
    ~down_to_up:(fun dacc ~rebuild ->
      down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
        (* The name occurrences component of this [uacc] is cleared (see
           further down this file) before simplifying a handler.  This is done
           so we can precisely identify the free names of the handler. *)
        assert (Name_occurrences.is_empty (UA.name_occurrences uacc));
        rebuild uacc ~after_rebuild:(fun handler uacc ->
          rebuild_one_continuation_handler cont ~at_unit_toplevel recursive
            cont_handler ~params ~extra_params_and_args
            ~is_single_inlinable_use handler uacc ~after_rebuild)))

let rebuild_non_recursive_let_cont_handler cont
      (uses : Continuation_env_and_param_types.t) ~params ~handler
      ~free_names_of_handler ~is_single_inlinable_use ~is_single_use scope
      (extra_params_and_args : EPA.t) cont_handler uacc ~after_rebuild =
  let uenv = UA.uenv uacc in
  let uenv =
    (* CR mshinwell: Change types so that [free_names_of_handler] only
       needs to be provided in the [Uses] case. *)
    match uses with
    | No_uses -> uenv
    | Uses _ ->
      (* We must make the final decision now as to whether to inline this
         continuation or not; we can't wait until
         [Simplify_apply_cont.rebuild_apply_cont] because we need to decide
         sooner than that whether to keep the [Let_cont] (in order to keep
         free name sets correct). *)
      if is_single_inlinable_use then begin
        (* Note that [Continuation_uses] won't set [is_single_inlinable_use]
           if [cont] is an exception handler. *)
        assert (not (CH.is_exn_handler cont_handler));
        let arity = CH.arity cont_handler in
        (* We pass the parameters and the handler expression, rather than
           the [CH.t], to avoid re-opening the name abstraction. *)
        UE.add_linearly_used_inlinable_continuation uenv cont scope arity
          ~params ~handler ~free_names_of_handler
      end else begin
        match CH.behaviour cont_handler with
        | Unreachable { arity; } ->
          UE.add_unreachable_continuation uenv cont scope arity
        | Alias_for { arity; alias_for; } ->
          UE.add_continuation_alias uenv cont arity ~alias_for
        | Unknown { arity; } ->
          if is_single_use then
            UE.add_continuation_with_handler uenv cont scope arity cont_handler
          else
            UE.add_continuation uenv cont scope arity
      end
  in
  (* The parameters are removed from the free name information as they are no
     longer in scope. *)
  let uacc =
    let name_occurrences =
      ListLabels.fold_left (params @ EPA.extra_params extra_params_and_args)
        ~init:(UA.name_occurrences uacc)
        ~f:(fun name_occurrences param ->
          KP.var param
          |> Name_occurrences.remove_var name_occurrences)
    in
    UA.with_name_occurrences uacc ~name_occurrences
  in
  after_rebuild cont_handler (UA.with_uenv uacc uenv)

let simplify_non_recursive_let_cont_handler ~denv_before_body ~dacc_after_body
      cont params ~(handler : Expr.t) cont_handler ~prior_lifted_constants
      ~inlining_state_at_let_cont ~inlined_debuginfo_at_let_cont
      ~scope ~is_exn_handler ~denv_for_toplevel_check ~unit_toplevel_exn_cont
      ~prior_cont_uses_env ~down_to_up =
  let cont_uses_env = DA.continuation_uses_env dacc_after_body in
  let code_age_relation_after_body =
    TE.code_age_relation (DA.typing_env dacc_after_body)
  in
  let consts_lifted_during_body = DA.get_lifted_constants dacc_after_body in
  let uses =
    CUE.compute_handler_env cont_uses_env cont ~params
      (* CR mshinwell: rename this parameter, the env does not
         have the constants in it now *)
      ~env_at_fork_plus_params_and_consts:denv_before_body
      ~consts_lifted_during_body
      ~code_age_relation_after_body
  in
  let dacc =
    (* CR mshinwell: Improve function names to clarify that this
       function (unlike the function of the same name in [DE])
       does not add to the environment, only to the accumulator. *)
    DA.add_lifted_constants dacc_after_body prior_lifted_constants
  in
  match uses with
  | No_uses ->
    (* Don't simplify the handler if there aren't any uses:
       otherwise, its code will be deleted but any continuation
       usage information collected during its simplification will
       remain. *)
    let cont_uses_env =
      CUE.union prior_cont_uses_env (CUE.remove cont_uses_env cont)
    in
    let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env in
    down_to_up dacc
      ~continuation_has_zero_uses:true
      ~rebuild:(fun uacc ~after_rebuild ->
        rebuild_non_recursive_let_cont_handler cont uses ~params
          ~handler ~free_names_of_handler:Name_occurrences.empty
          ~is_single_inlinable_use:false ~is_single_use:false scope
          EPA.empty cont_handler uacc ~after_rebuild)
  | Uses { handler_env; arg_types_by_use_id; extra_params_and_args;
           is_single_inlinable_use; is_single_use; } ->
    let handler_env, extra_params_and_args =
      (* Unbox the parameters of the continuation if possible.
         Any such unboxing will induce a rewrite (or wrapper) on
         the application sites of the continuation. *)
      match Continuation.sort cont with
      | Normal when is_single_inlinable_use ->
        assert (not is_exn_handler);
        handler_env, extra_params_and_args
      | Normal | Define_root_symbol ->
        assert (not is_exn_handler);
        let param_types =
          TE.find_params (DE.typing_env handler_env) params
        in
        Unbox_continuation_params.make_unboxing_decisions handler_env
          ~arg_types_by_use_id ~params ~param_types extra_params_and_args
      | Return | Toplevel_return ->
        assert (not is_exn_handler);
        handler_env, extra_params_and_args
      | Exn ->
        assert is_exn_handler;
        handler_env, extra_params_and_args
    in
    let at_unit_toplevel =
      (* We try to show that [handler] postdominates [body] (which is done by
         showing that [body] can only return through [cont]) and that if [body]
         raises any exceptions then it only does so to toplevel. If this can be
         shown and we are currently at the toplevel of a compilation unit, the
         handler for the environment can remain marked as toplevel (and suitable
         for "let symbol" bindings); otherwise, it cannot. *)
      DE.at_unit_toplevel denv_for_toplevel_check
        && (not (CH.is_exn_handler cont_handler))
        && Continuation.Set.subset
          (CUE.all_continuations_used cont_uses_env)
          (Continuation.Set.of_list [cont; unit_toplevel_exn_cont])
    in
    let dacc =
      let cont_uses_env =
        CUE.union prior_cont_uses_env (CUE.remove cont_uses_env cont)
      in
      let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env in
      let denv =
        (* Install the environment arising from the join into [dacc].  Note
           that this environment doesn't just contain the joined types; it may
           also contain definitions of code that were produced during
           simplification of the body.  (The [DE] component of [dacc_after_body]
           is discarded since we are now moving into a different scope.) *)
        DE.set_at_unit_toplevel_state handler_env at_unit_toplevel
      in
      let denv =
        if not at_unit_toplevel then denv
        else DE.mark_parameters_as_toplevel denv params
      in
      let denv =
        (* In the case where the continuation is going to be inlined, [denv] is
           basically the use environment, which might have a deeper inlining
           depth increment (e.g. where an [Apply] was inlined, revealing the
           linear inlinable use of the continuation).  We need to make sure the
           handler is simplified using the depth at the [Let_cont]. *)
        DE.set_inlining_state denv inlining_state_at_let_cont
      in
      (* Likewise, the inlined debuginfo may need restoring. *)
      DE.set_inlined_debuginfo denv inlined_debuginfo_at_let_cont
      |> DA.with_denv dacc
    in
    simplify_one_continuation_handler dacc cont ~at_unit_toplevel
      Non_recursive cont_handler ~params ~handler ~extra_params_and_args
      ~is_single_inlinable_use ~down_to_up:(fun dacc ~rebuild ->
        down_to_up dacc ~continuation_has_zero_uses:false
          ~rebuild:(fun uacc ~after_rebuild ->
            rebuild uacc ~after_rebuild:(fun cont_handler ~params
                  ~handler ~free_names_of_handler uacc ->
              rebuild_non_recursive_let_cont_handler cont uses ~params ~handler
                ~free_names_of_handler ~is_single_inlinable_use ~is_single_use
                scope extra_params_and_args cont_handler uacc ~after_rebuild)))

let simplify_non_recursive_let_cont dacc non_rec ~down_to_up =
  let cont_handler = Non_recursive_let_cont_handler.handler non_rec in
  Non_recursive_let_cont_handler.pattern_match non_rec ~f:(fun cont ~body ->
    let denv = DA.denv dacc in
    let denv_for_toplevel_check = denv in
    let unit_toplevel_exn_cont = DE.unit_toplevel_exn_continuation denv in
    let dacc, prior_lifted_constants =
      (* We clear the lifted constants accumulator so that we can easily
         obtain, below, any constants that are generated during the
         simplification of the [body].  We will add these
         [prior_lifted_constants] back into [dacc] later. *)
      DA.get_and_clear_lifted_constants dacc
    in
    let inlining_state_at_let_cont = DE.get_inlining_state (DA.denv dacc) in
    let inlined_debuginfo_at_let_cont =
      DE.get_inlined_debuginfo (DA.denv dacc)
    in
    let scope = DE.get_continuation_scope_level (DA.denv dacc) in
    let is_exn_handler = CH.is_exn_handler cont_handler in
    CH.pattern_match cont_handler ~f:(fun params ~handler ->
      let denv_before_body =
        (* We add the parameters assuming that none of them are at toplevel.
           When we do the toplevel calculation before simplifying the
           handler, we will mark any of the parameters that are in fact at
           toplevel as such. *)
        DE.add_parameters_with_unknown_types (DA.denv dacc) params
          ~at_unit_toplevel:false
      in
      let dacc_for_body =
        DE.increment_continuation_scope_level denv_before_body
        |> DA.with_denv dacc
      in
      let prior_cont_uses_env = DA.continuation_uses_env dacc_for_body in
      let dacc_for_body =
        DA.with_continuation_uses_env dacc_for_body ~cont_uses_env:CUE.empty
      in
      assert (DA.no_lifted_constants dacc_for_body);
      (* First the downwards traversal is done on the body. *)
      Simplify_expr.simplify_expr dacc_for_body body
        ~down_to_up:(fun dacc_after_body ~rebuild:rebuild_body ->
          (* Then, before the upwards traversal of the body, we do the
             downwards traversal of the handler. *)
          simplify_non_recursive_let_cont_handler ~denv_before_body
            ~dacc_after_body cont params ~handler cont_handler
            ~prior_lifted_constants ~inlining_state_at_let_cont
            ~inlined_debuginfo_at_let_cont ~scope ~is_exn_handler
            ~denv_for_toplevel_check ~unit_toplevel_exn_cont
            ~prior_cont_uses_env
            (* After doing the downwards traversal of the handler, we continue
               the downwards traversal of any surrounding expression (which
               would have to be a [Let_cont]; as such, there's no problem
               with returning the [DE] from the [handler] inside [dacc]
               since it will be replaced by the one from the surrounding
               context). *)
            ~down_to_up:(fun dacc ~continuation_has_zero_uses
                    ~rebuild:rebuild_handler ->
              down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
                let uenv_without_cont = UA.uenv uacc in
                (* Now, on the upwards traversal, the handler is rebuilt.
                   We need to be careful with the free name information
                   returned in [uacc] in two ways:
                   - Observe that linear inlining of the continuation doesn't
                     change the free names of the whole [Let_cont] (so nothing
                     extra to do here).
                   - If the continuation has zero uses, we must not
                     count the free names of the handler, as it will be
                     removed. *)
                let name_occurrences_subsequent_exprs =
                  UA.name_occurrences uacc
                in
                let uacc = UA.clear_name_occurrences uacc in
                rebuild_handler uacc ~after_rebuild:(fun handler uacc ->
                  let name_occurrences_handler =
                    if continuation_has_zero_uses then Name_occurrences.empty
                    else UA.name_occurrences uacc
                  in
                  let uacc = UA.clear_name_occurrences uacc in
                  (* Having rebuilt the handler, we now rebuild the body. *)
                  rebuild_body uacc ~after_rebuild:(fun body uacc ->
                    let name_occurrences_body = UA.name_occurrences uacc in
                    let num_free_occurrences_of_cont_in_body =
                      (* Note that this does not count uses in trap actions. *)
                      Name_occurrences.count_continuation
                        name_occurrences_body
                        cont
                    in
                    let is_applied_with_traps =
                      Name_occurrences.continuation_is_applied_with_traps
                        name_occurrences_body
                        cont
                    in
                    let remove_let_cont_leaving_body =
                      match num_free_occurrences_of_cont_in_body with
                      | Zero -> true
                      | One | More_than_one -> false
                    in
                    (* We are passing back over a binder, so remove the
                       bound continuation from the free name information.
                       Then compute the free names of the whole [Let_cont]. *)
                    let name_occurrences_body =
                      Name_occurrences.remove_continuation
                        name_occurrences_body cont
                    in
                    (* Having rebuilt both the body and handler, the [Let_cont]
                       expression itself is rebuilt -- unless either the
                       continuation had zero uses, in which case we're left
                       with the body; or if the body is just an [Apply_cont]
                       (with no trap action) of [cont], in which case we're
                       left with the handler.
                       The upwards environment of [uacc] is replaced so that
                       out-of-scope continuation bindings do not end up in the
                       accumulator. *)
                    let uacc = UA.with_uenv uacc uenv_without_cont in
                    let expr, uacc =
                      if remove_let_cont_leaving_body then
                        let uacc =
                          let name_occurrences =
                            Name_occurrences.union name_occurrences_body
                              name_occurrences_subsequent_exprs
                          in
                          UA.with_name_occurrences uacc ~name_occurrences
                        in
                        body, uacc
                      else
                        let remove_let_cont_leaving_handler =
                          match Expr.descr body with
                          | Apply_cont apply_cont ->
                            if not (Continuation.equal cont
                              (Apply_cont.continuation apply_cont))
                            then false
                            else
                              begin match Apply_cont.args apply_cont with
                              | [] ->
                                Option.is_none
                                  (Apply_cont.trap_action apply_cont)
                              | _::_ -> false
                              end
                          | Let _ | Apply _ | Switch _ | Invalid _
                          | Let_cont _ -> false
                        in
                        if remove_let_cont_leaving_handler then
                          let handler =
                            CH.pattern_match handler ~f:(fun _params ~handler ->
                              handler)
                          in
                          let uacc =
                            let name_occurrences =
                              Name_occurrences.union name_occurrences_handler
                                name_occurrences_subsequent_exprs
                            in
                            UA.with_name_occurrences uacc ~name_occurrences
                          in
                          handler, uacc
                        else
                          let uacc =
                            let name_occurrences =
                              Name_occurrences.union name_occurrences_body
                                (Name_occurrences.union name_occurrences_handler
                                  name_occurrences_subsequent_exprs)
                            in
                            UA.with_name_occurrences uacc ~name_occurrences
                          in
                          let expr =
                            Let_cont.create_non_recursive' ~cont handler ~body
                              ~num_free_occurrences_of_cont_in_body:
                                (Known num_free_occurrences_of_cont_in_body)
                              ~is_applied_with_traps
                          in
                          expr, uacc
                    in
                    after_rebuild expr uacc)))))))

let rebuild_recursive_let_cont_handlers cont arity ~original_cont_scope_level
      handler uacc ~after_rebuild =
  let uacc =
    UA.map_uenv uacc ~f:(fun uenv ->
      UE.add_continuation_with_handler uenv cont original_cont_scope_level
        arity handler)
  in
  let handlers = Continuation.Map.singleton cont handler in
  after_rebuild handlers uacc

(* This only takes one handler at present since we don't yet support
   simplification of multiple recursive handlers. *)
let simplify_recursive_let_cont_handlers ~denv_before_body ~dacc_after_body
      cont params ~handler cont_handler ~prior_lifted_constants arity
      ~original_cont_scope_level ~down_to_up =
  let denv, _arg_types =
    (* XXX These don't have the same scope level as the
        non-recursive case *)
    DE.add_parameters_with_unknown_types'
      ~at_unit_toplevel:false denv_before_body params
  in
  let code_age_relation_after_body =
    TE.code_age_relation (DA.typing_env dacc_after_body)
  in
  let denv =
    DA.get_lifted_constants dacc_after_body
    |> LCS.add_to_denv denv
  in
  let typing_env =
    TE.with_code_age_relation (DE.typing_env denv)
      code_age_relation_after_body
  in
  let denv = DE.with_typing_env denv typing_env in
  let dacc = DA.with_denv dacc_after_body denv in
  let dacc = DA.add_lifted_constants dacc prior_lifted_constants in
  let dacc = DA.map_denv dacc ~f:DE.set_not_at_unit_toplevel in
  simplify_one_continuation_handler dacc cont
    ~at_unit_toplevel:false Recursive
    cont_handler ~params ~handler
    ~extra_params_and_args:Continuation_extra_params_and_args.empty
    ~is_single_inlinable_use:false
    ~down_to_up:(fun dacc ~rebuild:rebuild_handler ->
      let cont_uses_env = CUE.remove (DA.continuation_uses_env dacc) cont in
      let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env in
      down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
        let uacc =
          UA.map_uenv uacc ~f:(fun uenv ->
            UE.add_continuation uenv cont original_cont_scope_level arity)
        in
        let name_occurrences_subsequent_exprs =
          UA.name_occurrences uacc
        in
        let uacc = UA.clear_name_occurrences uacc in
        rebuild_handler uacc ~after_rebuild:(fun cont_handler ~params
              ~handler:_ ~free_names_of_handler:_ uacc ->
          let uacc = UA.add_free_names uacc name_occurrences_subsequent_exprs in
          (* The parameters are removed from the free name information as they
             are no longer in scope. *)
          let uacc =
            let name_occurrences =
              ListLabels.fold_left params
                ~init:(UA.name_occurrences uacc)
                ~f:(fun name_occurrences param ->
                  KP.var param
                  |> Name_occurrences.remove_var name_occurrences)
            in
            UA.with_name_occurrences uacc ~name_occurrences
          in
          rebuild_recursive_let_cont_handlers cont arity
            ~original_cont_scope_level cont_handler uacc ~after_rebuild)))

let rebuild_recursive_let_cont ~body handlers ~uenv_without_cont uacc
      ~after_rebuild : Expr.t * UA.t =
  let uacc = UA.with_uenv uacc uenv_without_cont in
  let expr = Flambda.Let_cont.create_recursive handlers ~body in
  after_rebuild expr uacc

(* CR mshinwell: We should not simplify recursive continuations with no
   entry point -- could loop forever.  (Need to think about this again.) *)
let simplify_recursive_let_cont dacc recs ~down_to_up : Expr.t * UA.t =
  let module CH = Continuation_handler in
  Recursive_let_cont_handlers.pattern_match recs ~f:(fun ~body rec_handlers ->
    assert (not (Continuation_handlers.contains_exn_handler rec_handlers));
    let denv_before_body = DA.denv dacc in
    let original_cont_scope_level =
      DE.get_continuation_scope_level denv_before_body
    in
    let handlers = Continuation_handlers.to_map rec_handlers in
    let cont, cont_handler =
      match Continuation.Map.bindings handlers with
      | [] | _ :: _ :: _ ->
        Misc.fatal_error "Support for simplification of multiply-recursive \
          continuations is not yet implemented"
      | [c] -> c
    in
    CH.pattern_match cont_handler ~f:(fun params ~handler ->
      let arity = KP.List.arity_with_subkinds params in
      let dacc =
        DA.map_denv dacc ~f:DE.increment_continuation_scope_level
      in
      let dacc, prior_lifted_constants =
        (* We clear the lifted constants accumulator so that we can easily
           obtain, below, any constants that are generated during the
           simplification of the [body].  We will add these
           [prior_lifted_constants] back into [dacc] later. *)
        DA.get_and_clear_lifted_constants dacc
      in
      Simplify_expr.simplify_expr dacc body
        ~down_to_up:(fun dacc_after_body ~rebuild:rebuild_body ->
          simplify_recursive_let_cont_handlers ~denv_before_body
            ~dacc_after_body cont params ~handler cont_handler
            ~prior_lifted_constants arity ~original_cont_scope_level
            ~down_to_up:(fun dacc ~rebuild:rebuild_handlers ->
              down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
                let uenv_without_cont = UA.uenv uacc in
                rebuild_handlers uacc ~after_rebuild:(fun handlers uacc ->
                  rebuild_body uacc ~after_rebuild:(fun body uacc ->
                    (* We are passing back over a binder, so remove the
                       bound continuation from the free name information. *)
                    let uacc =
                      let name_occurrences =
                        Name_occurrences.remove_continuation
                          (UA.name_occurrences uacc) cont
                      in
                      UA.with_name_occurrences uacc ~name_occurrences
                    in
                    rebuild_recursive_let_cont ~body handlers
                      ~uenv_without_cont uacc ~after_rebuild)))))))

let simplify_let_cont dacc (let_cont : Let_cont.t) ~down_to_up : Expr.t * UA.t =
  match let_cont with
  | Non_recursive { handler; _ } ->
    simplify_non_recursive_let_cont dacc handler ~down_to_up
  | Recursive handlers ->
    simplify_recursive_let_cont dacc handlers ~down_to_up
