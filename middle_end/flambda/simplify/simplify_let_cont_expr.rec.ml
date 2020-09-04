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
      ~(extra_params_and_args : EPA.t) handler uacc ~after_rebuild =
  let handler, uacc =
    let params = params @ extra_params_and_args.extra_params in
    (* We might need to place lifted constants now, as they could
       depend on continuation parameters.  As such we must also compute
       the unused parameters after placing any constants! *)
    if (not at_unit_toplevel)
      || List.compare_length_with params 0 = 0
    then handler, uacc
    else
      Simplify_common.place_lifted_constants uacc
        Dominator
        ~lifted_constants_from_defining_expr:LCS.empty
        ~lifted_constants_from_body:(UA.lifted_constants uacc)
        ~put_bindings_around_body:(fun ~body -> body)
        ~body:handler
        ~critical_deps_of_bindings:(KP.List.free_names params)
  in
  let free_names = Expr.free_names handler in
  let used_params =
    (* Removal of unused parameters of recursive continuations is not
       currently supported. *)
    match recursive with
    | Recursive -> params
    | Non_recursive ->
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
    List.filter (fun extra_param ->
        Name_occurrences.mem_var free_names (KP.var extra_param))
      extra_params_and_args.extra_params
  in
  let params' = used_params @ used_extra_params in
  let handler =
    CH.with_params_and_handler cont_handler (CPH.create params' ~handler)
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
  after_rebuild handler uacc

let simplify_one_continuation_handler dacc cont ~at_unit_toplevel recursive
      cont_handler ~params ~handler ~extra_params_and_args ~down_to_up =
  Simplify_expr.simplify_expr dacc handler
    ~down_to_up:(fun dacc ~rebuild ->
      down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
        rebuild uacc ~after_rebuild:(fun handler uacc ->
          rebuild_one_continuation_handler cont ~at_unit_toplevel recursive
            cont_handler ~params ~extra_params_and_args handler uacc
            ~after_rebuild)))

let rebuild_non_recursive_let_cont_handler cont
      (uses : Continuation_env_and_param_types.t) ~is_single_inlinable_use
      ~is_single_use scope ~is_exn_handler handler uacc ~after_rebuild =
  let uenv = UA.uenv uacc in
  let uenv =
    match uses with
    | No_uses -> uenv
    | Uses _ ->
      let can_inline =
        (* CR mshinwell: This check must line up with Continuation_uses.
           Remove the duplication. *)
        if is_single_inlinable_use && (not is_exn_handler) then Some handler
        else None
      in
      match can_inline with
      | Some handler ->
        (* CR mshinwell: tidy up *)
        let arity =
          match CH.behaviour handler with
          | Unreachable { arity; }
          | Alias_for { arity; _ }
          | Unknown { arity; } -> arity
        in
        UE.add_continuation_to_inline uenv cont scope arity handler
      | None ->
        match CH.behaviour handler with
        | Unreachable { arity; } ->
          UE.add_unreachable_continuation uenv cont scope arity
        | Alias_for { arity; alias_for; } ->
          UE.add_continuation_alias uenv cont arity ~alias_for
        | Unknown { arity; } ->
          if is_single_use then
            UE.add_continuation_with_handler uenv cont scope arity handler
          else
            UE.add_continuation uenv cont scope arity
  in
  after_rebuild handler (UA.with_uenv uacc uenv)

let simplify_non_recursive_let_cont_handler ~denv_before_body ~dacc_after_body
      cont params ~(handler : Expr.t) cont_handler ~prior_lifted_constants
      ~inlining_depth_increment_at_let_cont ~inlined_debuginfo_at_let_cont
      ~scope ~at_unit_toplevel ~is_exn_handler ~down_to_up =
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
    down_to_up dacc
      ~rebuild:(rebuild_non_recursive_let_cont_handler cont uses
        ~is_single_inlinable_use:false ~is_single_use:false scope
        ~is_exn_handler cont_handler)
  (* CR mshinwell: Refactor so we don't have the
     [is_single_use] hack.  The problem is that we want to
     have the code of the handler available if we might want to
     substitute it into a Switch---which we only want to do if
     we won't duplicate code (i.e. if there is only one use)
     ---but this is not a normal "inlinable" position and cannot
     be treated as such (e.g. for join calculations). *)
  | Uses { handler_env; arg_types_by_use_id; extra_params_and_args;
           is_single_inlinable_use; is_single_use; } ->
    let handler_typing_env, extra_params_and_args =
      let handler_typing_env = DE.typing_env handler_env in
      (* Unbox the parameters of the continuation if possible.
         Any such unboxing will induce a rewrite (or wrapper) on
         the application sites of the continuation. *)
      match Continuation.sort cont with
      | Normal when is_single_inlinable_use ->
        assert (not is_exn_handler);
        handler_typing_env, extra_params_and_args
      | Normal | Define_root_symbol ->
        assert (not is_exn_handler);
        let param_types = TE.find_params handler_typing_env params in
        Unbox_continuation_params.make_unboxing_decisions handler_typing_env
          ~arg_types_by_use_id ~params ~param_types extra_params_and_args
      | Return | Toplevel_return ->
        assert (not is_exn_handler);
        handler_typing_env, extra_params_and_args
      | Exn ->
        assert is_exn_handler;
        handler_typing_env, extra_params_and_args
    in
    let handler_env = DE.with_typing_env handler_env handler_typing_env in
    let dacc =
      let denv =
        (* Install the environment arising from the join into [dacc].  Note
           that this environment doesn't just contain the joined types; it may
           also contain definitions of code that were produced during
           simplification of the body.  (The [DE] component of [dacc_after_body]
           is discarded since we are now moving into a different scope.) *)
        DE.set_at_unit_toplevel_state handler_env at_unit_toplevel
      in
      let denv =
        (* In the case where the continuation is going to be inlined, [denv] is
           basically the use environment, which might have a deeper inlining
           depth increment (e.g. where an [Apply] was inlined, revealing the
           linear inlinable use of the continuation).  We need to make sure the
           handler is simplified using the depth at the [Let_cont]. *)
        DE.set_inlining_depth_increment denv
          inlining_depth_increment_at_let_cont
      in
      (* Likewise, the inlined debuginfo may need restoring. *)
      DE.set_inlined_debuginfo denv inlined_debuginfo_at_let_cont
      |> DA.with_denv dacc
    in
    simplify_one_continuation_handler dacc cont ~at_unit_toplevel
      Non_recursive cont_handler ~params ~handler ~extra_params_and_args
      ~down_to_up:(fun dacc ~rebuild ->
        down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
          rebuild uacc ~after_rebuild:(fun cont_handler uacc ->
            rebuild_non_recursive_let_cont_handler cont uses
              ~is_single_inlinable_use ~is_single_use scope ~is_exn_handler
              cont_handler uacc ~after_rebuild)))

let rebuild_non_recursive_let_cont cont ~body handler ~uenv_without_cont uacc
      ~after_rebuild =
  (* The upwards environment of [uacc] is replaced so that out-of-scope
     continuation bindings do not end up in the accumulator. *)
  let uacc = UA.with_uenv uacc uenv_without_cont in
  let expr = Let_cont.create_non_recursive cont handler ~body in
  after_rebuild expr uacc

let simplify_non_recursive_let_cont dacc non_rec ~down_to_up =
  let cont_handler = Non_recursive_let_cont_handler.handler non_rec in
  Non_recursive_let_cont_handler.pattern_match non_rec ~f:(fun cont ~body ->
    let denv = DA.denv dacc in
    let unit_toplevel_exn_cont = DE.unit_toplevel_exn_continuation denv in
    let at_unit_toplevel =
      (* We try to show that [handler] postdominates [body] (which is done by
         showing that [body] can only return through [cont]) and that if
         [body] raises any exceptions then it only does so to toplevel.
         If this can be shown and we are currently at the toplevel of a
         compilation unit, the handler for the environment can remain marked
         as toplevel (and suitable for "let symbol" bindings); otherwise, it
         cannot. *)
      DE.at_unit_toplevel denv
        && (not (Continuation_handler.is_exn_handler cont_handler))
        && Continuation.Set.subset
              (Name_occurrences.continuations (Expr.free_names body))
              (Continuation.Set.of_list [cont; unit_toplevel_exn_cont])
    in
    let dacc, prior_lifted_constants =
      (* We clear the lifted constants accumulator so that we can easily
         obtain, below, any constants that are generated during the
         simplification of the [body].  We will add these
         [prior_lifted_constants] back into [dacc] later. *)
      DA.get_and_clear_lifted_constants dacc
    in
    let inlining_depth_increment_at_let_cont =
      DE.get_inlining_depth_increment (DA.denv dacc)
    in
    let inlined_debuginfo_at_let_cont =
      DE.get_inlined_debuginfo (DA.denv dacc)
    in
    let scope = DE.get_continuation_scope_level (DA.denv dacc) in
    let params_and_handler = CH.params_and_handler cont_handler in
    let is_exn_handler = CH.is_exn_handler cont_handler in
    CPH.pattern_match params_and_handler ~f:(fun params ~handler ->
      let denv_before_body =
        DE.add_parameters_with_unknown_types ~at_unit_toplevel
          (DA.denv dacc) params
      in
      let dacc_for_body =
        DE.increment_continuation_scope_level denv_before_body
        |> DA.with_denv dacc
      in
      assert (DA.no_lifted_constants dacc_for_body);
      (* First the downwards traversal is done on the body. *)
      Simplify_expr.simplify_expr dacc_for_body body
        ~down_to_up:(fun dacc_after_body ~rebuild:rebuild_body ->
          (* Then, before the upwards traversal of the body, we do the
             downwards traversal of the handler. *)
          simplify_non_recursive_let_cont_handler ~denv_before_body
            ~dacc_after_body cont params ~handler cont_handler
            ~prior_lifted_constants ~inlining_depth_increment_at_let_cont
            ~inlined_debuginfo_at_let_cont ~scope ~at_unit_toplevel
            ~is_exn_handler
            (* After doing the downwards traversal of the handler, we continue
               the downwards traversal of any surrounding expression (which
               would have to be a [Let_cont]; as such, there's no problem
               with returning the [DE] from the [handler] inside [dacc]
               since it will be replaced by the one from the surrounding
               context). *)
            ~down_to_up:(fun dacc ~rebuild:rebuild_handler ->
              down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
                let uenv_without_cont = UA.uenv uacc in
                (* Now, on the upwards traversal, the handler is rebuilt... *)
                rebuild_handler uacc ~after_rebuild:(fun handler uacc ->
                  (* ...and then the body. *)
                  rebuild_body uacc ~after_rebuild:(fun body uacc ->
                    (* Having rebuilt both the body and handler, the [Let_cont]
                       expression itself is rebuilt. *)
                    rebuild_non_recursive_let_cont cont ~body handler
                      ~uenv_without_cont uacc ~after_rebuild)))))))

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
  let cont_uses_env = DA.continuation_uses_env dacc_after_body in
  let denv, arg_types =
    (* XXX These don't have the same scope level as the
        non-recursive case *)
    DE.add_parameters_with_unknown_types'
      ~at_unit_toplevel:false denv_before_body params
  in
  (* CR mshinwell: This next part is dubious, use the rewritten
      version in the recursive-continuation-unboxing branch. *)
  let (cont_uses_env, _apply_cont_rewrite_id) :
    Continuation_uses_env.t * Apply_cont_rewrite_id.t =
    (* We don't know anything, it's like it was called
        with an arbitrary argument! *)
    CUE.record_continuation_use cont_uses_env cont
      Non_inlinable (* Maybe simpler ? *)
      ~env_at_use:(
        (* not useful as we will have only top *)
        denv_before_body
      )
      ~arg_types
  in
  let code_age_relation_after_body =
    TE.code_age_relation (DA.typing_env dacc_after_body)
  in
  let denv =
    DA.get_lifted_constants dacc_after_body
    |> DE.add_lifted_constants denv
  in
  let typing_env =
    TE.with_code_age_relation (DE.typing_env denv)
      code_age_relation_after_body
  in
  let denv = DE.with_typing_env denv typing_env in
  let dacc =
    DA.with_denv dacc_after_body denv
    |> DA.with_continuation_uses_env ~cont_uses_env
  in
  let dacc = DA.add_lifted_constants dacc prior_lifted_constants in
  let dacc = DA.map_denv dacc ~f:DE.set_not_at_unit_toplevel in
  simplify_one_continuation_handler dacc cont
    ~at_unit_toplevel:false Recursive
    cont_handler ~params ~handler
    ~extra_params_and_args:
      Continuation_extra_params_and_args.empty
    ~down_to_up:(fun dacc ~rebuild:rebuild_handler ->
      down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
        let uacc =
          UA.map_uenv uacc ~f:(fun uenv ->
            UE.add_continuation uenv cont original_cont_scope_level arity)
        in
        rebuild_handler uacc ~after_rebuild:(fun handler uacc ->
          rebuild_recursive_let_cont_handlers cont arity
            ~original_cont_scope_level handler uacc ~after_rebuild)))

let rebuild_recursive_let_cont ~body handlers ~uenv_without_cont uacc
      ~after_rebuild : Expr.t * UA.t =
  let uacc = UA.with_uenv uacc uenv_without_cont in
  let expr = Flambda.Let_cont.create_recursive handlers ~body in
  after_rebuild expr uacc

(* CR mshinwell: We should not simplify recursive continuations with no
   entry point -- could loop forever.  (Need to think about this again.) *)
let simplify_recursive_let_cont dacc recs ~down_to_up : Expr.t * UA.t =
  let module CH = Continuation_handler in
  let module CPH = Continuation_params_and_handler in
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
    let params_and_handler = CH.params_and_handler cont_handler in
    CPH.pattern_match params_and_handler ~f:(fun params ~handler ->
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
                    rebuild_recursive_let_cont ~body handlers
                      ~uenv_without_cont uacc ~after_rebuild)))))))

let simplify_let_cont dacc (let_cont : Let_cont.t) ~down_to_up : Expr.t * UA.t =
  match let_cont with
  | Non_recursive { handler; _ } ->
    simplify_non_recursive_let_cont dacc handler ~down_to_up
  | Recursive handlers ->
    simplify_recursive_let_cont dacc handlers ~down_to_up
