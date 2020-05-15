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

(* CR mshinwell: Need to simplify each [dbg] we come across. *)

(* CR-someday mshinwell: See whether resolution of continuation aliases can
   be made more transparent (e.g. through [find_continuation]). Tricky
   potentially in conjunction with the rewrites. *)

(* CR-someday mshinwell: Consider defunctionalising, which should be
   straightforward now this is in CPS form. *)

let rec simplify_expr dacc expr ~after_traversal =
  match Expr.descr expr with
  | Let let_expr ->
    simplify_let dacc let_expr ~after_traversal
  | Let_symbol let_symbol ->
    simplify_let_symbol dacc let_symbol ~after_traversal
  | Let_cont let_cont ->
    simplify_let_cont dacc let_cont ~after_traversal
  | Apply apply ->
    simplify_apply dacc apply ~after_traversal
  | Apply_cont apply_cont ->
    simplify_apply_cont dacc apply_cont ~after_traversal
  | Switch switch ->
    simplify_switch dacc switch ~after_traversal
  | Invalid _ ->
    (* CR mshinwell: Make sure that a program can be simplified to just
       [Invalid].  [Un_cps] should translate any [Invalid] that it sees as if
       it were [Halt_and_catch_fire].  Compilation units should also be
       able to be simplified to just [Invalid], but this must not cause
       linking failures. *)
    after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
      after_rebuild expr uacc)

and simplify_let dacc let_expr ~after_traversal =
  Let.pattern_match let_expr ~f:(fun ~bound_vars ~body ->
    Simplify_named.simplify_named dacc ~bound_vars (Let.defining_expr let_expr)
      ~after_traversal:(fun dacc ~rebuild:rebuild_defining_expr ->
        simplify_expr dacc body
          ~after_traversal:(fun dacc ~rebuild:rebuild_body ->
            after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
              rebuild_body uacc ~after_rebuild:(fun body uacc ->
                rebuild_defining_expr uacc
                  ~after_rebuild:(fun ~bindings_outermost_first:bindings uacc ->
                    let expr =
                      Simplify_common.bind_let_bound ~bindings ~body
                    in
                    after_rebuild expr uacc))))))

and simplify_let_symbol dacc let_symbol_expr ~after_traversal =
  let module LS = Let_symbol in
  if not (DE.at_unit_toplevel (DA.denv dacc)) then begin
    Misc.fatal_errorf "[Let_symbol] is only allowed at the toplevel of \
        compilation units (not even at the toplevel of function bodies):@ %a"
      LS.print let_symbol_expr
  end;
  let module Bound_symbols = LS.Bound_symbols in
  let scoping_rule = LS.scoping_rule let_symbol_expr in
  let bound_symbols = LS.bound_symbols let_symbol_expr in
  let defining_expr = LS.defining_expr let_symbol_expr in
  let body = LS.body let_symbol_expr in
  let prior_lifted_constants = R.get_lifted_constants (DA.r dacc) in
  let dacc = DA.map_r dacc ~f:R.clear_lifted_constants in
  let bound_symbols_free_names = Bound_symbols.free_names bound_symbols in
  let dacc =
    (* CR mshinwell: tidy this up? *)
    DA.map_denv dacc ~f:(fun denv ->
      Name_occurrences.fold_names bound_symbols_free_names
        ~init:denv
        ~f:(fun denv name ->
          Name.pattern_match name
            ~var:(fun _ -> denv)
            ~symbol:(fun symbol ->
              match bound_symbols with
              | Singleton _ -> DE.now_defining_symbol denv symbol
              | Sets_of_closures _ ->
                (* [Simplify_set_of_closures] will do [now_defining_symbol]. *)
                denv)))
  in
  Simplify_static_const.simplify_static_const dacc bound_symbols defining_expr
    ~after_traversal:(fun dacc ~rebuild:rebuild_defining_expr ->
      let dacc =
        DA.map_denv dacc ~f:(fun denv ->
          Name_occurrences.fold_names bound_symbols_free_names
            ~init:denv
            ~f:(fun denv name ->
              Name.pattern_match name
                ~var:(fun _ -> denv)
                ~symbol:(fun symbol ->
                  match bound_symbols with
                  | Singleton _ -> DE.no_longer_defining_symbol denv symbol
                  | Sets_of_closures _ -> denv)))
      in
      let dacc =
        match bound_symbols with
        | Singleton symbol ->
          DA.map_r dacc ~f:(fun r ->
            R.consider_constant_for_sharing r symbol defining_expr)
        | Sets_of_closures _ -> dacc
      in
      simplify_expr dacc body
        ~after_traversal:(fun dacc ~rebuild:rebuild_body ->
          after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
            rebuild_body uacc ~after_rebuild:(fun body uacc ->
              rebuild_defining_expr uacc
                ~after_rebuild:(rebuild_let_symbol ~prior_lifted_constants
                  ~scoping_rule ~body ~after_rebuild)))))

and rebuild_let_symbol ~prior_lifted_constants ~scoping_rule ~body
      ~after_rebuild bound_symbols defining_expr uacc =
  let lifted_constants = R.get_lifted_constants (UA.r uacc) in
  let uacc =
    UA.map_r uacc ~f:(fun r ->
      R.set_lifted_constants r prior_lifted_constants)
  in
  let all_lifted_constants =
    (bound_symbols, defining_expr, Name_occurrences.empty)
      :: List.map (fun lifted_constant ->
          LC.bound_symbols lifted_constant,
          LC.defining_expr lifted_constant,
            Name_occurrences.empty)
        lifted_constants
  in
  let sorted_lifted_constants =
    Sort_lifted_constants.sort None all_lifted_constants
  in
  let expr =
    List.fold_left (fun body (bound_symbols, defining_expr) ->
        Simplify_common.create_let_symbol (UA.r uacc) scoping_rule
          (UA.code_age_relation uacc) bound_symbols defining_expr body)
      body
      sorted_lifted_constants.bindings_outermost_last
  in
  after_rebuild expr uacc

and simplify_one_continuation_handler dacc cont handler
      (recursive : Recursive.t) (cont_handler : CH.t) ~params
      ~(extra_params_and_args : EPA.t) ~after_traversal =
  (* This simplifies the handler part of a Let_cont expression.  The handler
     may be non-recursive or recursive.  After simplifying the handler,
     unused parameters are removed. *)
  simplify_expr dacc handler
    ~after_traversal:(fun dacc ~rebuild:rebuild_handler ->
      after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
        rebuild_handler uacc ~after_rebuild:(fun handler uacc ->
          let free_names = Expr.free_names handler in
          let used_params =
            (* Removal of unused parameters of recursive continuations is not
               currently supported. *)
            match recursive with
            | Recursive -> params
            | Non_recursive ->
              let first = ref true in
              List.filter (fun param ->
                  (* CR mshinwell: We should have a robust means of propagating
                     which parameter is the exception bucket.  Then this hack
                     can be removed. *)
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
          let handler =
            CH.with_params_and_handler cont_handler
              (CPH.create (used_params @ used_extra_params) ~handler)
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
          after_rebuild handler uacc)))

and simplify_handler_of_non_recursive_let_cont ~dacc_after_body
      ~denv_before_body ~at_unit_toplevel cont ~scope ~is_exn_handler
      cont_handler ~params ~handler ~prior_lifted_constants ~after_traversal =
  (* We get here after having traversed the body (i.e. the scope) of a
     non-recursive continuation defined by a [Let_cont] expression (but before
     the expression forming such body has been rebuilt).  We do join point,
     unboxing and reification calculations before proceeding to simplify
     the handler of the continuation. *)
  let cont_uses_env = DA.continuation_uses_env dacc_after_body in
  let code_age_relation_after_body = DA.code_age_relation dacc_after_body in
  let lifted_during_body = R.get_lifted_constants (DA.r dacc_after_body) in
  let dacc =
    (* We need any lifted constant symbols and associated code IDs that were
       produced during simplification of the body to be defined in the fork
       environment passed to [compute_handler_env].
       The [DE] component of [dacc_after_body] is discarded since we have
       finished with the body now and will be moving into a different scope
       (that of the handler). *)
    (* CR mshinwell: We don't actually need to add equations on the symbols
       here, since the equations giving joined types will supercede them in
       [compute_handler_env]. However we can't just define the symbols; we also
       need the code IDs as well (which live in [DE]; the code age relation in
       [TE] is dealt with below).
       mshinwell: This is no longer the case if we have a single inlinable
       use. We copy the equations from [dacc] into the single use's
       environment (which might not contain them all already, somewhat
       counterintuitively) and return that as the handler env. *)
    DE.add_lifted_constants denv_before_body ~lifted:lifted_during_body
    |> DA.with_denv dacc_after_body
  in
  let uses =
    (* This is the join point calculation: from the types of the arguments
       of the continuation at its use sites, determine the types of the
       parameters at the top of the handler. *)
    CUE.compute_handler_env cont_uses_env cont ~params
      ~env_at_fork_plus_params_and_consts:(DA.typing_env dacc)
      (* XXX Check next line: used to be [consts_lifted_during_body]
         perhaps? *)
      ~consts_lifted_during_body:lifted_during_body
  in
  let dacc =
    DA.map_r dacc ~f:(fun r ->
      R.add_prior_lifted_constants r prior_lifted_constants)
  in
  match uses with
  | No_uses ->
    (* Don't simplify the handler if there aren't any uses: otherwise, its code
       will be deleted but any continuation usage information collected during
       its simplification will remain. *)
    after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
      after_rebuild cont_handler uacc)
  (* CR mshinwell: Refactor so we don't have the
     [is_single_use] hack.  The problem is that we want to
     have the code of the handler available if we might want to
     substitute it into a Switch---which we only want to do if
     we won't duplicate code (i.e. if there is only one use)
     ---but this is not a normal "inlinable" position and cannot
     be treated as such (e.g. for join calculations). *)
  | Uses { handler_typing_env; arg_types_by_use_id; extra_params_and_args;
           is_single_inlinable_use; is_single_use; } ->
    let typing_env, extra_params_and_args =
      (* Unbox the parameters of the continuation if possible.  Any such
         unboxing will induce a rewrite (or wrapper) on the application sites
         of the continuation. *)
      match Continuation.sort cont with
      | Normal when is_single_inlinable_use ->
        assert (not is_exn_handler);
        handler_typing_env, extra_params_and_args
      | Normal | Define_root_symbol ->
        assert (not is_exn_handler);
        let param_types = TE.find_params handler_typing_env params in
        Unbox_continuation_params.make_unboxing_decisions
          handler_typing_env ~arg_types_by_use_id ~params
          ~param_types extra_params_and_args
      | Return | Toplevel_return ->
        assert (not is_exn_handler);
        handler_typing_env, extra_params_and_args
      | Exn ->
        assert is_exn_handler;
        handler_typing_env, extra_params_and_args
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        (* Ensure that during simplification of the handler we will know about
           any changes to the code age relation that arose during simplification
           of the body.  Additionally, install the typing environment arising
           from the join into [dacc]. *)
        let denv =
          code_age_relation_after_body
          |> TE.with_code_age_relation typing_env
          |> DE.with_typing_env denv
        in
        if at_unit_toplevel then denv
        else DE.set_not_at_unit_toplevel denv)
    in
    let dacc, handler =
      (* When still at toplevel, attempt to reify the types of the
         continuation's parameters, to allow lifting of inconstants (one
         specific case being closures with computed environment entries). *)
      match Continuation.sort cont with
      | Normal when is_single_inlinable_use || not at_unit_toplevel ->
        dacc, handler
      | Return | Toplevel_return | Exn -> dacc, handler
      | Normal | Define_root_symbol ->
        (* CR mshinwell: This shouldn't be [assert] in the [Define_root_symbol]
           case *)
        assert at_unit_toplevel;
        Reify_continuation_param_types.
          lift_via_reification_of_continuation_param_types dacc
            ~params ~extra_params_and_args ~handler
    in
    simplify_one_continuation_handler dacc cont handler Recursive.Non_recursive
      cont_handler ~params ~extra_params_and_args
      ~after_traversal:(fun dacc ~rebuild:rebuild_handler ->
        after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
          rebuild_handler uacc
            ~after_rebuild:(register_non_recursive_let_cont_handler cont
              scope ~is_exn_handler ~is_single_inlinable_use ~is_single_use
              ~after_rebuild)))

and register_non_recursive_let_cont_handler cont scope ~is_exn_handler
      ~is_single_inlinable_use ~is_single_use ~after_rebuild handler uacc =
  let uenv = UA.uenv uacc in
  let uenv =
    let can_inline =
      (* CR mshinwell: This check must line up with
         Continuation_uses.  Remove the duplication. *)
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

and simplify_non_recursive_let_cont_handler dacc handler ~after_traversal =
  let cont_handler = Non_recursive_let_cont_handler.handler handler in
  Non_recursive_let_cont_handler.pattern_match handler ~f:(fun cont ~body ->
    let denv = DA.denv dacc in
    let unit_toplevel_exn_cont = DE.unit_toplevel_exn_continuation denv in
    let at_unit_toplevel =
      (* We try to show that [handler] postdominates [body] (which is done by
         showing that [body] can only return through [cont]) and that if
         [body] raises any exceptions then it only does so to toplevel.
         If this can be shown and we are currently at the toplevel of a
         compilation unit, the handler for the environment can remain marked
         as toplevel (and suitable for [Let_symbol] bindings); otherwise, it
         cannot. *)
      DE.at_unit_toplevel denv
        && (not (Continuation_handler.is_exn_handler cont_handler))
        && Continuation.Set.subset
              (Name_occurrences.continuations (Expr.free_names body))
              (Continuation.Set.of_list [cont; unit_toplevel_exn_cont])
    in
    let r, prior_lifted_constants =
      (* We clear the lifted constants accumulator so that we can easily
         obtain, below, any constants that are generated during the
         simplification of the [body]. We will add these
         [prior_lifted_constants] back into [r] later. *)
      R.get_and_clear_lifted_constants (DA.r dacc)
    in
    let dacc = DA.with_r dacc r in
    let scope = DE.get_continuation_scope_level (DA.denv dacc) in
    let params_and_handler = CH.params_and_handler cont_handler in
    let is_exn_handler = CH.is_exn_handler cont_handler in
    CPH.pattern_match params_and_handler ~f:(fun params ~handler ->
      let denv_before_body = DE.define_parameters (DA.denv dacc) ~params in
      let dacc_for_body =
        DE.increment_continuation_scope_level denv_before_body
        |> DA.with_denv dacc
      in
      simplify_expr dacc_for_body body
        ~after_traversal:(fun dacc_after_body ~rebuild:rebuild_body ->
          simplify_handler_of_non_recursive_let_cont ~dacc_after_body
            ~denv_before_body ~at_unit_toplevel cont ~scope ~is_exn_handler
            cont_handler ~params ~handler ~prior_lifted_constants
            ~after_traversal:(fun dacc ~rebuild:rebuild_handler ->
              after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
                let uenv_to_return = UA.uenv uacc in
                rebuild_handler uacc ~after_rebuild:(fun handler uacc ->
                  rebuild_body uacc ~after_rebuild:(fun body uacc ->
                    (* The upwards environment of [uacc] is replaced so
                       that out-of-scope continuation bindings do not end up
                       in the accumulator. *)
                    let expr =
                      Let_cont.create_non_recursive cont handler ~body
                    in
                    let uacc = UA.with_uenv uacc uenv_to_return in
                    after_rebuild expr uacc)))))))

and simplify_handler_of_recursive_let_cont ~dacc_after_body ~definition_denv
      cont ~original_cont_scope_level ~arity ~params ~handler cont_handler
      ~prior_lifted_constants ~after_traversal =
  let cont_uses_env = DA.continuation_uses_env dacc_after_body in
  let r = DA.r dacc_after_body in
  let arg_types =
    (* We can't know a good type from the call types *)
    List.map T.unknown arity
  in
  (* CR mshinwell: This next part is dubious, use the rewritten
     version in the recursive-continuation-unboxing branch. *)
  let (cont_uses_env, _apply_cont_rewrite_id) :
    Continuation_uses_env.t * Apply_cont_rewrite_id.t =
    (* We don't know anything, it's like it was called
       with an arbitrary argument! *)
    CUE.record_continuation_use cont_uses_env cont
      Non_inlinable (* Maybe simpler ? *)
      ~typing_env_at_use:(
        (* not useful as we will have only top *)
        DE.typing_env definition_denv
      )
      ~arg_types
  in
  let denv =
    (* CR mshinwell: These don't have the same scope level as the
       non-recursive case *)
    DE.add_parameters definition_denv params ~param_types:arg_types
  in
  let code_age_relation_after_body =
    TE.code_age_relation (DA.typing_env dacc_after_body)
  in
  let denv =
    DE.add_lifted_constants denv ~lifted:(R.get_lifted_constants r)
  in
  let typing_env =
    TE.with_code_age_relation (DE.typing_env denv) code_age_relation_after_body
  in
  let denv = DE.with_typing_env denv typing_env in
  let r = R.add_prior_lifted_constants r prior_lifted_constants in
  let dacc = DA.create denv cont_uses_env r in
  let dacc = DA.map_denv dacc ~f:DE.set_not_at_unit_toplevel in
  simplify_one_continuation_handler dacc cont handler Recursive.Recursive
    cont_handler ~params ~extra_params_and_args:EPA.empty
    ~after_traversal:(fun dacc ~rebuild:rebuild_handler ->
      after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
        rebuild_handler uacc
          ~after_rebuild:(register_recursive_let_cont_handler
            cont ~original_cont_scope_level ~arity ~after_rebuild)))

and register_recursive_let_cont_handler cont ~original_cont_scope_level ~arity
      ~after_rebuild handler uacc =
  let uacc =
    UA.map_uenv uacc ~f:(fun uenv ->
      UE.add_continuation_with_handler uenv cont
        original_cont_scope_level
        arity handler)
  in
  let handlers = Continuation.Map.singleton cont handler in
  after_rebuild handlers uacc

(* CR mshinwell: We should not simplify recursive continuations with no
   entry point -- could loop forever.  (Need to think about this again.) *)
and simplify_recursive_let_cont_handlers dacc handlers ~after_traversal =
  let module CH = Continuation_handler in
  let module CPH = Continuation_params_and_handler in
  Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body handlers ->
    assert (not (Continuation_handlers.contains_exn_handler handlers));
    let definition_denv = DA.denv dacc in
    let original_cont_scope_level =
      DE.get_continuation_scope_level definition_denv
    in
    let handlers = Continuation_handlers.to_map handlers in
    let cont, cont_handler =
      match Continuation.Map.bindings handlers with
      | [c] -> c
      | [] | _ :: _ :: _ ->
        Misc.fatal_errorf "Support for simplification of multiply-recursive \
            continuations is not yet implemented"
    in
    let params_and_handler = CH.params_and_handler cont_handler in
    CPH.pattern_match params_and_handler ~f:(fun params ~handler ->
      let arity = KP.List.arity params in
      let dacc = DA.map_denv dacc ~f:DE.increment_continuation_scope_level in
      let r, prior_lifted_constants =
        (* We clear the lifted constants accumulator so that we can easily
           obtain, below, any constants that are generated during the
           simplification of the [body].  We will add these
           [prior_lifted_constants] back into [r] later. *)
        R.get_and_clear_lifted_constants (DA.r dacc)
      in
      let dacc = DA.with_r dacc r in
      simplify_expr dacc body
        ~after_traversal:(fun dacc_after_body ~rebuild:rebuild_body ->
          simplify_handler_of_recursive_let_cont ~dacc_after_body
            ~definition_denv cont ~original_cont_scope_level
            ~arity ~params ~handler cont_handler ~prior_lifted_constants
            ~after_traversal:(fun dacc ~rebuild:rebuild_handlers ->
              after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
                let uenv_to_return = UA.uenv uacc in
                rebuild_handlers uacc ~after_rebuild:(fun handlers uacc ->
                  rebuild_body uacc ~after_rebuild:(fun body uacc ->
                    let expr = Let_cont.create_recursive handlers ~body in
                    let uacc = UA.with_uenv uacc uenv_to_return in
                    after_rebuild expr uacc)))))))

and simplify_let_cont dacc (let_cont : Let_cont.t) ~after_traversal =
  match let_cont with
  | Non_recursive { handler; _ } ->
    simplify_non_recursive_let_cont_handler dacc handler ~after_traversal
  | Recursive handlers ->
    simplify_recursive_let_cont_handlers dacc handlers ~after_traversal

and simplify_direct_full_application dacc apply function_decl_opt ~result_arity
      ~after_traversal =
  let callee = Apply.callee apply in
  let args = Apply.args apply in
  let inlined =
    match function_decl_opt with
    | None -> None
    | Some (function_decl, function_decl_rec_info) ->
      let apply_inlining_depth = Apply.inlining_depth apply in
      let decision =
        Inlining_decision.make_decision_for_call_site (DA.denv dacc)
          ~function_decl_rec_info
          ~apply_inlining_depth
          (Apply.inline apply)
      in
      match Inlining_decision.Call_site_decision.can_inline decision with
      | Do_not_inline -> None
      | Inline { unroll_to; } ->
        let dacc, inlined =
          Inlining_transforms.inline dacc ~callee
            ~args function_decl
            ~apply_return_continuation:(Apply.continuation apply)
            ~apply_exn_continuation:(Apply.exn_continuation apply)
            ~apply_inlining_depth ~unroll_to
            (Apply.dbg apply)
        in
        Some (dacc, inlined)
  in
  match inlined with
  | Some (dacc, inlined) -> simplify_expr dacc inlined ~after_traversal
  | None ->
    let dacc, use_id =
      DA.record_continuation_use dacc (Apply.continuation apply) Non_inlinable
        ~typing_env_at_use:(DA.typing_env dacc)
        ~arg_types:(T.unknown_types_from_arity result_arity)
    in
    let dacc, exn_cont_use_id =
      DA.record_continuation_use dacc
        (Exn_continuation.exn_handler (Apply.exn_continuation apply))
        Non_inlinable
        ~typing_env_at_use:(DA.typing_env dacc)
        ~arg_types:(T.unknown_types_from_arity (
          Exn_continuation.arity (Apply.exn_continuation apply)))
    in
    after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
      let expr =
        apply
        |> Simplify_common.update_exn_continuation_extra_args uacc
             ~exn_cont_use_id
        |> Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
             result_arity
      in
      after_rebuild expr uacc)

and simplify_direct_partial_application dacc apply ~callee's_code_id
      ~callee's_closure_id ~param_arity ~result_arity ~recursive
      ~after_traversal =
  (* For simplicity, we disallow [@inline] attributes on partial
     applications.  The user may always write an explicit wrapper instead
     with such an attribute. *)
  (* CR-someday mshinwell: Pierre noted that we might like a function to be
     inlined when applied to its first set of arguments, e.g. for some kind
     of type class like thing. *)
  let args = Apply.args apply in
  let dbg = Apply.dbg apply in
  begin match Apply.inline apply with
  | Always_inline | Never_inline ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@inlined] attributes may not be used \
        on partial applications")
  | Unroll _ ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@unroll] attributes may not be used \
        on partial applications")
  | Default_inline -> ()
  end;
  let arity = List.length param_arity in
  assert (arity > List.length args);
  let applied_args, remaining_param_arity =
    Misc.Stdlib.List.map2_prefix (fun arg kind ->
        if not (K.equal kind K.value) then begin
          Misc.fatal_errorf "Non-[value] kind in partial application: %a"
            Apply.print apply
        end;
        arg)
      args param_arity
  in
  begin match result_arity with
  | [kind] when K.equal kind K.value -> ()
  | _ ->
    Misc.fatal_errorf "Partially-applied function with non-[value] \
        return kind: %a"
      Apply.print apply
  end;
  let wrapper_var = Variable.create "partial_app" in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let wrapper_closure_id =
    Closure_id.wrap compilation_unit (Variable.create "partial_app_closure")
  in
  let wrapper_taking_remaining_args, dacc =
    let return_continuation = Continuation.create () in
    let remaining_params =
      List.map (fun kind ->
          let param = Variable.create "param" in
          Kinded_parameter.create param kind)
        remaining_param_arity
    in
    let args = applied_args @ (List.map KP.simple remaining_params) in
    let call_kind =
      Call_kind.direct_function_call callee's_code_id callee's_closure_id
        ~return_arity:result_arity
    in
    let applied_args_with_closure_vars = (* CR mshinwell: rename *)
      List.map (fun applied_arg ->
          Var_within_closure.wrap compilation_unit (Variable.create "arg"),
            applied_arg)
        ((Apply.callee apply) :: applied_args)
    in
    let my_closure = Variable.create "my_closure" in
    let exn_continuation =
      Apply.exn_continuation apply
      |> Exn_continuation.without_extra_args
    in
    let body =
      let full_application =
        Apply.create ~callee:(Apply.callee apply)
          ~continuation:return_continuation
          exn_continuation
          ~args
          ~call_kind
          dbg
          ~inline:Default_inline
          ~inlining_depth:(Apply.inlining_depth apply)
      in
      List.fold_left (fun expr (closure_var, arg) ->
          match Simple.must_be_var arg with
          | None -> expr
          | Some arg ->
            let arg = VB.create arg Name_mode.normal in
            Expr.create_let arg
              (Named.create_prim
                (Unary (Project_var {
                  project_from = wrapper_closure_id;
                  var = closure_var;
                }, Simple.var my_closure))
                dbg)
              expr)
        (Expr.create_apply full_application)
        (List.rev applied_args_with_closure_vars)
    in
    let params_and_body =
      Function_params_and_body.create ~return_continuation
        exn_continuation
        remaining_params
        ~body
        ~dbg
        ~my_closure
    in
    let code_id =
      Code_id.create
        ~name:(Closure_id.to_string callee's_closure_id ^ "_partial")
        (Compilation_unit.get_current_exn ())
    in
    let function_decl =
      Function_declaration.create ~code_id
        ~params_arity:(KP.List.arity remaining_params)
        ~result_arity
        ~stub:true
        ~dbg
        ~inline:Default_inline
        ~is_a_functor:false
        ~recursive
    in
    let function_decls =
      Function_declarations.create
        (Closure_id.Map.singleton wrapper_closure_id function_decl)
    in
    let closure_elements =
      Var_within_closure.Map.of_list applied_args_with_closure_vars
    in
    (* CR mshinwell: Factor out this next part into a helper function *)
    let code =
      Lifted_constant.create_piece_of_code (DA.denv dacc) code_id
        params_and_body
    in
    let dacc =
      dacc
      |> DA.map_r ~f:(fun r -> R.new_lifted_constant r code)
      |> DA.map_denv ~f:(fun denv ->
        DE.add_lifted_constants denv ~lifted:[code])
    in
    Set_of_closures.create function_decls ~closure_elements, dacc
  in
  let apply_cont =
    Apply_cont.create (Apply.continuation apply)
      ~args:[Simple.var wrapper_var] ~dbg
  in
  let expr =
    let wrapper_var = VB.create wrapper_var Name_mode.normal in
    let closure_vars =
      Closure_id.Map.singleton wrapper_closure_id wrapper_var
    in
    let pattern = Bindable_let_bound.set_of_closures ~closure_vars in
    Expr.create_pattern_let pattern
      (Named.create_set_of_closures wrapper_taking_remaining_args)
      (Expr.create_apply_cont apply_cont)
  in
  simplify_expr dacc expr ~after_traversal

(* CR mshinwell: Should it be an error to encounter a non-direct application
   of a symbol after [Simplify]? This shouldn't usually happen, but I'm not 100%
   sure it cannot in every case. *)

and simplify_direct_over_application dacc apply ~param_arity ~after_traversal =
  let arity = List.length param_arity in
  let args = Apply.args apply in
  assert (arity < List.length args);
  let full_app_args, remaining_args = Misc.Stdlib.List.split_at arity args in
  let func_var = Variable.create "full_apply" in
  let perform_over_application =
    Apply.create ~callee:(Simple.var func_var)
      ~continuation:(Apply.continuation apply)
      (Apply.exn_continuation apply)
      ~args:remaining_args
      ~call_kind:(Call_kind.indirect_function_call_unknown_arity ())
      (Apply.dbg apply)
      ~inline:(Apply.inline apply)
      ~inlining_depth:(Apply.inlining_depth apply)
  in
  let after_full_application = Continuation.create () in
  let after_full_application_handler =
    let params_and_handler =
      let func_param = KP.create func_var K.value in
      Continuation_params_and_handler.create [func_param]
        ~handler:(Expr.create_apply perform_over_application)
    in
    Continuation_handler.create ~params_and_handler
      ~stub:false
      ~is_exn_handler:false
  in
  let full_apply =
    Apply.with_continuation_callee_and_args apply
      after_full_application
      ~callee:(Apply.callee apply)
      ~args:full_app_args
  in
  let expr =
    Let_cont.create_non_recursive after_full_application
      after_full_application_handler
      ~body:(Expr.create_apply full_apply)
  in
  simplify_expr dacc expr ~after_traversal

and simplify_direct_function_call dacc apply ~callee's_code_id_from_type
      ~callee's_code_id_from_call_kind ~callee's_closure_id ~param_arity
      ~result_arity ~recursive function_decl_opt ~after_traversal =
  let result_arity_of_application =
    Call_kind.return_arity (Apply.call_kind apply)
  in
  if not (Flambda_arity.equal result_arity_of_application result_arity)
  then begin
    Misc.fatal_errorf "Wrong return arity for direct OCaml function call \
        (expected %a, found %a):@ %a"
      Flambda_arity.print result_arity
      Flambda_arity.print result_arity_of_application
      Apply.print apply
  end;
  let callee's_code_id : _ Or_bottom.t =
    match callee's_code_id_from_call_kind with
    | None -> Ok callee's_code_id_from_type
    | Some callee's_code_id_from_call_kind ->
      let code_age_rel = TE.code_age_relation (DA.typing_env dacc) in
      Code_age_relation.meet code_age_rel callee's_code_id_from_call_kind
        callee's_code_id_from_type
  in
  match callee's_code_id with
  | Bottom -> simplifies_to_invalid dacc ~after_traversal
  | Ok callee's_code_id ->
    let call_kind =
      Call_kind.direct_function_call callee's_code_id callee's_closure_id
        ~return_arity:result_arity
    in
    let apply = Apply.with_call_kind apply call_kind in
    let args = Apply.args apply in
    let provided_num_args = List.length args in
    let num_params = List.length param_arity in
    if provided_num_args = num_params then
      simplify_direct_full_application dacc apply function_decl_opt
        ~result_arity ~after_traversal
    else if provided_num_args > num_params then
      simplify_direct_over_application dacc apply ~param_arity ~after_traversal
    else if provided_num_args > 0 && provided_num_args < num_params then
      simplify_direct_partial_application dacc apply ~callee's_code_id
        ~callee's_closure_id ~param_arity ~result_arity ~recursive
        ~after_traversal
    else
      Misc.fatal_errorf "Function with %d params when simplifying \
          direct OCaml function call with %d arguments: %a"
        num_params
        provided_num_args
        Apply.print apply

and simplify_function_call_where_callee's_type_unavailable dacc apply
      (call : Call_kind.Function_call.t) ~arg_types ~after_traversal =
  let cont = Apply.continuation apply in
  let denv = DA.denv dacc in
  let typing_env_at_use = DE.typing_env denv in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~typing_env_at_use:(DA.typing_env dacc)
      ~arg_types:(T.unknown_types_from_arity (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  let check_return_arity_and_record_return_cont_use ~return_arity =
    DA.record_continuation_use dacc cont Non_inlinable ~typing_env_at_use
      ~arg_types:(T.unknown_types_from_arity return_arity)
  in
  let call_kind, use_id, dacc =
    match call with
    | Indirect_unknown_arity ->
      let dacc, use_id =
        DA.record_continuation_use dacc (Apply.continuation apply) Non_inlinable
          ~typing_env_at_use ~arg_types:[T.any_value ()]
      in
      Call_kind.indirect_function_call_unknown_arity (), use_id, dacc
    | Indirect_known_arity { param_arity; return_arity; } ->
      let args_arity = T.arity_of_list arg_types in
      if not (Flambda_arity.equal param_arity args_arity) then begin
        Misc.fatal_errorf "Argument arity on indirect-known-arity \
            application doesn't match [Call_kind] (expected %a, \
            found %a):@ %a"
          Flambda_arity.print param_arity
          Flambda_arity.print args_arity
          Apply.print apply
      end;
      let dacc, use_id =
        check_return_arity_and_record_return_cont_use ~return_arity
      in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity
          ~return_arity
      in
      call_kind, use_id, dacc
    | Direct { return_arity; _ } ->
      let param_arity = T.arity_of_list arg_types in
      (* Some types have regressed in precision.  Since this used to be a
         direct call, however, we know the function's arity even though we
         don't know which function it is. *)
      let dacc, use_id =
        check_return_arity_and_record_return_cont_use ~return_arity
      in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity
          ~return_arity
      in
      call_kind, use_id, dacc
  in
  after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
    let apply =
      Apply.with_call_kind apply call_kind
      |> Simplify_common.update_exn_continuation_extra_args uacc
           ~exn_cont_use_id
    in
    let expr =
      Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
        (Call_kind.return_arity call_kind) apply
    in
    after_rebuild expr uacc)

(* CR mshinwell: I've seen at least one case where a call of kind
   [Indirect_unknown_arity] has been generated with no warning, despite having
   [@inlined always]. *)
and simplify_function_call dacc apply ~callee_ty
      (call : Call_kind.Function_call.t) ~arg_types ~after_traversal =
  let type_unavailable () =
    simplify_function_call_where_callee's_type_unavailable dacc apply call
      ~arg_types ~after_traversal
  in
  (* CR mshinwell: Should this be using [meet_shape], like for primitives? *)
  let denv = DA.denv dacc in
  match T.prove_single_closures_entry (DE.typing_env denv) callee_ty with
  | Proved (callee's_closure_id, _closures_entry, func_decl_type) ->
    (* CR mshinwell: We should check that the [set_of_closures] in the
       [closures_entry] structure in the type does indeed contain the
       closure in question. *)
    begin match func_decl_type with
    | Ok (Inlinable inlinable) ->
      let module I = T.Function_declaration_type.Inlinable in
      let callee's_code_id_from_call_kind =
        match call with
        | Direct { code_id; closure_id; _ } ->
          if not (Closure_id.equal closure_id callee's_closure_id) then begin
            Misc.fatal_errorf "Closure ID %a in application doesn't match \
                closure ID %a discovered via typing.@ Application:@ %a"
              Closure_id.print closure_id
              Closure_id.print callee's_closure_id
              Apply.print apply
          end;
          Some code_id
        | Indirect_unknown_arity
        | Indirect_known_arity _ -> None
      in
      (* CR mshinwell: This should go in Typing_env (ditto logic for Rec_info
         in Simplify_simple *)
      let function_decl_rec_info =
        let rec_info = I.rec_info inlinable in
        match Simple.rec_info (Apply.callee apply) with
        | None -> rec_info
        | Some newer -> Rec_info.merge rec_info ~newer
      in
      let callee's_code_id_from_type = I.code_id inlinable in
      simplify_direct_function_call dacc apply ~callee's_code_id_from_type
        ~callee's_code_id_from_call_kind ~callee's_closure_id
        ~param_arity:(I.param_arity inlinable)
        ~result_arity:(I.result_arity inlinable)
        ~recursive:(I.recursive inlinable)
        (Some (inlinable, function_decl_rec_info))
        ~after_traversal
    | Ok (Non_inlinable non_inlinable) ->
      let module N = T.Function_declaration_type.Non_inlinable in
      let callee's_code_id_from_type = N.code_id non_inlinable in
      let callee's_code_id_from_call_kind =
        match call with
        | Direct { code_id; _ } -> Some code_id
        | Indirect_unknown_arity
        | Indirect_known_arity _ -> None
      in
      simplify_direct_function_call dacc apply
        ~callee's_code_id_from_type
        ~callee's_code_id_from_call_kind
        ~callee's_closure_id
        ~param_arity:(N.param_arity non_inlinable)
        ~result_arity:(N.result_arity non_inlinable)
        ~recursive:(N.recursive non_inlinable)
        None ~after_traversal
    | Bottom -> simplifies_to_invalid dacc ~after_traversal
    | Unknown -> type_unavailable ()
    end
  | Unknown -> type_unavailable ()
  | Invalid -> simplifies_to_invalid dacc ~after_traversal

and simplify_apply_shared dacc apply : _ Or_bottom.t =
  let min_name_mode = Name_mode.normal in
  match S.simplify_simple dacc (Apply.callee apply) ~min_name_mode with
  | Bottom, _ty -> Bottom
  | Ok callee, callee_ty ->
    match S.simplify_simples dacc (Apply.args apply) ~min_name_mode with
    | _, Bottom -> Bottom
    | _changed, Ok args_with_types ->
      let args, arg_types = List.split args_with_types in
      let inlining_depth =
        DE.get_inlining_depth_increment (DA.denv dacc)
          + Apply.inlining_depth apply
      in
      let apply =
        Apply.create ~callee
          ~continuation:(Apply.continuation apply)
          (Apply.exn_continuation apply)
          ~args
          ~call_kind:(Apply.call_kind apply)
          (DE.add_inlined_debuginfo' (DA.denv dacc) (Apply.dbg apply))
          ~inline:(Apply.inline apply)
          ~inlining_depth
      in
      Ok (callee_ty, apply, arg_types)

and simplify_method_call dacc apply ~callee_ty ~obj ~arg_types
      ~after_traversal =
  let callee_kind = T.kind callee_ty in
  if not (K.is_value callee_kind) then begin
    Misc.fatal_errorf "Method call with callee of wrong kind %a: %a"
      K.print callee_kind
      T.print callee_ty
  end;
  let denv = DA.denv dacc in
  DE.check_simple_is_bound denv obj;
  let expected_arity = List.map (fun _ -> K.value) arg_types in
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal expected_arity args_arity) then begin
    Misc.fatal_errorf "All arguments to a method call must be of kind \
        [value]:@ %a"
      Apply.print apply
  end;
  let dacc, use_id =
    DA.record_continuation_use dacc (Apply.continuation apply) Non_inlinable
      ~typing_env_at_use:(DE.typing_env denv)
      ~arg_types:[T.any_value ()]
  in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~typing_env_at_use:(DA.typing_env dacc)
      ~arg_types:(T.unknown_types_from_arity (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
    let expr =
      apply
      |> Simplify_common.update_exn_continuation_extra_args uacc
           ~exn_cont_use_id
      |> Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
           (Flambda_arity.create [K.value])
    in
    after_rebuild expr uacc)

and simplify_c_call dacc apply ~callee_ty ~param_arity ~return_arity ~arg_types
      ~after_traversal =
  let callee_kind = T.kind callee_ty in
  if not (K.is_value callee_kind) then begin
    Misc.fatal_errorf "C callees must be of kind [value], not %a: %a"
      K.print callee_kind
      T.print callee_ty
  end;
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal args_arity param_arity) then begin
    Misc.fatal_errorf "Arity %a of [Apply] arguments doesn't match \
        parameter arity %a of C callee:@ %a"
      Flambda_arity.print args_arity
      Flambda_arity.print param_arity
      Apply.print apply
  end;
  let dacc, use_id =
    DA.record_continuation_use dacc (Apply.continuation apply) Non_inlinable
      ~typing_env_at_use:(DA.typing_env dacc)
      ~arg_types:(T.unknown_types_from_arity return_arity)
  in
  let dacc, exn_cont_use_id =
    (* CR mshinwell: Try to factor out these stanzas, here and above. *)
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~typing_env_at_use:(DA.typing_env dacc)
      ~arg_types:(T.unknown_types_from_arity (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
    let expr =
      apply
      |> Simplify_common.update_exn_continuation_extra_args uacc
           ~exn_cont_use_id
      |> Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
           return_arity
    in
    after_rebuild expr uacc)

and simplify_apply dacc apply ~after_traversal =
  match simplify_apply_shared dacc apply with
  | Bottom -> simplifies_to_invalid dacc ~after_traversal
  | Ok (callee_ty, apply, arg_types) ->
    match Apply.call_kind apply with
    | Function call ->
      simplify_function_call dacc apply ~callee_ty call ~arg_types
        ~after_traversal
    | Method { kind = _; obj; } ->
      simplify_method_call dacc apply ~callee_ty ~obj ~arg_types
        ~after_traversal
    | C_call { alloc = _; param_arity; return_arity; } ->
      simplify_c_call dacc apply ~callee_ty ~param_arity ~return_arity
        ~arg_types ~after_traversal

and simplify_apply_cont dacc apply_cont ~after_traversal =
  let module AC = Apply_cont in
  let min_name_mode = Name_mode.normal in
  match S.simplify_simples dacc (AC.args apply_cont) ~min_name_mode with
  | _, Bottom -> simplifies_to_invalid dacc ~after_traversal
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
      DA.record_continuation_use dacc
        (AC.continuation apply_cont)
        use_kind
        ~typing_env_at_use:(DA.typing_env dacc)
        ~arg_types
    in
    after_traversal dacc
      ~rebuild:(rebuild_apply_cont apply_cont ~args rewrite_id)

and rebuild_apply_cont apply_cont ~args rewrite_id uacc ~after_rebuild =
  let module AC = Apply_cont in
  let uenv = UA.uenv uacc in
  let cont = AC.continuation apply_cont in
  let rewrite = UE.find_apply_cont_rewrite uenv cont in
  let cont = UE.resolve_continuation_aliases uenv cont in
  let rewrite_use_result =
    let apply_cont =
      AC.update_continuation_and_args apply_cont cont ~args
    in
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
  let normal_case () =
    match rewrite_use_result with
    | Apply_cont apply_cont ->
      after_rebuild (Expr.create_apply_cont apply_cont) uacc
    | Expr expr -> after_rebuild expr uacc
  in
  match UE.find_continuation uenv cont with
  | Unknown { arity = _; handler = _; } -> normal_case ()
  | Unreachable { arity = _; } ->
    (* N.B. We allow this transformation even if there is a trap action,
       on the basis that there wouldn't be any opportunity to collect any
       backtrace, even if the [Apply_cont] were compiled as "raise". *)
    after_rebuild (Expr.create_invalid ()) uacc
  | Inline { arity = _; handler; } ->
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
      (* CR mshinwell: maybe instead of [Inline] it should say "linearly
         used" or "stub" -- could avoid resimplification of linearly used
         ones maybe, although this wouldn't remove any parameter-to-argument
         [Let]s. However perhaps [Flambda_to_cmm] could deal with these. *)
      match AC.trap_action apply_cont with
      | Some _ ->
        (* Until such time as we can manually add to the backtrace buffer,
           never substitute a "raise" for the body of an exception
           handler. *)
        normal_case ()
      | None ->
        Flambda.Continuation_params_and_handler.pattern_match
          (Flambda.Continuation_handler.params_and_handler handler)
          ~f:(fun params ~handler ->
            (* CR mshinwell: Why does [New_let_binding] have a
               [Variable]? *)
            (* CR mshinwell: Should verify that names in the
               [Apply_cont_rewrite] are in scope. *)
            (* We can't easily call [simplify_expr] on the inlined body
               since [dacc] isn't the correct accumulator and environment
               any more. However there's no need to simplify the inlined
               body except to make use of parameter-to-argument bindings; we
               just leave them for a subsequent round of [Simplify] or
               [Un_cps] to clean up. *)
            let args = Apply_cont.args apply_cont in
            let params_and_args =
              assert (List.compare_lengths params args = 0);
              List.map (fun (param, arg) ->
                  param, Named.create_simple arg)
                (List.combine params args)
            in
            let expr =
              Expr.bind_parameters ~bindings:params_and_args ~body:handler
            in
            after_rebuild expr uacc)

and simplify_arms_of_switch dacc switch ~scrutinee_ty =
  let module AC = Apply_cont in
  let typing_env_at_use = DA.typing_env dacc in
  Target_imm.Map.fold (fun arm action (arms, dacc) ->
      let shape =
        let imm = Target_imm.int (Target_imm.to_targetint arm) in
        T.this_naked_immediate imm
      in
      match T.meet typing_env_at_use scrutinee_ty shape with
      | Bottom -> arms, dacc
      | Ok (_meet_ty, env_extension) ->
        let typing_env_at_use =
          TE.add_env_extension typing_env_at_use env_extension
        in
        let args = AC.args action in
        match args with
        | [] ->
          let dacc, rewrite_id =
            DA.record_continuation_use dacc (AC.continuation action)
              Non_inlinable ~typing_env_at_use ~arg_types:[]
          in
          let arms = Target_imm.Map.add arm (action, rewrite_id, []) arms in
          arms, dacc
        | _::_ ->
          let min_name_mode = Name_mode.normal in
          match S.simplify_simples dacc args ~min_name_mode with
          | _, Bottom -> arms, dacc
          | _changed, Ok args_with_types ->
            let args, arg_types = List.split args_with_types in
            let dacc, rewrite_id =
              DA.record_continuation_use dacc (AC.continuation action)
                Non_inlinable ~typing_env_at_use ~arg_types
            in
            let arity = List.map T.kind arg_types in
            let action = Apply_cont.update_args action ~args in
            let arms =
              Target_imm.Map.add arm (action, rewrite_id, arity) arms
            in
            arms, dacc)
    (Switch.arms switch)
    (Target_imm.Map.empty, dacc)

and absorb_apply_conts_and_classify_arms uacc arm (action, use_id, arity)
      (new_let_conts, arms, identity_arms, not_arms) =
  match Simplify_common.add_wrapper_for_switch_arm uacc action ~use_id arity
  with
  | Apply_cont action ->
    let action =
      (* First try to absorb any [Apply_cont] expression that forms
         the entirety of the arm's action (via an intermediate
         zero-arity continuation without trap action) into the
         [Switch] expression itself. *)
      if not (Apply_cont.is_goto action) then Some action
      else
        let cont = Apply_cont.continuation action in
        match UE.find_continuation (UA.uenv uacc) cont with
        | Inline { arity = _; handler; }
        | Unknown { arity = _; handler = Some handler; } ->
          Continuation_params_and_handler.pattern_match
            (Continuation_handler.params_and_handler handler)
            ~f:(fun params ~handler ->
              assert (List.length params = 0);
              match Expr.descr handler with
              | Apply_cont action -> Some action
              | Let _ | Let_symbol _ | Let_cont _ | Apply _
              | Switch _ | Invalid _ -> Some action)
        | Unknown _ -> Some action
        | Unreachable _ -> None
    in
    begin match action with
    | None ->
      (* The destination is unreachable; delete the [Switch] arm. *)
      new_let_conts, arms, identity_arms, not_arms
    | Some action ->
      let normal_case ~identity_arms ~not_arms =
        let arms = Target_imm.Map.add arm action arms in
        new_let_conts, arms, identity_arms, not_arms
      in
      (* Now check to see if the arm is of a form that might mean the
          whole [Switch] is either the identity or a boolean NOT. *)
      match Apply_cont.to_one_arg_without_trap_action action with
      | None -> normal_case ~identity_arms ~not_arms
      | Some arg ->
        (* CR-someday mshinwell: Maybe this check should be
            generalised e.g. to detect
            | 0 -> apply_cont k x y 1
            | 1 -> apply_cont k x y 0
        *)
        let [@inline always] const arg =
          match Reg_width_const.descr arg with
          | Tagged_immediate arg ->
            if Target_imm.equal arm arg then
              let identity_arms =
                Target_imm.Map.add arm action identity_arms
              in
              normal_case ~identity_arms ~not_arms
            else if
              (Target_imm.equal arm Target_imm.bool_true
                && Target_imm.equal arg Target_imm.bool_false)
              ||
                (Target_imm.equal arm Target_imm.bool_false
                  && Target_imm.equal arg Target_imm.bool_true)
            then
              let not_arms = Target_imm.Map.add arm action not_arms in
              normal_case ~identity_arms ~not_arms
            else
              normal_case ~identity_arms ~not_arms
          | Naked_immediate _ | Naked_float _ | Naked_int32 _
          | Naked_int64 _ | Naked_nativeint _ ->
            normal_case ~identity_arms ~not_arms
        in
        Simple.pattern_match arg ~const
          ~name:(fun _ -> normal_case ~identity_arms ~not_arms)
    end
  | New_wrapper (new_cont, new_handler) ->
    let new_let_cont = new_cont, new_handler in
    let new_let_conts = new_let_cont :: new_let_conts in
    let action = Apply_cont.goto new_cont in
    let arms = Target_imm.Map.add arm action arms in
    new_let_conts, arms, identity_arms, not_arms

and simplify_switch dacc switch ~after_traversal =
  let min_name_mode = Name_mode.normal in
  let scrutinee = Switch.scrutinee switch in
  match S.simplify_simple dacc scrutinee ~min_name_mode with
  | Bottom, _ -> simplifies_to_invalid dacc ~after_traversal
  | Ok scrutinee, scrutinee_ty ->
    let arms, dacc = simplify_arms_of_switch dacc switch ~scrutinee_ty in
    after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
      let new_let_conts, arms, identity_arms, not_arms =
        Target_imm.Map.fold (absorb_apply_conts_and_classify_arms uacc)
          arms
          ([], Target_imm.Map.empty, Target_imm.Map.empty, Target_imm.Map.empty)
      in
      let switch_is_identity =
        let arm_discrs = Target_imm.Map.keys arms in
        let identity_arms_discrs = Target_imm.Map.keys identity_arms in
        if not (Target_imm.Set.equal arm_discrs identity_arms_discrs) then
          None
        else
          Target_imm.Map.data identity_arms
          |> List.map Apply_cont.continuation
          |> Continuation.Set.of_list
          |> Continuation.Set.get_singleton
      in
      let switch_is_boolean_not =
        let arm_discrs = Target_imm.Map.keys arms in
        let not_arms_discrs = Target_imm.Map.keys not_arms in
        if (not (Target_imm.Set.equal arm_discrs Target_imm.all_bools))
          || (not (Target_imm.Set.equal arm_discrs not_arms_discrs))
        then
          None
        else
          Target_imm.Map.data not_arms
          |> List.map Apply_cont.continuation
          |> Continuation.Set.of_list
          |> Continuation.Set.get_singleton
      in
      rebuild_switch dacc uacc ~scrutinee ~scrutinee_ty arms ~switch_is_identity
        ~switch_is_boolean_not ~new_let_conts ~after_rebuild)

and rebuild_switch dacc uacc ~scrutinee ~scrutinee_ty arms ~switch_is_identity
      ~switch_is_boolean_not ~new_let_conts ~after_rebuild =
  let create_tagged_scrutinee k =
    let bound_to = Variable.create "tagged_scrutinee" in
    let body = k ~tagged_scrutinee:(Simple.var bound_to) in
    let bound_vars =
      Bindable_let_bound.singleton (VB.create bound_to NM.normal)
    in
    let named =
      Named.create_prim (Unary (Box_number Untagged_immediate, scrutinee))
        Debuginfo.none
    in
    Simplify_named.simplify_named dacc ~bound_vars named
      ~after_traversal:(fun _dacc ~rebuild:rebuild_named ->
        rebuild_named uacc
          ~after_rebuild:(fun ~bindings_outermost_first:bindings uacc ->
            Simplify_common.bind_let_bound ~bindings:bindings ~body, uacc))
  in
  let body, uacc =
    match switch_is_identity with
    | Some dest ->
      create_tagged_scrutinee (fun ~tagged_scrutinee ->
        let apply_cont =
          Apply_cont.create dest ~args:[tagged_scrutinee]
            ~dbg:Debuginfo.none
        in
        Expr.create_apply_cont apply_cont)
    | None ->
      match switch_is_boolean_not with
      | Some dest ->
        create_tagged_scrutinee (fun ~tagged_scrutinee ->
          let not_scrutinee = Variable.create "not_scrutinee" in
          let apply_cont =
            Apply_cont.create dest ~args:[Simple.var not_scrutinee]
              ~dbg:Debuginfo.none
          in
          Expr.create_let (VB.create not_scrutinee NM.normal)
            (Named.create_prim (P.Unary (Boolean_not, tagged_scrutinee))
              Debuginfo.none)
            (Expr.create_apply_cont apply_cont))
      | None ->
        let expr = Expr.create_switch ~scrutinee ~arms in
        if !Clflags.flambda_invariant_checks
          && Simple.is_const scrutinee
          && Target_imm.Map.cardinal arms > 1
        then begin
          Misc.fatal_errorf "[Switch] with constant scrutinee (type: %a) \
              should have been simplified away:@ %a"
            T.print scrutinee_ty
            Expr.print expr
        end;
        expr, uacc
  in
  let expr =
    List.fold_left (fun body (new_cont, new_handler) ->
        Let_cont.create_non_recursive new_cont new_handler ~body)
      body
      new_let_conts
  in
  after_rebuild expr uacc

and simplifies_to_invalid dacc ~after_traversal =
  after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
    after_rebuild (Expr.create_invalid ()) uacc)
