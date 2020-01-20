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
(* CR mshinwell: Consider defunctionalising to remove the [k]. *)
(* CR mshinwell: May in any case be able to remove the polymorphic recursion. *)
(* CR mshinwell: See whether resolution of continuation aliases can be made
   more transparent (e.g. through [find_continuation]).  Tricky potentially in
   conjunction with the rewrites. *)

type 'a k = DA.t -> ('a * UA.t)

let rec simplify_let
  : 'a. DA.t -> Let.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc let_expr k ->
  let module L = Flambda.Let in
  (* CR mshinwell: Find out if we need the special fold function for lets. *)
  L.pattern_match let_expr ~f:(fun ~bound_vars ~body ->
    let { Simplify_named. bindings_outermost_first = bindings; dacc; } =
      Simplify_named.simplify_named dacc ~bound_vars (L.defining_expr let_expr)
    in
    let body, user_data, uacc = simplify_expr dacc body k in
    Simplify_common.bind_let_bound ~bindings ~body, user_data, uacc)

and simplify_let_symbol
  : 'a. DA.t -> Let_symbol.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc let_symbol_expr k ->
  let module LS = Let_symbol in
  if not (DE.at_unit_toplevel (DA.denv dacc)) then begin
    Misc.fatal_errorf "[Let_symbol] is only allowed at the toplevel of \
        compilation units (not even at the toplevel of function bodies):@ %a"
      LS.print let_symbol_expr
  end;
  let module Bound_symbols = LS.Bound_symbols in
  let bound_symbols = LS.bound_symbols let_symbol_expr in
  (* CR mshinwell: We can't do this in conjunction with the current
     reification scheme for continuation parameters; that has to put the
     symbols in the environment.
  Symbol.Set.iter (fun sym ->
      if DE.mem_symbol (DA.denv dacc) sym then begin
        Misc.fatal_errorf "Symbol %a is already defined:@ %a"
          Symbol.print sym
          LS.print let_symbol_expr
      end)
    (LS.Bound_symbols.being_defined bound_symbols);
  Code_id.Set.iter (fun code_id ->
      if DE.mem_code (DA.denv dacc) code_id then begin
        Misc.fatal_errorf "Code ID %a is already defined:@ %a"
          Code_id.print code_id
          LS.print let_symbol_expr
      end)
    (Bound_symbols.code_being_defined bound_symbols); *)
  let defining_expr = LS.defining_expr let_symbol_expr in
  let body = LS.body let_symbol_expr in
  let prior_lifted_constants = R.get_lifted_constants (DA.r dacc) in
  let dacc = DA.map_r dacc ~f:R.clear_lifted_constants in
  let defining =
    Name_occurrences.symbols (Bound_symbols.free_names bound_symbols)
  in
  let dacc =
    (* CR mshinwell: tidy this up? *)
    DA.map_denv dacc ~f:(fun denv ->
      Symbol.Set.fold (fun symbol denv ->
          match bound_symbols with
          | Singleton _ -> DE.now_defining_symbol denv symbol
          | Sets_of_closures _ ->
            (* [Simplify_set_of_closures] will do [now_defining_symbol]. *)
            denv)
        defining
        denv)
  in
  let bound_symbols, defining_expr, dacc =
    Simplify_static_const.simplify_static_const dacc bound_symbols defining_expr
  in
  let dacc =
    Variable.Set.fold (fun var dacc ->
        DA.Usage.record_use_of_variable dacc var)
      (Name_occurrences.variables (Static_const.free_names defining_expr))
      dacc
  in
  let bound_symbols_free_names = Bound_symbols.free_names bound_symbols in
  Symbol.Set.iter (fun sym ->
      DE.check_symbol_is_bound (DA.denv dacc) sym)
    (Name_occurrences.symbols bound_symbols_free_names);
  Code_id.Set.iter (fun code_id ->
      DE.check_code_id_is_bound (DA.denv dacc) code_id)
    (Name_occurrences.code_ids bound_symbols_free_names);
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      Symbol.Set.fold (fun symbol denv ->
          match bound_symbols with
          | Singleton _ -> DE.no_longer_defining_symbol denv symbol
          | Sets_of_closures _ -> denv)
        defining
        denv)
  in
  let dacc =
    match bound_symbols with
    | Singleton symbol ->
      DA.map_r dacc ~f:(fun r ->
        R.consider_constant_for_sharing r symbol defining_expr)
    | Sets_of_closures _ -> dacc
  in
  let body, user_data, uacc = simplify_expr dacc body k in
  let lifted_constants = R.get_lifted_constants (UA.r uacc) in
  let uacc =
    UA.map_r uacc ~f:(fun r -> R.set_lifted_constants r prior_lifted_constants)
  in
  let all_lifted_constants =
    (bound_symbols, defining_expr)
      :: List.map (fun lifted_constant ->
          LC.bound_symbols lifted_constant, LC.defining_expr lifted_constant)
        lifted_constants
  in
(*
Format.eprintf "All bindings:@ %a\n%!"
  (Format.pp_print_list ~pp_sep:Format.pp_print_space
    (fun ppf (bound_syms, def) ->
      Format.fprintf ppf "@[(%a@ %a)@]"
        Bound_symbols.print bound_syms Static_const.print def))
  all_lifted_constants;
*)
  let sorted_lifted_constants =
    (* CR mshinwell: [Sort_lifted_constants] should never need dacc here.
       We should maybe change the interface to make it optional and cause
       an error if it tries to use it. *)
    Sort_lifted_constants.sort dacc all_lifted_constants
  in
  let expr =
    List.fold_left (fun body (bound_symbols, defining_expr) ->
        Simplify_common.create_let_symbol (UA.code_age_relation uacc)
          bound_symbols defining_expr body)
      body
      sorted_lifted_constants.bindings_outermost_last
  in
  expr, user_data, uacc

and simplify_one_continuation_handler :
 'a. DA.t
  -> Continuation.t
  -> Recursive.t
  -> CH.t
  -> params:KP.t list
  -> handler:Expr.t
  -> extra_params_and_args:Continuation_extra_params_and_args.t
  -> 'a k 
  -> Continuation_handler.t * 'a * UA.t
= fun dacc cont (_recursive : Recursive.t) (cont_handler : CH.t) ~params
      ~(handler : Expr.t) ~(extra_params_and_args : EPA.t) k ->
  let handler, (user_data, params), uacc =
    simplify_expr dacc handler (fun dacc ->
      (* It is here that the need for [DA.Usage] crystallises: we need the
         used-variables information before reconstructing the term (in the
         recursive cases), so the normal [free_names] infrastructure doesn't
         suffice. *)
      let used_variables = DA.Usage.used_variables dacc in
      let used_params =
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
              Variable.Set.mem (KP.var param) used_variables
            end)
          params
      in
      let used_extra_params =
        List.filter (fun extra_param ->
            Variable.Set.mem (KP.var extra_param) used_variables)
          extra_params_and_args.extra_params
      in
      let rewrite =
        Apply_cont_rewrite.create ~original_params:params
          ~used_params:(KP.Set.of_list used_params)
          ~extra_params:extra_params_and_args.extra_params
          ~extra_args:extra_params_and_args.extra_args
          ~extra_args_recursive_uses:
            extra_params_and_args.extra_args_recursive_uses
          ~used_extra_params:(KP.Set.of_list used_extra_params)
      in
      let user_data, uacc = k dacc in
      let uacc =
        UA.map_uenv uacc ~f:(fun uenv ->
          UE.add_apply_cont_rewrite uenv cont rewrite)
      in
      let params = used_params @ used_extra_params in
      (user_data, params), uacc)
  in
  let handler =
    CH.with_params_and_handler cont_handler (CPH.create params ~handler)
  in
  handler, user_data, uacc

and simplify_non_recursive_let_cont_handler
  : 'a. DA.t -> Non_recursive_let_cont_handler.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc non_rec_handler k ->
  let cont_handler = Non_recursive_let_cont_handler.handler non_rec_handler in
  Non_recursive_let_cont_handler.pattern_match non_rec_handler
    ~f:(fun cont ~body ->
      let free_names_of_body = Expr.free_names body in
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
               (Name_occurrences.continuations free_names_of_body)
               (Continuation.Set.of_list [cont; unit_toplevel_exn_cont])
      in
      let body, handler, user_data, uacc =
        let body, (result, uenv', user_data), uacc =
          let scope = DE.get_continuation_scope_level (DA.denv dacc) in
          let params_and_handler = CH.params_and_handler cont_handler in
          let is_exn_handler = CH.is_exn_handler cont_handler in
          CPH.pattern_match params_and_handler ~f:(fun params ~handler ->
            let denv = DE.define_parameters (DA.denv dacc) ~params in
            let dacc =
              DA.with_denv dacc (DE.increment_continuation_scope_level denv)
            in
            simplify_expr dacc body (fun dacc_after_body ->
              (* CR mshinwell: Could assert that the code age relation is
                 a superset. *)
      (*
              Format.eprintf "Parameters for %a: %a\n%!"
                Continuation.print cont
                KP.List.print params;
      *)
              let denv =
                (* CR mshinwell: Don't need to re-add constants that were
                   above the [Let_cont], they will already be in denv. *)
                let r = DA.r dacc_after_body in
                DE.add_lifted_constants denv ~lifted:(R.get_lifted_constants r)
              in
              let uses =
                let param_types =
                  List.map (fun param -> T.unknown (KP.kind param)) params
                in
                let cont_uses_env = DA.continuation_uses_env dacc_after_body in
                CUE.compute_handler_env cont_uses_env cont Non_recursive
                  ~definition_typing_env_with_params_defined:
                    (DE.typing_env denv)
                  ~params ~param_types
              in
              let handler, user_data, uacc, is_single_inlinable_use =
                match uses with
                | No_uses ->
                  (* Don't simplify the handler if there aren't any uses:
                     otherwise, its code will be deleted but any continuation
                     usage information collected during its simplification will
                     remain. *)
                  let user_data, uacc = k dacc_after_body in
                  cont_handler, user_data, uacc, false
                | Uses { handler_typing_env; arg_types_by_use_id;
                         extra_params_and_args; is_single_inlinable_use; } ->
                  let typing_env, extra_params_and_args =
                    match Continuation.sort cont with
                    | Normal when is_single_inlinable_use ->
                      assert (not is_exn_handler);
                      handler_typing_env, extra_params_and_args
                    | Normal | Define_root_symbol ->
                      assert (not is_exn_handler);
                      let param_types =
                        TE.find_params handler_typing_env params
                      in
Format.eprintf "%a: trying to unbox.@ params %a,@ param types %a\n%!"
  Continuation.print cont
  KP.List.print params
  (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) param_types;
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
Format.eprintf "EPA:@ %a\n%!"
  Continuation_extra_params_and_args.print extra_params_and_args;
                  let dacc =
                    DA.with_denv dacc_after_body
                      (DE.with_typing_env denv typing_env)
                  in
                  let dacc, handler =
                    match Continuation.sort cont with
                    | Normal
                       when is_single_inlinable_use
                         || not at_unit_toplevel -> dacc, handler
                    | Return | Toplevel_return | Exn -> dacc, handler
                    | Normal | Define_root_symbol ->
                      (* CR mshinwell: This shouldn't be [assert] in the
                         [Define_root_symbol] case *)
                      assert at_unit_toplevel;
                      Reify_continuation_param_types.
                        lift_via_reification_of_continuation_param_types dacc
                          ~params ~extra_params_and_args ~handler
                  in
                  let dacc =
                    if at_unit_toplevel then dacc
                    else DA.map_denv dacc ~f:DE.set_not_at_unit_toplevel
                  in
                  try
                    let handler, user_data, uacc =
                      simplify_one_continuation_handler dacc cont Non_recursive
                        cont_handler ~params ~handler ~extra_params_and_args k
                    in
                    handler, user_data, uacc, is_single_inlinable_use
                  with Misc.Fatal_error -> begin
                    if !Clflags.flambda2_context_on_error then begin
                      Format.eprintf "\n%sContext is:%s simplifying \
                          continuation handler (inlinable? %b)@ %a@ with \
                          [extra_params_and_args]@ %a@ \
                          with downwards accumulator:@ %a\n"
                        (Flambda_colours.error ())
                        (Flambda_colours.normal ())
                        is_single_inlinable_use
                        CH.print cont_handler
                        Continuation_extra_params_and_args.print
                        extra_params_and_args
                        DA.print dacc
                    end;
                    raise Misc.Fatal_error
                  end
              in
              let uenv = UA.uenv uacc in
              let uenv_to_return = uenv in
              let uenv =
                match uses with
                | No_uses -> uenv
                | Uses _ ->
                  let can_inline =
                    if is_single_inlinable_use && (not is_exn_handler) then
                      Some handler
                    else
                      None
                  in
                  match can_inline with
                  | Some handler ->
                    (* CR mshinwell: tidy up *)
                    let arity =
                      match CH.behaviour handler with
                      | Unreachable { arity; }
                      | Alias_for { arity; _ }
                      | Apply_cont_with_constant_arg { arity; _ }
                      | Unknown { arity; } -> arity
                    in
                    UE.add_continuation_to_inline uenv cont scope arity
                      handler
                  | None ->
                    match CH.behaviour handler with
                    | Unreachable { arity; } ->
                      UE.add_unreachable_continuation uenv cont scope arity
                    | Alias_for { arity; alias_for; } ->
                      UE.add_continuation_alias uenv cont arity ~alias_for
                    | Apply_cont_with_constant_arg
                        { cont = destination_cont; arg = destination_arg;
                          arity; } ->
                      UE.add_continuation_apply_cont_with_constant_arg uenv cont
                        scope arity ~destination_cont ~destination_arg
                    | Unknown { arity; } ->
                      UE.add_continuation uenv cont scope arity
              in
              let uacc = UA.with_uenv uacc uenv in
              (handler, uenv_to_return, user_data), uacc))
        in
        (* The upwards environment of [uacc] is replaced so that out-of-scope
           continuation bindings do not end up in the accumulator. *)
        let uacc = UA.with_uenv uacc uenv' in
        body, result, user_data, uacc
      in
      Let_cont.create_non_recursive cont handler ~body, user_data, uacc)

(* CR mshinwell: We should not simplify recursive continuations with no
   entry point -- could loop forever.  (Need to think about this again.) *)
and simplify_recursive_let_cont_handlers
  : 'a. DA.t -> Recursive_let_cont_handlers.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc rec_handlers k ->
  let module CH = Continuation_handler in
  let module CPH = Continuation_params_and_handler in
  Recursive_let_cont_handlers.pattern_match rec_handlers
    ~f:(fun ~body rec_handlers ->
      assert (not (Continuation_handlers.contains_exn_handler rec_handlers));
      let definition_denv = DA.denv dacc in
      let original_cont_scope_level =
        DE.get_continuation_scope_level definition_denv
      in
      let handlers = Continuation_handlers.to_map rec_handlers in
      let cont, cont_handler =
        match Continuation.Map.bindings handlers with
        | [] | _ :: _ :: _ ->
          Misc.fatal_errorf "Support for simplification of multiply-recursive \
              continuations is not yet implemented"
        | [c] -> c
      in
      let params_and_handler = CH.params_and_handler cont_handler in
      CPH.pattern_match params_and_handler ~f:(fun params ~handler ->
        let arity = KP.List.arity params in
        let param_types =
          let arity = KP.List.arity_with_subkinds params in
          List.map (fun (kind : K.With_subkind.t) ->
              (* CR mshinwell: Move to [Flambda_type] *)
              match K.With_subkind.kind kind with
              | Value ->
                begin match K.With_subkind.subkind kind with
                | Anything -> T.any_value ()
                | Boxed_float -> T.any_boxed_float ()
                | Boxed_int32 -> T.any_boxed_int32 ()
                | Boxed_int64 -> T.any_boxed_int64 ()
                | Boxed_nativeint -> T.any_boxed_nativeint ()
                | Immediate -> T.any_tagged_immediate ()
                end
              | (Naked_number _ | Fabricated) as kind -> T.unknown kind)
            arity
        in
        let denv = DE.add_parameters (DA.denv dacc) params ~param_types in
        let dacc =
          DA.with_denv dacc (DE.increment_continuation_scope_level denv)
        in
        let body, (handlers, user_data), uacc =
          simplify_expr dacc body (fun dacc ->
            let uses =
              let cont_uses_env = DA.continuation_uses_env dacc in
              CUE.compute_handler_env cont_uses_env cont Recursive
                ~definition_typing_env_with_params_defined:
                  (DE.typing_env denv)
                ~params ~param_types
            in
            match uses with
            | No_uses ->
              Misc.fatal_error "To be continued"
            | Uses { handler_typing_env; arg_types_by_use_id;
                     extra_params_and_args; is_single_inlinable_use = _; } ->
              let typing_env, extra_params_and_args =
                Unbox_continuation_params.make_unboxing_decisions
                  handler_typing_env ~arg_types_by_use_id ~params
                  ~param_types extra_params_and_args
              in
              let dacc =
                DA.with_denv dacc
                  (DE.with_typing_env denv typing_env)
              in
              let dacc = DA.map_denv dacc ~f:DE.set_not_at_unit_toplevel in
              let dacc =
                DA.map_denv dacc ~f:(fun denv ->
                  DE.now_inside_handler_of_recursive_continuation denv
                    (DE.get_continuation_scope_level denv))
              in
              let handler, user_data, uacc =
                simplify_one_continuation_handler dacc cont Recursive
                  cont_handler ~params ~handler ~extra_params_and_args
                  (fun dacc ->
                    Format.eprintf "Usage:@ %a\nUsed vars:@ %a%!"
                      Downwards_usage.print (DA.Usage.get dacc)
                      Variable.Set.print (DA.Usage.used_variables dacc);
                    let user_data, uacc = k dacc in
                    let uacc =
                      UA.map_uenv uacc ~f:(fun uenv ->
                        UE.add_continuation uenv cont
                          original_cont_scope_level
                          arity)
                    in
                    user_data, uacc)
              in
              let handlers = Continuation.Map.singleton cont handler in
              (handlers, user_data), uacc)
        in
        let expr = Flambda.Let_cont.create_recursive handlers ~body in
        expr, user_data, uacc))

and simplify_let_cont
  : 'a. DA.t -> Let_cont.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc (let_cont : Let_cont.t) k ->
  match let_cont with
  | Non_recursive { handler; _ } ->
    simplify_non_recursive_let_cont_handler dacc handler k
  | Recursive handlers ->
    simplify_recursive_let_cont_handlers dacc handlers k

and simplify_direct_full_application
  : 'a. DA.t -> Apply.t
    -> (T.Function_declaration_type.Inlinable.t * Rec_info.t) option
    -> result_arity:Flambda_arity.t
    -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply function_decl_opt ~result_arity k ->
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
      | Do_not_inline ->
(*
Format.eprintf "Not inlining (%a) %a\n%!"
  Inlining_decision.Call_site_decision.print decision
  Apply.print apply;
*)
        None
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
  | Some (dacc, inlined) ->
(*
Format.eprintf "Simplifying inlined body with DE depth delta = %d\n%!"
  (DE.get_inlining_depth_increment (DA.denv dacc));
*)
    simplify_expr dacc inlined k
  | None ->
    let dacc, use_id =
      DA.record_continuation_use dacc (Apply.continuation apply) Non_inlinable
        ~typing_env_at_use:(DE.typing_env (DA.denv dacc))
        ~arg_types:(T.unknown_types_from_arity result_arity)
    in
    let dacc, exn_cont_use_id =
      DA.record_continuation_use dacc
        (Exn_continuation.exn_handler (Apply.exn_continuation apply))
        Non_inlinable
        ~typing_env_at_use:(DE.typing_env (DA.denv dacc))
        ~arg_types:(T.unknown_types_from_arity (
          Exn_continuation.arity (Apply.exn_continuation apply)))
    in
    let user_data, uacc = k dacc in
    let apply =
      Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
        apply
    in
    let expr =
      Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
        result_arity apply
    in
    expr, user_data, uacc

and simplify_direct_partial_application
  : 'a. DA.t -> Apply.t -> callee's_code_id:Code_id.t
    -> callee's_closure_id:Closure_id.t
    -> param_arity:Flambda_arity.t -> result_arity:Flambda_arity.t
    -> recursive:Recursive.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply ~callee's_code_id ~callee's_closure_id
      ~param_arity ~result_arity ~recursive k ->
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
          let param = Parameter.wrap (Variable.create "param") in
          Kinded_parameter.create param (K.With_subkind.create kind Anything))
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
    let body =
      let full_application =
        Apply.create ~callee:(Apply.callee apply)
          ~continuation:return_continuation
          (Apply.exn_continuation apply)
          ~args
          ~call_kind
          dbg
          ~inline:Default_inline
          ~inlining_depth:(Apply.inlining_depth apply)
      in
      List.fold_left (fun expr (closure_var, applied_arg) ->
          match Simple.must_be_var applied_arg with
          | None -> expr
          | Some applied_arg ->
            let applied_arg =
              VB.create applied_arg Name_mode.normal
            in
            Expr.create_let applied_arg
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
        (Apply.exn_continuation apply)
        remaining_params
        ~body
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
  simplify_expr dacc expr k

(* CR mshinwell: Should it be an error to encounter a non-direct application
   of a symbol after [Simplify]? This shouldn't usually happen, but I'm not 100%
   sure it cannot in every case. *)

and simplify_direct_over_application
  : 'a. DA.t -> Apply.t -> param_arity:Flambda_arity.t
    -> result_arity:Flambda_arity.t -> 'a k
    -> Expr.t * 'a * UA.t
= fun dacc apply ~param_arity ~result_arity:_ k ->
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
      let func_param =
        KP.create (Parameter.wrap func_var) K.With_subkind.any_value
      in
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
  simplify_expr dacc expr k

and simplify_direct_function_call
  : 'a. DA.t -> Apply.t -> callee's_code_id_from_type:Code_id.t
    -> callee's_code_id_from_call_kind:Code_id.t option
    -> callee's_closure_id:Closure_id.t
    -> param_arity:Flambda_arity.t -> result_arity:Flambda_arity.t
    -> recursive:Recursive.t -> arg_types:T.t list
    -> (T.Function_declaration_type.Inlinable.t * Rec_info.t) option
    -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply ~callee's_code_id_from_type ~callee's_code_id_from_call_kind
      ~callee's_closure_id ~param_arity ~result_arity ~recursive ~arg_types:_
      function_decl_opt k ->
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
      let code_age_rel = TE.code_age_relation (DE.typing_env (DA.denv dacc)) in
      Code_age_relation.meet code_age_rel callee's_code_id_from_call_kind
        callee's_code_id_from_type
  in
  match callee's_code_id with
  | Bottom ->
    let user_data, uacc = k dacc in
    Expr.create_invalid (), user_data, uacc
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
        ~result_arity k
    else if provided_num_args > num_params then
      simplify_direct_over_application dacc apply ~param_arity ~result_arity k
    else if provided_num_args > 0 && provided_num_args < num_params then
      simplify_direct_partial_application dacc apply ~callee's_code_id
        ~callee's_closure_id ~param_arity ~result_arity ~recursive k
    else
      Misc.fatal_errorf "Function with %d params when simplifying \
          direct OCaml function call with %d arguments: %a"
        num_params
        provided_num_args
        Apply.print apply

and simplify_function_call_where_callee's_type_unavailable
  : 'a. DA.t -> Apply.t -> Call_kind.Function_call.t -> arg_types:T.t list
    -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply (call : Call_kind.Function_call.t) ~arg_types k ->
  let cont = Apply.continuation apply in
  let denv = DA.denv dacc in
  let typing_env_at_use = DE.typing_env denv in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~typing_env_at_use:(DE.typing_env (DA.denv dacc))
      ~arg_types:(T.unknown_types_from_arity (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  let check_return_arity_and_record_return_cont_use ~return_arity =
(*
    let cont_arity = DA.continuation_arity dacc cont in
    if not (Flambda_arity.equal return_arity cont_arity) then begin
      Misc.fatal_errorf "Return arity (%a) on application's continuation@ \
          doesn't match return arity (%a) specified in [Call_kind]:@ %a"
        Flambda_arity.print cont_arity
        Flambda_arity.print return_arity
        Apply.print apply
    end;
*)
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
  let user_data, uacc = k dacc in
  let apply =
    Apply.with_call_kind apply call_kind
    |> Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
  in
  let expr =
    Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
      (Call_kind.return_arity call_kind) apply
  in
  expr, user_data, uacc

(* CR mshinwell: I've seen at least one case where a call of kind
   [Indirect_unknown_arity] has been generated with no warning, despite having
   [@inlined always]. *)

and simplify_function_call
  : 'a. DA.t -> Apply.t -> callee_ty:T.t -> Call_kind.Function_call.t
    -> arg_types:T.t list -> 'a k
    -> Expr.t * 'a * UA.t
= fun dacc apply ~callee_ty (call : Call_kind.Function_call.t) ~arg_types k ->
  let type_unavailable () =
    simplify_function_call_where_callee's_type_unavailable dacc apply call
      ~arg_types k
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
        ~callee's_code_id_from_call_kind ~callee's_closure_id ~arg_types
        ~param_arity:(I.param_arity inlinable)
        ~result_arity:(I.result_arity inlinable)
        ~recursive:(I.recursive inlinable)
        (Some (inlinable, function_decl_rec_info)) k
    | Ok (Non_inlinable non_inlinable) ->
      let module N = T.Function_declaration_type.Non_inlinable in
      let callee's_code_id_from_type = N.code_id non_inlinable in
      let callee's_code_id_from_call_kind =
        match call with
        | Direct { code_id; _ } -> Some code_id
        | Indirect_unknown_arity
        | Indirect_known_arity _ -> None
      in
      simplify_direct_function_call dacc apply ~callee's_code_id_from_type
        ~callee's_code_id_from_call_kind
        ~callee's_closure_id ~arg_types
        ~param_arity:(N.param_arity non_inlinable)
        ~result_arity:(N.result_arity non_inlinable)
        ~recursive:(N.recursive non_inlinable)
        None k
    | Bottom ->
      let user_data, uacc = k dacc in
      Expr.create_invalid (), user_data, uacc
    | Unknown -> type_unavailable ()
    end
  | Unknown -> type_unavailable ()
  | Invalid ->
    let user_data, uacc = k dacc in
    Expr.create_invalid (), user_data, uacc

and simplify_apply_shared dacc apply : _ Or_bottom.t * DA.t =
(*
  DA.check_continuation_is_bound dacc (Apply.continuation apply);
  DA.check_exn_continuation_is_bound dacc (Apply.exn_continuation apply);
*)
  let min_name_mode = Name_mode.normal in
  match S.simplify_simple dacc (Apply.callee apply) ~min_name_mode with
  | Bottom, _ty -> Bottom, dacc
  | Ok callee, callee_ty ->
    match S.simplify_simples dacc (Apply.args apply) ~min_name_mode with
    | Bottom -> Bottom, dacc
    | Ok args_with_types ->
      let args, arg_types = List.split args_with_types in
      let dacc = DA.Usage.record_uses_of_simples dacc args in
      let inlining_depth =
        DE.get_inlining_depth_increment (DA.denv dacc)
          + Apply.inlining_depth apply
      in
(*
Format.eprintf "Apply of %a: apply's inlining depth %d, DE's delta %d\n%!"
  Simple.print callee
  (Apply.inlining_depth apply)
  (DE.get_inlining_depth_increment (DA.denv dacc));
*)
      let apply =
        Apply.create ~callee
          ~continuation:(Apply.continuation apply)
          (Apply.exn_continuation apply)
          ~args
          ~call_kind:(Apply.call_kind apply)
          (* CR mshinwell: check if the next line has the args the right way
             around *)
          (DE.add_inlined_debuginfo' (DA.denv dacc) (Apply.dbg apply))
          ~inline:(Apply.inline apply)
          ~inlining_depth
      in
      Ok (callee_ty, apply, arg_types), dacc

and simplify_method_call
  : 'a. DA.t -> Apply.t -> callee_ty:T.t -> kind:Call_kind.method_kind
    -> obj:Simple.t -> arg_types:T.t list -> 'a k
    -> Expr.t * 'a * UA.t
= fun dacc apply ~callee_ty ~kind:_ ~obj ~arg_types k ->
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
      ~typing_env_at_use:(DE.typing_env (DA.denv dacc))
      ~arg_types:(T.unknown_types_from_arity (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  (* CR mshinwell: Need to record exception continuation use (check all other
     cases like this too) *)
  let user_data, uacc = k dacc in
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let expr =
    Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
      (Flambda_arity.create [K.value]) apply
  in
  expr, user_data, uacc

and simplify_c_call
  : 'a. DA.t -> Apply.t -> callee_ty:T.t -> param_arity:Flambda_arity.t
    -> return_arity:Flambda_arity.t -> arg_types:T.t list -> 'a k
    -> Expr.t * 'a * UA.t
= fun dacc apply ~callee_ty ~param_arity ~return_arity ~arg_types k ->
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
(* CR mshinwell: We can't do these checks (here and elsewhere) on [DA]
   any more.  Maybe we can check on [UA] after calling [k] instead.
  let cont = Apply.continuation apply in
  let cont_arity = DA.continuation_arity dacc cont in
  if not (Flambda_arity.equal cont_arity return_arity) then begin
    Misc.fatal_errorf "Arity %a of [Apply] continuation doesn't match \
        return arity %a of C callee:@ %a"
      Flambda_arity.print cont_arity
      Flambda_arity.print return_arity
      Apply.print apply
  end;
*)
  let dacc, use_id =
    DA.record_continuation_use dacc (Apply.continuation apply) Non_inlinable
      ~typing_env_at_use:(DE.typing_env (DA.denv dacc))
      ~arg_types:(T.unknown_types_from_arity return_arity)
  in
  let dacc, exn_cont_use_id =
    (* CR mshinwell: Try to factor out these stanzas, here and above. *)
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~typing_env_at_use:(DE.typing_env (DA.denv dacc))
      ~arg_types:(T.unknown_types_from_arity (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  let user_data, uacc = k dacc in
  (* CR mshinwell: Make sure that [resolve_continuation_aliases] has been
     called before building of any term that contains a continuation *)
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let expr =
    Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
      return_arity apply
  in
  expr, user_data, uacc

and simplify_apply
  : 'a. DA.t -> Apply.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply k ->
  match simplify_apply_shared dacc apply with
  | Bottom, dacc ->
    let user_data, uacc = k dacc in
    Expr.create_invalid (), user_data, uacc
  | Ok (callee_ty, apply, arg_types), dacc ->
    match Apply.call_kind apply with
    | Function call ->
      simplify_function_call dacc apply ~callee_ty call ~arg_types k
    | Method { kind; obj; } ->
      simplify_method_call dacc apply ~callee_ty ~kind ~obj ~arg_types k
    | C_call { alloc = _; param_arity; return_arity; } ->
      simplify_c_call dacc apply ~callee_ty ~param_arity ~return_arity
        ~arg_types k

and simplify_apply_cont
  : 'a. DA.t -> Apply_cont.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply_cont k ->
  let module AC = Apply_cont in
  let min_name_mode = Name_mode.normal in
  match S.simplify_simples dacc (AC.args apply_cont) ~min_name_mode with
  | Bottom ->
    let user_data, uacc = k dacc in
    Expr.create_invalid (), user_data, uacc
  | Ok args_with_types ->
    let args, arg_types = List.split args_with_types in
    let dacc = DA.Usage.record_uses_of_simples dacc args in
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
        ~typing_env_at_use:(DE.typing_env (DA.denv dacc))
        ~arg_types
    in
(*
Format.eprintf "Apply_cont %a: arg types %a, rewrite ID %a\n%!"
  Continuation.print (AC.continuation apply_cont)
  (Format.pp_print_list T.print) arg_types
  Apply_cont_rewrite_id.print rewrite_id;
Format.eprintf "Apply_cont starts out being %a\n%!"
  Apply_cont.print apply_cont;
*)
    let user_data, uacc = k dacc in
    let uenv = UA.uenv uacc in
    let rewrite = UE.find_apply_cont_rewrite uenv (AC.continuation apply_cont) in
    let cont =
      UE.resolve_continuation_aliases uenv (AC.continuation apply_cont)
    in
    let apply_cont_expr, apply_cont, args =
      let apply_cont = AC.update_continuation_and_args apply_cont cont ~args in
      let apply_cont =
        match AC.trap_action apply_cont with
        | None -> apply_cont
        | Some (Push { exn_handler; } | Pop { exn_handler; _ }) ->
          if UE.mem_continuation uenv exn_handler then apply_cont
          else AC.clear_trap_action apply_cont
      in
      (* CR mshinwell: Could remove the option type most likely if
         [Simplify_static] was fixed to handle the toplevel exn continuation
         properly. *)
      match rewrite with
      | None ->
        Expr.create_apply_cont apply_cont, apply_cont,
          Apply_cont.args apply_cont
      | Some rewrite ->
(*
Format.eprintf "Applying rewrite (ID %a):@ %a\n%!"
  Apply_cont_rewrite_id.print rewrite_id
  Apply_cont_rewrite.print rewrite;
*)
        Apply_cont_rewrite.rewrite_use rewrite rewrite_id apply_cont
    in
(*
Format.eprintf "Apply_cont is now %a\n%!" Expr.print apply_cont_expr;
*)
    if !Clflags.flambda_invariant_checks then begin
      Variable.Set.iter (fun var ->
          let name = Name.var var in
          if not (TE.mem (DE.typing_env (DA.denv dacc)) name) then begin
            Misc.fatal_errorf "[Apply_cont]@ %a after rewrite@ %a \
                contains unbound names in:@ %a"
              Expr.print apply_cont_expr
              (Misc.Stdlib.Option.print Apply_cont_rewrite.print) rewrite
              DA.print dacc
          end)
        (Name_occurrences.variables (Expr.free_names apply_cont_expr))
    end;
    let check_arity_against_args ~arity:_ = () in
(*
      if not (Flambda_arity.equal args_arity arity) then begin
        Misc.fatal_errorf "Arity of arguments in [Apply_cont] (%a) does not \
            match continuation's arity from the environment (%a):@ %a"
          Flambda_arity.print args_arity
          Flambda_arity.print arity
          AC.print apply_cont
      end
    in
*)
    let normal_case () = apply_cont_expr, user_data, uacc in
    match UE.find_continuation uenv cont with
    | Unknown { arity; } ->
      check_arity_against_args ~arity;
      normal_case ()
    | Unreachable { arity; } ->
      check_arity_against_args ~arity;
      (* N.B. We allow this transformation even if there is a trap action,
         on the basis that there wouldn't be any opportunity to collect any
         backtrace, even if the [Apply_cont] were compiled as "raise". *)
      Expr.create_invalid (), user_data, uacc
    | Apply_cont_with_constant_arg { cont = _; arg = _; arity; } ->
      check_arity_against_args ~arity;
      normal_case ()
    | Inline { arity; handler; } ->
      (* CR mshinwell: With -g, we can end up with continuations that are
         just a sequence of phantom lets then "goto".  These would normally
         be treated as aliases, but of course aren't in this scenario,
         unless the continuations are used linearly. *)
      (* CR mshinwell: maybe instead of [Inline] it should say "linearly used"
         or "stub" -- could avoid resimplification of linearly used ones maybe,
         although this wouldn't remove any parameter-to-argument [Let]s.
         However perhaps [Flambda_to_cmm] could deal with these. *)
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
(*
            let params_arity = KP.List.arity params in
            if not (Flambda_arity.equal params_arity args_arity) then begin
              Misc.fatal_errorf "Arity of arguments in [Apply_cont] does not \
                  match arity of parameters on handler (%a):@ %a"
                Flambda_arity.print params_arity
                AC.print apply_cont
            end;
*)
            (* CR mshinwell: Why does [New_let_binding] have a [Variable]? *)
            (* CR mshinwell: Should verify that names in the
               [Apply_cont_rewrite] are in scope. *)
            (* We can't easily call [simplify_expr] on the inlined body since
               [dacc] isn't the correct accumulator and environment any more.
               However there's no need to simplify the inlined body except to
               make use of parameter-to-argument bindings; we just leave them
               for a subsequent round of [Simplify] or [Un_cps] to clean up. *)
            let params_and_args =
              assert (List.compare_lengths params args = 0);
              List.map (fun (param, arg) ->
                  param, Named.create_simple arg)
                (List.combine params args)
            in
            let expr =
              Expr.bind_parameters ~bindings:params_and_args ~body:handler
            in
            expr, user_data, uacc)

(* CR mshinwell: Consider again having [Switch] arms taking arguments. *)
and simplify_switch
  : 'a. DA.t -> Switch.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc switch k ->
  let min_name_mode = Name_mode.normal in
  let scrutinee = Switch.scrutinee switch in
  let invalid () =
    let user_data, uacc = k dacc in
    Expr.create_invalid (), user_data, uacc
  in
  (*
  Format.eprintf "Simplifying Switch, env@ %a@ %a\n%!"
    TE.print (DE.typing_env (DA.denv dacc)) Switch_expr.print switch;
    *)
  match S.simplify_simple dacc scrutinee ~min_name_mode with
  | Bottom, _ty -> invalid ()
  | Ok scrutinee, scrutinee_ty ->
    let dacc = DA.Usage.record_use_of_simple dacc scrutinee in
  (*
    Format.eprintf "Simplified scrutinee is %a : %a\n%!"
      Simple.print scrutinee T.print scrutinee_ty;
      *)
    let arms = Switch.arms switch in
    let arms, dacc =
      let typing_env_at_use = DE.typing_env (DA.denv dacc) in
      Immediate.Map.fold (fun arm cont (arms, dacc) ->
          let shape =
            let imm = Immediate.int (Immediate.to_targetint arm) in
            T.this_naked_immediate imm
          in
          (*
          Format.eprintf "arm %a scrutinee_ty %a shape %a\n%!"
            Immediate.print arm T.print scrutinee_ty T.print shape;
            *)
          match T.meet typing_env_at_use scrutinee_ty shape with
          | Bottom ->
          (*
            Format.eprintf "Switch arm %a is bottom\n%!" Immediate.print arm;
            *)
            arms, dacc
          | Ok (_meet_ty, env_extension) ->
         (* 
            Format.eprintf "Switch arm %a : %a is kept\n%!"
              Immediate.print arm T.print meet_ty;
              *)
            let typing_env_at_use =
              TE.add_env_extension typing_env_at_use ~env_extension
            in
            let dacc, id =
              DA.record_continuation_use dacc cont Non_inlinable
                ~typing_env_at_use
                ~arg_types:[]
            in
            let arms = Immediate.Map.add arm (cont, id) arms in
            arms, dacc)
        arms
        (Immediate.Map.empty, dacc)
    in
    let user_data, uacc = k dacc in
    let uenv = UA.uenv uacc in
    let new_let_conts, arms, identity_arms, not_arms =
      Immediate.Map.fold
        (fun arm (cont, use_id)
             (new_let_conts, arms, identity_arms, not_arms) ->
          let new_let_cont =
            Simplify_common.add_wrapper_for_fixed_arity_continuation0 uacc cont
              ~use_id Flambda_arity.nullary
          in
          match new_let_cont with
          | None ->
            let cont = UE.resolve_continuation_aliases uenv cont in
            let normal_case ~identity_arms ~not_arms =
              let arms = Immediate.Map.add arm cont arms in
              new_let_conts, arms, identity_arms, not_arms
            in
            begin match UE.find_continuation uenv cont with
            | Unreachable _ -> new_let_conts, arms, identity_arms, not_arms
            | Apply_cont_with_constant_arg { cont; arg; arity = _; } ->
              begin match arg with
              | Tagged_immediate arg ->
                if Immediate.equal arm arg then
                  let identity_arms =
                    Immediate.Map.add arm cont identity_arms
                  in
                  normal_case ~identity_arms ~not_arms
                else if
                  (Immediate.equal arm Immediate.bool_true
                    && Immediate.equal arg Immediate.bool_false)
                  || 
                    (Immediate.equal arm Immediate.bool_false
                      && Immediate.equal arg Immediate.bool_true)
                then
                  let not_arms = Immediate.Map.add arm cont not_arms in
                  normal_case ~identity_arms ~not_arms
                else
                  normal_case ~identity_arms ~not_arms
              | Naked_immediate _ | Naked_float _ | Naked_int32 _
              | Naked_int64 _ | Naked_nativeint _ ->
                normal_case ~identity_arms ~not_arms
              end
            | Unknown _ | Inline _ -> normal_case ~identity_arms ~not_arms
            end
          | Some ((new_cont, _new_handler) as new_let_cont) ->
            let new_let_conts = new_let_cont :: new_let_conts in
            let arms = Immediate.Map.add arm new_cont arms in
            new_let_conts, arms, identity_arms, not_arms)
        arms
        ([], Immediate.Map.empty, Immediate.Map.empty,
          Immediate.Map.empty)
    in
    let switch_is_identity =
      let arm_discrs = Immediate.Map.keys arms in
      let identity_arms_discrs = Immediate.Map.keys identity_arms in
      if not (Immediate.Set.equal arm_discrs identity_arms_discrs) then
        None
      else
        Immediate.Map.data identity_arms
        |> Continuation.Set.of_list
        |> Continuation.Set.get_singleton
    in
    let switch_is_boolean_not =
      let arm_discrs = Immediate.Map.keys arms in
      let not_arms_discrs = Immediate.Map.keys not_arms in
      if (not (Immediate.Set.equal arm_discrs Immediate.all_bools))
        || (not (Immediate.Set.equal arm_discrs not_arms_discrs))
      then
        None
      else
        Immediate.Map.data not_arms
        |> Continuation.Set.of_list
        |> Continuation.Set.get_singleton
    in
    let create_tagged_scrutinee k =
      let bound_to = Variable.create "tagged_scrutinee" in
      let bound_vars =
        Bindable_let_bound.singleton (VB.create bound_to NM.normal)
      in
      let named =
        Named.create_prim (Unary (Box_number Untagged_immediate, scrutinee))
          Debuginfo.none
      in
      let { Simplify_named. bindings_outermost_first = bindings; dacc = _; } =
        Simplify_named.simplify_named dacc ~bound_vars named
      in
      let body = k ~tagged_scrutinee:(Simple.var bound_to) in
      Simplify_common.bind_let_bound ~bindings ~body, user_data, uacc
    in
    let body, user_data, uacc =
      match switch_is_identity with
      | Some dest ->
        create_tagged_scrutinee (fun ~tagged_scrutinee ->
          let apply_cont =
            Apply_cont.create dest ~args:[tagged_scrutinee] ~dbg:Debuginfo.none
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
          if Simple.is_const scrutinee
            && Immediate.Map.cardinal arms > 1
          then begin
            Misc.fatal_errorf "[Switch] with constant scrutinee (type: %a) \
                should have been simplified away:@ %a"
              T.print scrutinee_ty
              Expr.print expr
          end;
          expr, user_data, uacc
    in
    let expr =
      List.fold_left (fun body (new_cont, new_handler) ->
          Let_cont.create_non_recursive new_cont new_handler ~body)
        body
        new_let_conts
    in
    expr, user_data, uacc

and simplify_expr
  : 'a. DA.t -> Expr.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc expr k ->
  match Expr.descr expr with
  | Let let_expr -> simplify_let dacc let_expr k
  | Let_symbol let_symbol -> simplify_let_symbol dacc let_symbol k
  | Let_cont let_cont -> simplify_let_cont dacc let_cont k
  | Apply apply -> simplify_apply dacc apply k
  | Apply_cont apply_cont -> simplify_apply_cont dacc apply_cont k
  | Switch switch -> simplify_switch dacc switch k
  | Invalid _ ->
    (* CR mshinwell: Make sure that a program can be simplified to just
       [Invalid].  [Un_cps] should translate any [Invalid] that it sees as if
       it were [Halt_and_catch_fire]. *)
    let user_data, uacc = k dacc in
    expr, user_data, uacc
