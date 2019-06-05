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
module K = Flambda_kind
module KP = Kinded_parameter
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type

(* CR mshinwell: Need to simplify each [dbg] we come across. *)

let rec simplify_let env r (let_expr : Let.t) : Expr.t * R.t =
  let module L = Flambda.Let in
  (* CR mshinwell: Find out if we need the special fold function for lets. *)
  L.pattern_match let_expr ~f:(fun ~bound_var ~body ->
    let env, r, ty, (defining_expr : Reachable.t) =
      Simplify_named.simplify_named env r (L.defining_expr let_expr)
        ~result_var:bound_var
    in
    let kind = L.kind let_expr in
    let new_kind = T.kind ty in
    if not (K.equal new_kind kind) then begin
      Misc.fatal_errorf "Kind changed during simplification of [Let] \
          binding (new kind %a):@ %a"
        K.print new_kind
        L.print let_expr
    end;
    match defining_expr with
    | Invalid _ -> Expr.create_invalid (), r
    | Reachable defining_expr ->
      let body, r = simplify_expr env r body in
      let expr = Expr.create_let bound_var kind defining_expr body in
      expr, r)

and simplify_one_continuation_handler env r cont cont_handler =
  let params_and_handler =
    Continuation_handler.params_and_handler cont_handler
  in
  Continuation_params_and_handler.pattern_match params_and_handler
    ~f:(fun params ~handler ->
      let typing_env, arg_types =
        R.continuation_env_and_arg_types r env cont
      in
      let env =
        E.add_parameters (E.with_typing_environment env typing_env)
          params ~arg_types
      in
      let handler, r = simplify_expr env r handler in
      let params_and_handler =
        Continuation_params_and_handler.create params ~handler
      in
      let cont_handler =
        Continuation_handler.with_params_and_handler cont_handler
          params_and_handler
      in
      cont_handler, r)

and simplify_non_recursive_let_cont_handler env r let_cont non_rec_handler
      : Expr.t * R.t =
  let cont_handler = Non_recursive_let_cont_handler.handler non_rec_handler in
  Non_recursive_let_cont_handler.pattern_match non_rec_handler
    ~f:(fun cont ~body ->
      let body, r =
        let env =
          if Continuation_handler.is_exn_handler cont_handler then
            E.add_continuation env cont
              (Continuation_handler.arity cont_handler)
          else
            match Continuation_handler.behaviour cont_handler with
            | Unreachable { arity; } ->
              E.add_unreachable_continuation env cont arity
            | Alias_for { arity; alias_for; } ->
              E.add_continuation_alias env cont arity ~alias_for
            | Unknown { arity; } ->
              match Let_cont.should_inline_out let_cont with
              | None -> E.add_continuation env cont arity
              | Some non_rec_handler ->
                E.add_continuation_to_inline env cont arity
                  (Non_recursive_let_cont_handler.handler non_rec_handler)
        in
        let env = E.increment_continuation_scope_level env in
        let r = R.add_continuation r env cont in
        simplify_expr env r body
      in
      (* Lifted constants arising from simplification of the body need to
         be collected up and put in the environment before simplifying the
         handler, since the type(s) of the continuation's parameter(s) may
         involve the associated symbols. *)
      let env = E.add_lifted_constants env (R.get_lifted_constants r) in
      let cont_handler, r =
        simplify_one_continuation_handler env r cont cont_handler
      in
      Let_cont.create_non_recursive cont cont_handler ~body, r)

(* CR mshinwell: We should not simplify recursive continuations with no
   entry point -- could loop forever.  (Need to think about this again.) *)
and simplify_recursive_let_cont_handlers env r rec_handlers : Expr.t * R.t =
  Recursive_let_cont_handlers.pattern_match rec_handlers
    ~f:(fun ~body cont_handlers ->
      let cont_handlers = Continuation_handlers.to_map cont_handlers in
      let env, r =
        Continuation.Map.fold (fun cont cont_handler (env, r) ->
            let arity = Continuation_handler.arity cont_handler in
            let env = E.add_continuation env cont arity in
            let r = R.add_continuation r env cont in
            env, r)
          cont_handlers
          (env, r)
      in
      let body, r = simplify_expr env r body in
      let cont_handlers, r =
        Continuation.Map.fold (fun cont cont_handler (cont_handlers, r) ->
            let cont_handler, r =
              simplify_one_continuation_handler env r cont cont_handler
            in
            Continuation.Map.add cont cont_handler cont_handlers, r)
          cont_handlers
          (Continuation.Map.empty, r)
      in
      Let_cont.create_recursive cont_handlers ~body, r)

and simplify_let_cont env r (let_cont : Let_cont.t) : Expr.t * R.t =
  match let_cont with
  | Non_recursive { handler; _ } ->
    simplify_non_recursive_let_cont_handler env r let_cont handler
  | Recursive handlers ->
    simplify_recursive_let_cont_handlers env r handlers

and simplify_direct_full_application env r apply function_decl =
  let callee = Apply.callee apply in
  let args = Apply.args apply in
  let inlined =
    Inlining_transforms.inline env ~callee
      ~args function_decl
      ~apply_return_continuation:(Apply.continuation apply)
      ~apply_exn_continuation:(Apply.exn_continuation apply)
      (Apply.dbg apply)
      (Apply.inline apply)
  in
  match inlined with
  | Some (env, inlined) -> simplify_expr env r inlined
  | None -> Expr.create_apply apply, r

and simplify_direct_partial_application env r apply ~callee's_closure_id
      function_decl =
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
  Function_params_and_body.pattern_match
    (Function_declaration.params_and_body function_decl)
    ~f:(fun ~return_continuation:_ _exn_continuation params ~body:_
            ~my_closure:_ ->
      (* Since we're not inlining, the continuation parameters and body
         of the function declaration, etc., are irrelevant. *)
      let arity = List.length params in
      assert (arity > List.length args);
      let applied_args, remaining_params =
        Misc.Stdlib.List.map2_prefix (fun arg param ->
            let kind = KP.kind param in
            if not (K.equal kind K.value) then begin
              Misc.fatal_errorf "Non-[value] kind in partial application: %a"
                Apply.print apply
            end;
            arg)
          args params
      in
      let return_arity = Function_declaration.result_arity function_decl in
      begin match return_arity with
      | [kind] when Flambda_kind.equal kind K.value -> ()
      | _ ->
        Misc.fatal_errorf "Partially-applied function with non-[value] \
            return kind: %a"
          Apply.print apply
      end;
      let wrapper_var = Variable.create "partial" in
      let wrapper_taking_remaining_args =
        let return_continuation = Continuation.create () in
        let args = applied_args @ (List.map KP.simple remaining_params) in
        let call_kind =
          Call_kind.direct_function_call callee's_closure_id ~return_arity
        in
        let full_application =
          Apply.create ~callee:(Apply.callee apply)
            ~continuation:return_continuation
            (Apply.exn_continuation apply)
            ~args
            ~call_kind
            dbg
            ~inline:Default_inline
        in
        let applied_args_with_closure_vars =
          List.map (fun applied_arg ->
              Var_within_closure.wrap (Variable.create "arg"), applied_arg)
            applied_args
        in
        let my_closure = Variable.create "my_closure" in
        let body =
          List.fold_left (fun expr (closure_var, applied_arg) ->
              match Simple.must_be_var applied_arg with
              | None -> expr
              | Some applied_arg ->
                Expr.create_let applied_arg K.value
                  (Named.create_prim
                    (Unary (Project_var closure_var, Simple.var my_closure))
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
        let closure_origin =
          Function_declaration.closure_origin function_decl
        in
        let function_decl =
          Function_declaration.create ~closure_origin
            ~params_and_body
            ~result_arity:return_arity
            ~stub:true
            ~dbg
            ~inline:Default_inline
            ~is_a_functor:false
        in
        let closure_id = Closure_id.wrap (Variable.create "closure") in
        let function_decls =
          Function_declarations.create
            (Closure_id.Map.singleton closure_id function_decl)
        in
        let closure_elements =
          Var_within_closure.Map.of_list applied_args_with_closure_vars
        in
        Set_of_closures.create ~function_decls ~closure_elements
      in
      let apply_cont =
        Apply_cont.create (Apply.continuation apply)
          ~args:[Simple.var wrapper_var]
      in
      let expr =
        Expr.create_let wrapper_var K.value
          (Named.create_set_of_closures wrapper_taking_remaining_args)
          (Expr.create_apply_cont apply_cont)
      in
      simplify_expr env r expr)

and simplify_direct_over_application env r apply function_decl =
  Function_params_and_body.pattern_match
    (Function_declaration.params_and_body function_decl)
    ~f:(fun ~return_continuation:_ _exn_continuation params ~body:_
            ~my_closure:_ ->
      let arity = List.length params in
      let args = Apply.args apply in
      assert (arity < List.length args);
      let result_arity = Function_declaration.result_arity function_decl in
      let return_params =
        List.mapi (fun index kind ->
            let name = Format.sprintf "result%d" index in
            let var = Variable.create name in
            Kinded_parameter.create (Parameter.wrap var) kind)
          result_arity
      in
      let _, remaining_args = Misc.Stdlib.List.split_at arity args in
      let func_var = Variable.create "full_apply" in
      let perform_over_application =
        Apply.create ~callee:(Simple.var func_var)
          ~continuation:(Apply.continuation apply)
          (Apply.exn_continuation apply)
          ~args:remaining_args
          ~call_kind:(Call_kind.indirect_function_call_unknown_arity ())
          (Apply.dbg apply)
          ~inline:(Apply.inline apply)
      in
      let after_full_application = Continuation.create () in
      let after_full_application_handler =
        let params_and_handler =
          Continuation_params_and_handler.create return_params
            ~handler:(Expr.create_apply perform_over_application)
        in
        Continuation_handler.create ~params_and_handler
          ~stub:false
          ~is_exn_handler:false
      in
      let expr =
        Let_cont.create_non_recursive after_full_application
          after_full_application_handler
          ~body:(Expr.create_apply
            (Apply.with_continuation apply after_full_application))
      in
      simplify_expr env r expr)

and simplify_inlinable_direct_function_call env r apply
      ~callee's_closure_id function_decl ~arg_types =
  let params_arity = Function_declaration.params_arity function_decl in
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal params_arity args_arity) then begin
    Misc.fatal_errorf "Wrong argument arity for direct OCaml function call \
        (expected %a, found %a):@ %a"
      Flambda_arity.print params_arity
      Flambda_arity.print args_arity
      Apply.print apply
  end;
  let result_arity_of_application =
    Call_kind.return_arity (Apply.call_kind apply)
  in
  let result_arity = Function_declaration.result_arity function_decl in
  if not (Flambda_arity.equal result_arity_of_application result_arity)
  then begin
    Misc.fatal_errorf "Wrong return arity for direct OCaml function call \
        (expected %a, found %a):@ %a"
      Flambda_arity.print result_arity
      Flambda_arity.print result_arity_of_application
      Apply.print apply
  end;
  let call_kind =
    Call_kind.direct_function_call callee's_closure_id
      ~return_arity:result_arity
  in
  let apply = Apply.with_call_kind apply call_kind in
  let r =
    R.record_continuation_use r env (Apply.continuation apply)
      ~arg_types:(T.unknown_types_from_arity result_arity)
  in
  let args = Apply.args apply in
  let provided_num_args = List.length args in
  let num_params = List.length params_arity in
  if provided_num_args = num_params then
    simplify_direct_full_application env r apply function_decl
  else if provided_num_args > num_params then
    simplify_direct_over_application env r apply function_decl
  else if provided_num_args > 0 && provided_num_args < num_params then
    simplify_direct_partial_application env r apply
      ~callee's_closure_id function_decl
  else
    Misc.fatal_errorf "Function with %d params when simplifying \
        direct OCaml function call with %d arguments: %a"
      num_params
      provided_num_args
      Apply.print apply

and simplify_function_call_where_callee's_type_unavailable env r apply
      (call : Call_kind.Function_call.t) ~arg_types =
  let cont = Apply.continuation apply in
  let check_return_arity_and_record_return_cont_use ~return_arity =
    let cont_arity = E.continuation_arity env cont in
    if not (Flambda_arity.equal return_arity cont_arity) then begin
      Misc.fatal_errorf "Return arity (%a) on application's continuation@ \
          doesn't match return arity (%a) specified in [Call_kind]:@ %a"
        Flambda_arity.print cont_arity
        Flambda_arity.print return_arity
        Apply.print apply
    end;
    R.record_continuation_use r env cont
      ~arg_types:(T.unknown_types_from_arity return_arity)
  in
  let call_kind, r =
    match call with
    | Indirect_unknown_arity ->
      let r =
        try
          R.record_continuation_use r env (Apply.continuation apply)
            ~arg_types:[T.any_value ()]
        with Misc.Fatal_error -> begin
          Format.eprintf "\n%sContext is:%s simplifying [Apply]@ %a@ \
              in environment:@ %a@ with result structure:@ %a\n"
            (Misc.Color.bold_red ())
            (Misc.Color.reset ())
            Apply.print apply
            E.print env
            R.print r;
          raise Misc.Fatal_error
        end
      in
      Call_kind.indirect_function_call_unknown_arity (), r
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
      let r = check_return_arity_and_record_return_cont_use ~return_arity in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity
          ~return_arity
      in
      call_kind, r
    | Direct { return_arity; _ } ->
      let param_arity =
        (* Some types have regressed in precision.  Since this used to be a
           direct call, however, we know the function's arity even though we
           don't know which function it is. *)
        List.map (fun arg ->
            let _arg, ty = S.simplify_simple env arg in
            T.kind ty)
          (Apply.args apply)
      in
      let r = check_return_arity_and_record_return_cont_use ~return_arity in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity
          ~return_arity
      in
      call_kind, r
  in
  Expr.create_apply (Apply.with_call_kind apply call_kind), r

and simplify_function_call env r apply ~callee_ty
      (call : Call_kind.Function_call.t) ~arg_types : Expr.t * R.t =
  let type_unavailable () =
    simplify_function_call_where_callee's_type_unavailable env r apply call
      ~arg_types
  in
  (* CR mshinwell: Should this be using [meet_shape], like for primitives? *)
  match T.prove_single_closures_entry (E.typing_env env) callee_ty with
  | Proved (callee's_closure_id, func_decl_type) ->
    (* CR mshinwell: We should check that the [set_of_closures] in the
       [closures_entry] structure in the type does indeed contain the
       closure in question. *)
    begin match T.term_language_function_declaration func_decl_type with
    | Some function_decl ->
      begin match call with
      | Direct { closure_id; _ } ->
        if not (Closure_id.equal closure_id callee's_closure_id) then begin
          Misc.fatal_errorf "Closure ID %a in application doesn't match \
              closure ID %a discovered via typing.@ Application:@ %a"
            Closure_id.print closure_id
            Closure_id.print callee's_closure_id
            Apply.print apply
        end
      | Indirect_unknown_arity
      | Indirect_known_arity _ -> ()
      end;
      simplify_inlinable_direct_function_call env r apply
        ~callee's_closure_id function_decl ~arg_types
    | None -> type_unavailable ()
    end
  | Unknown -> type_unavailable ()
  | Invalid -> Expr.create_invalid (), r

and simplify_apply_shared env r apply =
  let cont = E.resolve_continuation_aliases env (Apply.continuation apply) in
  E.check_exn_continuation_is_bound env (Apply.exn_continuation apply);
  let callee, callee_ty = S.simplify_simple env (Apply.callee apply) in
  let args_with_types = S.simplify_simples env (Apply.args apply) in
  let args, arg_types = List.split args_with_types in
  let apply =
    Apply.with_continuation_callee_and_args apply cont ~callee ~args
  in
  callee_ty, apply, arg_types, r

and simplify_method_call env r apply ~callee_ty ~kind:_ ~obj ~arg_types =
  let callee_kind = T.kind callee_ty in
  if not (K.is_value callee_kind) then begin
    Misc.fatal_errorf "Method call with callee of wrong kind %a: %a"
      K.print callee_kind
      T.print callee_ty
  end;
  E.check_simple_is_bound env obj;
  let expected_arity = List.map (fun _ -> K.value) arg_types in
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal expected_arity args_arity) then begin
    Misc.fatal_errorf "All arguments to a method call must be of kind \
        [value]:@ %a"
      Apply.print apply
  end;
  let r =
    R.record_continuation_use r env (Apply.continuation apply)
      ~arg_types:[T.any_value ()]
  in
  Expr.create_apply apply, r

and simplify_c_call env r apply ~callee_ty ~param_arity ~return_arity
      ~arg_types =
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
  let cont = Apply.continuation apply in
  let cont_arity = E.continuation_arity env cont in
  if not (Flambda_arity.equal cont_arity return_arity) then begin
    Misc.fatal_errorf "Arity %a of [Apply] continuation doesn't match \
        return arity %a of C callee:@ %a"
      Flambda_arity.print cont_arity
      Flambda_arity.print return_arity
      Apply.print apply
  end;
  let r =
    R.record_continuation_use r env (Apply.continuation apply)
      ~arg_types:(T.unknown_types_from_arity return_arity)
  in
  Expr.create_apply apply, r

and simplify_apply env r apply : Expr.t * R.t =
  let callee_ty, apply, arg_types, r = simplify_apply_shared env r apply in
  match Apply.call_kind apply with
  | Function call ->
    simplify_function_call env r apply ~callee_ty call ~arg_types
  | Method { kind; obj; } ->
    simplify_method_call env r apply ~callee_ty ~kind ~obj ~arg_types
  | C_call { alloc = _; param_arity; return_arity; } ->
    simplify_c_call env r apply ~callee_ty ~param_arity ~return_arity
      ~arg_types

and simplify_apply_cont env r apply_cont : Expr.t * R.t =
  let module AC = Apply_cont in
  let cont =
    E.resolve_continuation_aliases env (AC.continuation apply_cont)
  in
  (* CR mshinwell: Consider changing interface of [simplify_simples] *)
  let args_with_types = S.simplify_simples env (AC.args apply_cont) in
  let args, arg_types = List.split args_with_types in
  let args_arity = T.arity_of_list arg_types in
  let r = R.record_continuation_use r env cont ~arg_types in
  let check_arity_against_args ~arity =
    if not (Flambda_arity.equal args_arity arity) then begin
      Misc.fatal_errorf "Arity of arguments in [Apply_cont] does not \
          match continuation's arity from the environment (%a):@ %a"
        Flambda_arity.print arity
        AC.print apply_cont
    end
  in
  let normal_case () =
    Expr.create_apply_cont
      (AC.update_continuation_and_args apply_cont cont ~args), r
  in
  match E.find_continuation env cont with
  | Unknown { arity; } ->
    check_arity_against_args ~arity;
    normal_case ()
  | Unreachable { arity; } ->
    check_arity_against_args ~arity;
    (* N.B. We allow this transformation even if there is a trap action,
       on the basis that there wouldn't be any opportunity to collect any
       backtrace, even if the [Apply_cont] were compiled as "raise". *)
    Expr.create_invalid (), r
  | Inline { arity; handler; } ->
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
          let params_arity = KP.List.arity params in
          if not (Flambda_arity.equal params_arity args_arity) then begin
            Misc.fatal_errorf "Arity of arguments in [Apply_cont] does not \
                match arity of parameters on handler (%a):@ %a"
              Flambda_arity.print params_arity
              AC.print apply_cont
          end;
          let expr =
            Expr.bind_parameters_to_simples ~bind:params ~target:args handler
          in
          try simplify_expr env r expr
          with Misc.Fatal_error -> begin
            Format.eprintf "\n%sContext is:%s inlining [Apply_cont]@ %a.@ \
                The inlined body was:@ %a@ in environment:@ %a\n"
              (Misc.Color.bold_red ())
              (Misc.Color.reset ())
              AC.print apply_cont
              Expr.print expr
              E.print env;
            raise Misc.Fatal_error
          end)

and simplify_switch env r (switch : Flambda.Switch.t) : Expr.t * R.t =
  let reachable_arms =
    Discriminant.Map.filter_map (Switch.arms switch) ~f:(fun _arm cont ->
      let cont = E.resolve_continuation_aliases env cont in
      match E.find_continuation env cont with
      | Unreachable _ -> None
      | Unknown _ | Inline _ -> Some cont)
  in
  let expr =
    Expr.create_switch ~scrutinee:(Switch.scrutinee switch)
      ~arms:reachable_arms
  in
  expr, r

and simplify_expr env r expr : Expr.t * R.t =
  match Expr.descr expr with
  | Let let_expr -> simplify_let env r let_expr
  | Let_cont let_cont -> simplify_let_cont env r let_cont
  | Apply apply -> simplify_apply env r apply
  | Apply_cont apply_cont -> simplify_apply_cont env r apply_cont
  | Switch switch -> simplify_switch env r switch
  | Invalid _ -> expr, r
