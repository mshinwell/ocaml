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

module CUE = Continuation_uses_env
module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module K = Flambda_kind
module KP = Kinded_parameter
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type
module UA = Upwards_acc
module UE = Simplify_env_and_result.Upwards_env

(* CR mshinwell: Need to simplify each [dbg] we come across. *)

let rec simplify_let dacc (let_expr : Let.t) k =
  let module L = Flambda.Let in
  (* CR mshinwell: Find out if we need the special fold function for lets. *)
  L.pattern_match let_expr ~f:(fun ~bound_var ~body ->
    let dacc, ty, (defining_expr : Reachable.t) =
      Simplify_named.simplify_named dacc (L.defining_expr let_expr)
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
    | Invalid _ ->
      let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
      Expr.create_invalid (), user_data, uacc
    | Reachable defining_expr ->
      let body, user_data, uacc = simplify_expr dacc body k in
      let expr = Expr.create_let bound_var kind defining_expr body in
      expr, user_data, uacc)

and simplify_one_continuation_handler dacc cont ~arg_types cont_handler k =
  let module CH = Continuation_handler in
  let module CPH = Continuation_params_and_handler in
  CPH.pattern_match (CH.params_and_handler cont_handler)
    ~f:(fun params ~handler ->
      let denv =
        DA.map_denv dacc ~f:(fun denv ->
          DE.add_parameters denv params ~arg_types)
      in
      let handler, user_data, uacc = simplify_expr dacc handler k in
      let used_params, unused_params =
        let free_names = Expr.free_names handler in
        List.partition (fun param ->
            Name_occurrences.mem_var free_names (KP.var param))
          params
      in
      let cont_handler =
        CH.with_params_and_handler cont_handler
          (CPH.create used_params ~handler)
      in
      match unused_params with
      | [] -> cont_handler, None, user_data, uacc
      | _::_ ->
        let original_cont = Continuation.create () in
        let wrapper_cont_handler =
          Continuation_handler.create
            ~params_and_handler:
              (Continuation_params_and_handler.create params ~handler:
                (Expr.create_apply_cont (
                  Apply_cont.create original_cont
                    ~args:(KP.List.simples used_params))))
            ~stub:true
            ~is_exn_handler:false
        in
        wrapper_cont_handler, Some (original_cont, cont_handler),
          user_data, uacc)

and simplify_non_recursive_let_cont_handler dacc let_cont non_rec_handler k =
  let cont_handler = Non_recursive_let_cont_handler.handler non_rec_handler in
  let definition_denv = DA.denv dacc in
  let definition_scope_level =
    DE.get_continuation_scope_level definition_denv
  in
  Non_recursive_let_cont_handler.pattern_match non_rec_handler
    ~f:(fun cont ~body ->
      let body, (cont_handler, additional_cont_handler, user_data, uenv'),
          uacc =
        let arity = Continuation_handler.arity cont_handler in
        let dacc =
          DA.map_denv dacc ~f:(fun denv ->
            DE.increment_continuation_scope_level denv)
        in
        let dacc =
          DA.add_continuation dacc cont ~definition_scope_level arity
        in
        simplify_expr dacc body (fun cont_uses_env r ->
          (* The environment currently in [dacc] is not the correct environment
             for simplifying the handler. Instead, we must use the environment
             of the [Let_cont] definition itself, augmented with any lifted
             constants arising from simplification of the body. (These need to
             be present since the type(s) of the continuation's parameter(s) may
             involve the associated symbols.)
             The environment in [dacc] does, however, contain the usage
             information for the continuation.  This will be used to
             compute the types of the continuation's parameter(s). *)
          let definition_denv =
            DE.add_lifted_constants definition_denv
              (R.get_lifted_constants r)
          in
          let typing_env, arg_types =
            CUE.continuation_env_and_arg_types cont_uses_env 
              ~definition_typing_env:(DE.typing_env definition_denv) cont
          in
          let definition_denv =
            DE.with_typing_environment definition_denv typing_env
          in
          let dacc = DA.create definition_denv cont_uses_env r in
          let cont_handler, additional_cont_handler, user_data, uacc =
            simplify_one_continuation_handler dacc cont ~arg_types
              cont_handler k
          in
          let uenv = UA.uenv uacc in
          let uenv' = uenv in
          let uenv =
            if Continuation_handler.is_exn_handler cont_handler then
              UE.add_continuation uenv cont definition_scope_level arity
            else
              match Continuation_handler.behaviour cont_handler with
              | Unreachable { arity; } ->
                UE.add_unreachable_continuation uenv cont
                  definition_scope_level arity
              | Alias_for { arity; alias_for; } ->
                UE.add_continuation_alias uenv cont arity ~alias_for
              | Unknown { arity; } ->
                let num_uses = DA.num_continuation_uses dacc cont in
                if num_uses = 1 || Continuation_handler.stub cont_handler then
                  UE.add_continuation_to_inline uenv cont
                    definition_scope_level arity
                    (Non_recursive_let_cont_handler.handler non_rec_handler)
                else
                  UE.add_continuation uenv cont definition_scope_level arity
          in
          let uacc = UA.with_uenv uacc uenv in
          (cont_handler, additional_cont_handler, uenv', user_data), uacc)
      in
      (* The upwards environment of [uacc] is replaced so that out-of-scope
         continuation bindings do not end up in the accumulator. *)
      let uacc = UA.with_uenv uacc uenv' in
      match additional_cont_handler with
      | None ->
        Let_cont.create_non_recursive cont cont_handler ~body,
          user_data, uacc
      | Some (additional_cont, additional_cont_handler) ->
        Let_cont.create_non_recursive additional_cont additional_cont_handler
            ~body:(Let_cont.create_non_recursive cont cont_handler ~body),
          user_data, uacc)

(* CR mshinwell: We should not simplify recursive continuations with no
   entry point -- could loop forever.  (Need to think about this again.) *)
and simplify_recursive_let_cont_handlers _dacc _rec_handlers _k =
  Misc.fatal_error "Temporarily disabled pending environment reorganisation"
(*
  Recursive_let_cont_handlers.pattern_match rec_handlers
    ~f:(fun ~body cont_handlers ->
      let cont_handlers = Continuation_handlers.to_map cont_handlers in

        let dacc =
          DA.map_denv dacc (fun denv ->
            DE.increment_continuation_scope_level
              (DE.add_continuation denv cont arity))
        in

      let denv =
        Continuation.Map.fold (fun cont cont_handler (env, r) ->
            let arity = Continuation_handler.arity cont_handler in
            let env = DE.add_continuation env cont arity in
            env, r)
          cont_handlers
          (env, r)
      in

      let body, uacc = simplify_expr dacc r body in
      let cont_handlers, uacc =
        Continuation.Map.fold
          (fun cont cont_handler (cont_handlers, uacc) ->
            let cont_handler, additional_cont_handlers, uacc =
              simplify_one_continuation_handler denv uenv r cont cont_handler
            in
            let cont_handlers =
              Continuation.Map.add cont cont_handler cont_handlers
            in
            (* CR-someday mshinwell: We won't yet remove invariant parameters
               of recursive continuations. *)
            let cont_handlers =
              match additional_cont_handlers with
              | None -> cont_handlers
              | Some (additional_cont, additional_cont_handler) ->
                Continuation.Map.add additional_cont additional_cont_handler
                  cont_handlers
            in
            cont_handlers, uacc)
          cont_handlers
          (Continuation.Map.empty, uacc)
      in
      Let_cont.create_recursive cont_handlers ~body, uacc)
*)

and simplify_let_cont dacc (let_cont : Let_cont.t) k =
  match let_cont with
  | Non_recursive { handler; _ } ->
    simplify_non_recursive_let_cont_handler dacc let_cont handler k
  | Recursive handlers ->
    simplify_recursive_let_cont_handlers dacc handlers k

and simplify_direct_full_application dacc apply function_decl_opt k =
  let callee = Apply.callee apply in
  let args = Apply.args apply in
  let inlined =
    match function_decl_opt with
    | None -> None
    | Some function_decl ->
      Inlining_transforms.inline dacc ~callee
        ~args function_decl
        ~apply_return_continuation:(Apply.continuation apply)
        ~apply_exn_continuation:(Apply.exn_continuation apply)
        (Apply.dbg apply)
        (Apply.inline apply)
  in
  match inlined with
  | Some (dacc, inlined) -> simplify_expr dacc inlined k
  | None ->
    let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
    Expr.create_apply apply, user_data, uacc

and simplify_direct_partial_application dacc apply ~callee's_closure_id
      ~param_arity ~result_arity k =
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
  let wrapper_var = Variable.create "partial" in
  let wrapper_taking_remaining_args =
    let return_continuation = Continuation.create () in
    let remaining_params =
      List.map (fun kind ->
          let param = Parameter.wrap (Variable.create "param") in
          Kinded_parameter.create param kind)
        remaining_param_arity
    in
    let args = applied_args @ (List.map KP.simple remaining_params) in
    let call_kind =
      Call_kind.direct_function_call callee's_closure_id
        ~return_arity:result_arity
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
    let closure_origin = Closure_origin.create callee's_closure_id in
    let function_decl =
      Function_declaration.create ~closure_origin
        ~params_and_body
        ~result_arity
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
  simplify_expr dacc expr k

and simplify_direct_over_application dacc apply ~param_arity ~result_arity k =
  let arity = List.length param_arity in
  let args = Apply.args apply in
  assert (arity < List.length args);
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
  simplify_expr dacc expr k

and simplify_direct_function_call dacc apply
      ~callee's_closure_id ~param_arity ~result_arity ~arg_types
      function_decl_opt k =
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal param_arity args_arity) then begin
    Misc.fatal_errorf "Wrong argument arity for direct OCaml function call \
        (expected %a, found %a):@ %a"
      Flambda_arity.print param_arity
      Flambda_arity.print args_arity
      Apply.print apply
  end;
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
  let call_kind =
    Call_kind.direct_function_call callee's_closure_id
      ~return_arity:result_arity
  in
  let apply = Apply.with_call_kind apply call_kind in
  let dacc =
    DA.record_continuation_use dacc (Apply.continuation apply)
      ~typing_env_at_use:(DE.typing_env (DA.denv dacc))
      ~arg_types:(T.unknown_types_from_arity result_arity)
  in
  let args = Apply.args apply in
  let provided_num_args = List.length args in
  let num_params = List.length param_arity in
  if provided_num_args = num_params then
    simplify_direct_full_application dacc apply function_decl_opt k
  else if provided_num_args > num_params then
    simplify_direct_over_application dacc apply ~param_arity ~result_arity k
  else if provided_num_args > 0 && provided_num_args < num_params then
    simplify_direct_partial_application dacc apply
      ~callee's_closure_id ~param_arity ~result_arity k
  else
    Misc.fatal_errorf "Function with %d params when simplifying \
        direct OCaml function call with %d arguments: %a"
      num_params
      provided_num_args
      Apply.print apply

and simplify_function_call_where_callee's_type_unavailable dacc apply
      (call : Call_kind.Function_call.t) ~arg_types k =
  let cont = Apply.continuation apply in
  let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
  let denv = DA.denv dacc in
  let typing_env_at_use = DE.typing_env denv in
  let check_return_arity_and_record_return_cont_use ~return_arity =
    let cont_arity = UE.continuation_arity (UA.uenv uacc) cont in
    if not (Flambda_arity.equal return_arity cont_arity) then begin
      Misc.fatal_errorf "Return arity (%a) on application's continuation@ \
          doesn't match return arity (%a) specified in [Call_kind]:@ %a"
        Flambda_arity.print cont_arity
        Flambda_arity.print return_arity
        Apply.print apply
    end;
    DA.record_continuation_use dacc cont ~typing_env_at_use
      ~arg_types:(T.unknown_types_from_arity return_arity)
  in
  let call_kind, denv =
    match call with
    | Indirect_unknown_arity ->
      let denv =
        try
          DA.record_continuation_use dacc (Apply.continuation apply)
            ~typing_env_at_use ~arg_types:[T.any_value ()]
        with Misc.Fatal_error -> begin
          Format.eprintf "\n%sContext is:%s simplifying [Apply]@ %a@ \
              with downwards accumulator:@ %a\n"
            (Misc.Color.bold_red ())
            (Misc.Color.reset ())
            Apply.print apply
            DA.print dacc;
          raise Misc.Fatal_error
        end
      in
      Call_kind.indirect_function_call_unknown_arity (), denv
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
      let denv = check_return_arity_and_record_return_cont_use ~return_arity in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity
          ~return_arity
      in
      call_kind, denv
    | Direct { return_arity; _ } ->
      let param_arity =
        (* Some types have regressed in precision.  Since this used to be a
           direct call, however, we know the function's arity even though we
           don't know which function it is. *)
        List.map (fun arg ->
            let _arg, ty = S.simplify_simple dacc arg in
            T.kind ty)
          (Apply.args apply)
      in
      let denv = check_return_arity_and_record_return_cont_use ~return_arity in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity
          ~return_arity
      in
      call_kind, denv
  in
  Expr.create_apply (Apply.with_call_kind apply call_kind), user_data, uacc

and simplify_function_call dacc apply ~callee_ty
      (call : Call_kind.Function_call.t) ~arg_types k =
  let type_unavailable () =
    simplify_function_call_where_callee's_type_unavailable dacc apply call
      ~arg_types
  in
  (* CR mshinwell: Should this be using [meet_shape], like for primitives? *)
  match T.prove_single_closures_entry (E.typing_env env) callee_ty with
  | Proved (callee's_closure_id, func_decl_type) ->
    (* CR mshinwell: We should check that the [set_of_closures] in the
       [closures_entry] structure in the type does indeed contain the
       closure in question. *)
    begin match func_decl_type with
    | Known (Inlinable { function_decl; }) ->
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
      simplify_direct_function_call env r apply
        ~callee's_closure_id ~arg_types
        ~param_arity:(Function_declaration.params_arity function_decl)
        ~result_arity:(Function_declaration.result_arity function_decl)
        (Some function_decl)
    | Known (Non_inlinable { param_arity; result_arity; }) ->
      simplify_direct_function_call env r apply
        ~callee's_closure_id ~arg_types
        ~param_arity ~result_arity
        None
    | Unknown -> type_unavailable ()
    end
  | Unknown -> type_unavailable ()
  | Invalid -> Expr.create_invalid (), uacc

and simplify_apply_shared dacc apply =
  (* XXX *)
  let cont = E.resolve_continuation_aliases env (Apply.continuation apply) in
  E.check_exn_continuation_is_bound env (Apply.exn_continuation apply);
  let callee, callee_ty = S.simplify_simple env (Apply.callee apply) in
  let args_with_types = S.simplify_simples env (Apply.args apply) in
  let args, arg_types = List.split args_with_types in
  let apply =
    Apply.with_continuation_callee_and_args apply cont ~callee ~args
  in
  callee_ty, apply, arg_types, r

and simplify_method_call dacc apply ~callee_ty ~kind:_ ~obj ~arg_types k =
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
  let denv =
    DE.record_continuation_use denv (Apply.continuation apply)
      ~arg_types:[T.any_value ()]
  in
  let user_data, uacc = k (DA.with_denv dacc denv) in
  Expr.create_apply apply, user_data, uacc

and simplify_c_call dacc apply ~callee_ty ~param_arity ~return_arity
      ~arg_types k =
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
  let denv =
    DE.record_continuation_use denv (Apply.continuation apply)
      ~arg_types:(T.unknown_types_from_arity return_arity)
  in
  let uacc = k (DA.with_denv dacc denv) in
  Expr.create_apply apply, uacc

and simplify_apply dacc apply k =
  let callee_ty, apply, arg_types, r = simplify_apply_shared env r apply in
  match Apply.call_kind apply with
  | Function call ->
    simplify_function_call dacc apply ~callee_ty call ~arg_types k
  | Method { kind; obj; } ->
    simplify_method_call dacc apply ~callee_ty ~kind ~obj ~arg_types k
  | C_call { alloc = _; param_arity; return_arity; } ->
    simplify_c_call dacc apply ~callee_ty ~param_arity ~return_arity
      ~arg_types k

and simplify_apply_cont dacc apply_cont k =
  let module AC = Apply_cont in
  (* CR mshinwell: Consider changing interface of [simplify_simples] *)
  let args_with_types = S.simplify_simples denv (AC.args apply_cont) in
  let args, arg_types = List.split args_with_types in
  let args_arity = T.arity_of_list arg_types in
  let dacc =
    DA.with_denv dacc
      (fun denv -> DE.record_continuation_use denv cont ~arg_types)
  in
  let uacc = k dacc in
  let uenv = UA.uenv uacc in
  let cont =
    UE.resolve_continuation_aliases uenv (AC.continuation apply_cont)
  in
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
      (AC.update_continuation_and_args apply_cont cont ~args), uacc
  in
  match UE.find_continuation uenv cont with
  | Unknown { arity; } ->
    check_arity_against_args ~arity;
    normal_case ()
  | Unreachable { arity; } ->
    check_arity_against_args ~arity;
    (* N.B. We allow this transformation even if there is a trap action,
       on the basis that there wouldn't be any opportunity to collect any
       backtrace, even if the [Apply_cont] were compiled as "raise". *)
    Expr.create_invalid (), uacc
  | Inline { arity; handler; } ->
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
          try simplify_expr dacc expr (fun _dacc -> uacc)
          with Misc.Fatal_error -> begin
            Format.eprintf "\n%sContext is:%s inlining [Apply_cont]@ %a.@ \
                The inlined body was:@ %a@ in environment:@ %a\n"
              (Misc.Color.bold_red ())
              (Misc.Color.reset ())
              AC.print apply_cont
              Expr.print expr
              DE.print (DA.denv dacc);
            raise Misc.Fatal_error
          end)

and simplify_switch dacc (switch : Flambda.Switch.t) k =
  let user_data, uacc = k dacc in
  let uenv = UA.uenv uacc in
  let reachable_arms =
    Discriminant.Map.filter_map (Switch.arms switch) ~f:(fun _arm cont ->
      let cont = UE.resolve_continuation_aliases uenv cont in
      match UE.find_continuation uenv cont with
      | Unreachable _ -> None
      | Unknown _ | Inline _ -> Some cont)
  in
  let expr =
    Expr.create_switch ~scrutinee:(Switch.scrutinee switch)
      ~arms:reachable_arms
  in
  expr, user_data, uacc

and simplify_expr
  : 'a. DA.t -> Expr.t -> (CUE.t -> R.t -> ('a * UA.t)) -> Expr.t * 'a * UA.t
= fun dacc expr k ->
  match Expr.descr expr with
  | Let let_expr -> simplify_let env r let_expr k
  | Let_cont let_cont -> simplify_let_cont env r let_cont k
  | Apply apply -> simplify_apply env r apply k
  | Apply_cont apply_cont -> simplify_apply_cont env r apply_cont k
  | Switch switch -> simplify_switch env r switch k
  | Invalid _ ->
    let user_data, uacc = k dacc in
    expr, user_data, uacc
