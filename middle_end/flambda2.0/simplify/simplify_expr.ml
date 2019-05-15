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
(* CR mshinwell: Need function to resolve continuation aliases.
   Should return Unknown _ | Unreachable | Inline _ *)

module Make (Simplify_named : Simplify_named_intf.S) = struct
  let rec simplify_let env r (let_expr : Flambda.Let.t) : Expr.t * R.t =
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
        Misc.fatal_errorf "Kind error during simplification of [Let] \
            binding (old kind %a, new kind %a):@ %a"
          K.print kind
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
        let arity = KP.List.arity params in
        let arg_types = R.continuation_arg_types r cont ~arity in
        let env = E.add_parameters env params ~arg_types in
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
        (* Lifted constants arising from simplification of the body need to
           be collected up and put in the environment before simplifying the
           handler, since the type(s) of the continuation's parameter(s) may
           involve the associated symbols. *)
        let already_lifted_constants = R.get_lifted_constants r in
        let body, r =
          let env =
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
          simplify_expr env r body
        in
        let lifted_constants =
          Symbol.Map.diff_domains (R.get_lifted_constants r)
            already_lifted_constants
        in
        let env = E.add_lifted_constants env lifted_constants in
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
        let env =
          Continuation.Map.fold (fun cont cont_handler env ->
              let arity = Continuation_handler.arity cont_handler in
              E.add_continuation env cont arity)
            cont_handlers
            env
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

  and simplify_direct_full_application env r apply call function_decl =
    let callee = Apply.callee apply in
    let args = Apply.args apply in
    let inlined =
      Inlining_transforms.inline env ~callee:(Simple.name callee)
        ~args function_decl
        ~apply_return_continuation:(Apply.continuation apply)
        ~apply_exn_continuation:(Apply.exn_continuation apply)
        (Apply.dbg apply)
        (Apply.inline apply)
    in
    match inlined with
    | None ->
      simplify_function_call_where_callee's_type_unavailable env r apply call
    | Some (env, inlined) -> simplify_expr env r inlined

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
              let param = Parameter.wrap var in
              Kinded_parameter.create (Parameter.wrap var) kind)
            result_arity
        in
        let full_app_args, remaining_args =
          Misc.Stdlib.List.split_at arity args
        in
        let func_var = Variable.create "full_apply" in
        let func_var_kind = K.value in
        let func_param = KP.create (Parameter.wrap func_var) func_var_kind in
        let perform_over_application =
          Apply.create ~callee:(Name.var func_var)
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

  and simplify_inlinable_direct_function_call env r apply call
        ~callee's_closure_id function_decl =
    let callee = Simple.name (Apply.callee apply) in
    let call_kind = Apply.call_kind apply in
    let arity_of_application = Call_kind.return_arity call_kind in
    let result_arity = Function_declaration.result_arity function_decl in
    let arity_mismatch =
      not (Flambda_arity.equal arity_of_application result_arity)
    in
    if arity_mismatch then begin
      Misc.fatal_errorf "Wrong arity for application (expected %a):@ %a"
        Flambda_arity.print result_arity
        Apply.print apply
    end;
    let args = Apply.args apply in
    (* CR mshinwell: We should check that the [param_arity] inside
       the call kind is compatible with the kinds of [args]. *)
    let provided_num_args = List.length args in
    let num_params = Function_declaration.num_params function_decl in
    if provided_num_args = num_params then
      simplify_direct_full_application env r apply call function_decl
    else if provided_num_args > num_params then
      simplify_direct_over_application env r apply function_decl
    else if provided_num_args > 0 && provided_num_args < num_params then
      simplify_direct_partial_application env r apply
        ~callee's_closure_id function_decl
    else
      Misc.fatal_errorf "Function with %d params when simplifying \
          application expression with %d arguments: %a"
        num_params
        provided_num_args
        Apply.print apply

  and simplify_function_call_where_callee's_type_unavailable env r apply
        (call : Call_kind.Function_call.t) =
    let call_kind =
      match call with
      | Indirect_unknown_arity ->
        Call_kind.indirect_function_call_unknown_arity ()
      | Indirect_known_arity { param_arity; return_arity; } ->
        Call_kind.indirect_function_call_known_arity ~param_arity ~return_arity
      | Direct { return_arity; _ } ->
        let param_arity =
          (* Some types have regressed in precision.  Since this used to be a
             direct call, we know how many arguments the function takes. *)
          List.map (fun arg ->
              let _arg, ty = S.simplify_simple env arg in
              T.kind ty)
            (Apply.args apply)
        in
        Call_kind.indirect_function_call_known_arity ~param_arity ~return_arity
    in
    Expr.create_apply (Apply.with_call_kind apply call_kind), r

  and simplify_function_call env r apply ~callee_ty
        (call : Call_kind.Function_call.t) : Expr.t * R.t =
    let type_unavailable () =
      simplify_function_call_where_callee's_type_unavailable env r apply call
    in
    match T.prove_closures (E.typing_env env) callee_ty with
    | Proved closures ->
      begin match Closure_id.Map.get_singleton closures with
      | Some (callee's_closure_id, { set_of_closures = set_ty; }) ->
        let set_ty = T.of_ty_fabricated set_ty in
        let proof = T.prove_sets_of_closures (E.typing_env env) set_ty in
        begin match proof with
        | Proved (_set_of_closures_name, set_of_closures) ->
          let closures = T.extensibility_contents set_of_closures.closures in
          begin match Closure_id.Map.find callee's_closure_id closures with
          | exception Not_found -> Expr.invalid (), r
          | closure_ty ->
            let closure_ty = T.of_ty_fabricated closure_ty in
            match T.prove_closure (E.typing_env env) closure_ty with
            | Proved { function_decls = Inlinable function_decl; } ->
              let function_decl =
                T.term_language_function_declaration function_decl
              in
              simplify_inlinable_direct_function_call env r apply call
                ~callee's_closure_id function_decl
            | Proved { function_decls = Non_inlinable None; } ->
              type_unavailable ()
            | Proved { function_decls =
                Non_inlinable (Some function_decls); } ->
              type_unavailable ()
            | Unknown -> type_unavailable ()
            | Invalid -> Expr.invalid (), r
          end
        | Unknown -> type_unavailable ()
        | Invalid -> Expr.invalid (), r
        end
      | None -> type_unavailable ()
      end
    | Unknown -> type_unavailable ()
    | Invalid -> Expr.invalid (), r

  and simplify_apply_shared env r apply =
    let callee, callee_ty = S.simplify_name env (Apply.callee apply) in
    let args = S.simplify_simples_and_drop_types env (Apply.args apply) in
    callee_ty, Apply.with_callee_and_args apply ~callee ~args, r

  and simplify_method_call env r apply ~callee_ty ~kind ~obj =
    let callee_kind = T.kind callee_ty in
    if not (K.is_value callee_kind) then begin
      Misc.fatal_errorf "Method call with callee of wrong kind %a: %a"
        K.print callee_kind
        T.print callee_ty
    end;
    Flambda.create_apply (Apply.method_call kind ~obj), r

  and simplify_c_call env r apply ~callee_ty =
    let callee_kind = T.kind callee_ty in
    if not (K.is_value callee_kind) then begin
      Misc.fatal_errorf "C call with callee of wrong kind %a: %a"
        K.print callee_kind
        T.print callee_ty
    end;
    Flambda.create_apply apply, r

  and simplify_apply env r apply : Expr.t * R.t =
    let callee_ty, apply, r = simplify_apply_shared env r apply in
    match Apply.call_kind apply with
    | Function call -> simplify_function_call env r apply ~callee_ty call
    | Method { kind; obj; } ->
      simplify_method_call env r apply ~callee_ty ~kind ~obj
    | C_call { alloc = _; param_arity = _; return_arity = _; } ->
      simplify_c_call env r apply ~callee_ty

  and simplify_apply_cont env r apply_cont : Expr.t * R.t =
    let module AC = Apply_cont in
    let cont = AC.continuation apply_cont in
    let args = S.simplify_simples_and_drop_types env (AC.args apply_cont) in
    match E.find_continuation env cont with
    | Unknown -> Apply_cont (AC.update_args apply_cont ~args), r
    | Unreachable -> Expr.invalid ()
    | Inline handler ->
      Flambda.Continuation_params_and_handler.pattern_match
        (Flambda.Continuation_handler.params_and_handler handler)
        ~f:(fun params ~handler ->
          if List.compare_lengths params args <> 0 then begin
            Misc.fatal_errorf "Wrong arity for [Apply_cont]: %a"
              AC.print apply_cont
          end;
          let expr =
            Expr.bind_parameters_to_simples ~bind:params ~target:args handler
          in
          try simplify_expr env r expr
          with Misc.Fatal_error -> begin
            Format.eprintf "\n%sContext is: inlining [Apply_cont] %a.@ \
                The inlined body was:@ %a@ in environment:@ %a\n"
              (Misc_color.bold_red ())
              (Misc_color.reset ())
              AC.print apply_cont
              Expr.print body
              E.print env;
            raise Misc.Fatal_error
          end)

  and simplify_switch env r (switch : Flambda.Switch.t) : Expr.t * R.t =
    let reachable_arms =
      Discriminant.Map.filter (fun _arm (_env, cont) ->
          match E.find_continuation env cont with
          | Unreachable -> false
          | Unknown | Inline _ -> true)
        arms
    in
    Flambda.Switch.update_arms switch ~arms:reachable_arms

  and simplify_expr env r (expr : Expr.t) : Expr.t * R.t =
    match tree with
    | Let let_expr -> simplify_let env r let_expr
    | Let_cont let_cont -> simplify_let_cont env r let_cont
    | Apply apply -> simplify_apply env r apply
    | Apply_cont apply_cont -> simplify_apply_cont env r apply_cont
    | Switch switch -> simplify_switch env r switch
    | Invalid _ -> tree, r
end
