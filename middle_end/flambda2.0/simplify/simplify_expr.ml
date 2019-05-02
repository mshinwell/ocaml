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

module B = Inlining_cost.Benefit
module E = Simplify_env_and_result.Env
module K = Flambda_kind
module KP = Kinded_parameter
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type

(* CR mshinwell: Need to simplify each [dbg] we come across. *)

module Make (Simplify_named : Simplify_named_intf.S) = struct
  (* XXX Need function to resolve continuation aliases
     Should return Unknown _ | Unreachable | Inline _ *)

  (* CR mshinwell: bad interface *)
  let record_lifted_constants env r ~f =
    let already_lifted_constants = R.get_lifted_constants r in
    let result, r = f env r in
    let lifted_constants =
      Symbol.Map.diff (R.get_lifted_constants r) already_lifted_constants
    in
    let env =
      Symbol.Map.fold (fun symbol (ty, _kind, _static_part) env ->
          E.add_symbol_for_lifted_constant env symbol ty)
        lifted_constants
        env
    in
    env, result, r

  let rec simplify_let_cont_handler ~env ~r ~cont:_
        ~(handler : Flambda.Continuation_handler.t) ~arg_tys =
    let new_handler, r = simplify_expr (E.inside_branch env) r handler.handler in
    let params =
      List.map2 (fun param arg_ty ->
          Flambda.Typed_parameter.with_type param arg_ty)
        handler.params arg_tys
    in
    let handler : Flambda.Continuation_handler.t =
      { params;
        stub = handler.stub;
        is_exn_handler = handler.is_exn_handler;
        handler = new_handler;
      }
    in
    r, handler

  (* CR mshinwell: We should not simplify recursive continuations with no
     entry point -- could loop forever. *)

  and simplify_let_cont_handlers0 env r ~handlers
        ~(recursive : Flambda.recursive) ~freshening
        : Flambda.Let_cont_handlers.t option * R.t =
    Continuation.Map.iter (fun cont _handler ->
        let cont = Freshening.apply_continuation freshening cont in
        if R.continuation_defined r cont then begin
          Misc.fatal_errorf "Ready to simplify continuation handlers \
              defining (at least) %a but such continuation(s) is/are already \
              defined in [r]"
            Continuation.print cont
        end)
      handlers;
    (* If none of the handlers are used in the body, delete them all. *)
    let all_unused =
      Continuation.Map.for_all (fun cont _handler ->
          let cont = Freshening.apply_continuation freshening cont in
          R.continuation_unused r cont)
        handlers
    in
    if all_unused then begin
      (* We don't need to touch [r] since we haven't simplified any of
         the handlers. *)
      None, r
    end else
      let handlers =
        Continuation.Map.fold (fun cont
                  (handler : Flambda.Continuation_handler.t) handlers ->
            let cont' = Freshening.apply_continuation freshening cont in
            let env =
              environment_for_let_cont_handler ~env cont ~handler
            in
            Format.eprintf "simplify_let_cont_handler\n%!";
            Format.eprintf "Environment for %a:@ %a@ \nParams:@ %a\n%!"
              Continuation.print cont
              T.Typing_env.print (E.get_typing_environment env)
              (Format.pp_print_list ~pp_sep:Format.pp_print_space
                Flambda.Typed_parameter.print) handler.params;
            let arg_tys, new_env =
              (* CR mshinwell: I have a suspicion that [r] may not contain the
                 usage information for the continuation when it's come from
                 [Unbox_continuation_params]. Check. *)
              try
                R.continuation_args_types r cont
                  ~arity:(Flambda.Continuation_handler.param_arity handler)
                  ~freshening
                  ~default_env:(E.get_typing_environment env)
              with Misc.Fatal_error -> begin
                let uses = R.continuation_uses_for r cont in
                Format.eprintf "\n%sContext is: computing join of argument \
                    types for %a, its uses are:%s@ %a\n"
                  (Misc_color.bold_red ())
                  Continuation.print cont
                  (Misc_color.reset ())
                  Continuation_uses.print uses;
                raise Misc.Fatal_error
              end
            in
            let env = E.replace_typing_environment env new_env in
            let r, handler =
              let r = R.create ~resolver:(E.resolver env) in
              simplify_let_cont_handler ~env ~r ~cont:cont' ~handler ~arg_tys
            in
            Continuation.Map.add cont' (handler, env, r) handlers)
          handlers
          Continuation.Map.empty
      in
      let continuation_unused cont =
        (* For a continuation being bound in the group to be unused, it must be
           unused within *all of the handlers* and the body. *)
        let unused_within_all_handlers =
          Continuation.Map.for_all (fun _cont (_handler, _env, r_from_handler) ->
              not (R.is_used_continuation r_from_handler cont))
            handlers
        in
        unused_within_all_handlers
          && not (R.is_used_continuation r cont)
      in
      (* Collect uses of the continuations and delete any unused ones.
         The usage information will subsequently be used by the continuation
         inlining and specialisation transformations. *)
      let r =
        Continuation.Map.fold (fun cont
                ((_handler : Flambda.Continuation_handler.t), env,
                 r_from_handler) r ->
            if continuation_unused cont then r
            else R.union (E.get_typing_environment env) r r_from_handler)
          handlers
          r
      in
      let r, handlers =
        Continuation.Map.fold (fun cont
                ((handler : Flambda.Continuation_handler.t), env, _r_from_handler)
                (r, handlers) ->
            let r, uses =
              R.exit_scope_of_let_cont r env cont ~params:handler.params
            in
            if continuation_unused cont then
              r, handlers
            else
              let handlers =
                Continuation.Map.add cont (handler, env, uses) handlers
              in
              r, handlers)
          handlers
          (r, Continuation.Map.empty)
      in
      Continuation.Map.iter (fun cont _handler ->
          assert (R.continuation_unused r cont))
        handlers;
      if Continuation.Map.is_empty handlers then begin
        None, r
      end else
        let r, handlers =
          Continuation.Map.fold (fun cont
                  ((handler : Flambda.Continuation_handler.t), env, uses)
                  (r, handlers') ->
              let ty =
                let handlers : Continuation_approx.continuation_handlers =
                  match recursive with
                  | Non_recursive ->
                    begin match Continuation.Map.bindings handlers with
                    | [_cont, (handler, _, _)] -> Non_recursive handler
                    | _ ->
                      Misc.fatal_errorf "Non_recursive Let_cont may only have one \
                          handler, but binds %a"
                        Continuation.Set.print (Continuation.Map.keys handlers)
                    end
                  | Recursive ->
                    let handlers =
                      Continuation.Map.map (fun (handler, _env, _uses) -> handler)
                        handlers
                    in
                    Recursive handlers
                in
                Continuation_approx.create ~name:cont ~handlers
                  ~params:handler.params
              in
              let r =
                R.define_continuation r cont env recursive uses ty
              in
              let handlers' = Continuation.Map.add cont handler handlers' in
              r, handlers')
            handlers
            (r, Continuation.Map.empty)
        in
        match recursive with
        | Non_recursive ->
          begin match Continuation.Map.bindings handlers with
          | [name, handler] ->
            Some (Flambda.Let_cont_handlers.Non_recursive { name; handler; }), r
          | _ -> assert false
          end
        | Recursive ->
          let is_non_recursive =
            if Continuation.Map.cardinal handlers > 1 then None
            else
              match Continuation.Map.bindings handlers with
              | [name, (handler : Flambda.Continuation_handler.t)] ->
                let fcs = Flambda.Expr.free_continuations handler.handler in
                if not (Continuation.Set.mem name fcs) then
                  Some (name, handler)
                else
                  None
              | _ -> None
          in
          match is_non_recursive with
          | Some (name, handler) ->
            Some (Flambda.Let_cont_handlers.Non_recursive { name; handler; }), r
          | None -> Some (Flambda.Let_cont_handlers.Recursive handlers), r

  and simplify_let_cont_handlers env r ~handlers ~recursive ~freshening =
    try simplify_let_cont_handlers0 env r ~handlers ~recursive ~freshening
    with Misc.Fatal_error -> begin
      Format.eprintf "\n%sContext is: simplify_let_cont_handlers:%s@ %a\n"
        (Misc_color.bold_red ())
        (Misc_color.reset ())
        (Continuation.Map.print Flambda.Continuation_handler.print) handlers;
      raise Misc.Fatal_error
    end

    (* In two stages we form the environment to be used for simplifying the
       [body].  If the continuations in [handlers] are recursive then
       that environment will also be used for simplifying the continuations
       themselves (otherwise the environment of the [Let_cont] is used). *)
    let conts_and_types, freshening =
      let normal_case ~handlers =
        Continuation.Map.fold (fun name
                (handler : Flambda.Continuation_handler.t)
                (conts_and_types, freshening) ->
            let freshened_name, freshening =
              Freshening.add_continuation freshening name
            in
            let conts_and_types =
              Continuation.Map.add freshened_name (name, ty) conts_and_types
            in
            conts_and_types, freshening)
          handlers
          (Continuation.Map.empty, E.freshening env)
      in
      let handlers = Flambda.Let_cont_handlers.to_continuation_map handlers in
      normal_case ~handlers
    in
    (* CR mshinwell: Is _unfreshened_name redundant? *)
    let body_env =
      let env = E.set_freshening env freshening in
      let env =
        Continuation.Map.fold (fun name (_unfreshened_name, cont_approx) env ->
  Format.eprintf "ADDING %a at level %a\n%!"
    Continuation.print name
    Scope_level.print (E.continuation_scope_level env);
            E.add_continuation env name cont_approx)
          conts_and_types
          env
      in
      E.increment_continuation_scope_level env
    in
    begin match handlers with
    | Recursive handlers ->
      (* The sequence is:
         1. Simplify the recursive handlers with their parameter types as
            pre-existing in the term.
         2. If all of the handlers are unused, there's nothing more to do.
         3. Extract the (hopefully more precise) Flambda types for the
            handlers' parameters from [r].
         4. The code from the simplification is discarded.
         5. The continuation(s) is/are unboxed as required.
         6. The continuation(s) are simplified once again using the
            Flambda types deduced in step 2.
         We could continue to a fixed point, but it doesn't seem worth the
         complication.
      *)
      let original_r = r in
      let original_handlers = handlers in
      let recursive : Flambda.recursive = Recursive in
      let handlers, r =
        simplify_let_cont_handlers env r ~handlers ~recursive ~freshening
      in
      begin match handlers with
      | None -> body, r
      | Some _handlers ->
        let new_env =
          ref (T.Typing_env.create ~resolver:(E.resolver env))
        in
        let arg_tys =
          Continuation.Map.mapi (fun cont
                    (handler : Flambda.Continuation_handler.t) ->
              let cont =
                Freshening.apply_continuation (E.freshening body_env) cont
              in
              (* N.B. If [cont]'s handler was deleted, the following function
                 will produce [Value_bottom] for the arguments, rather than
                 failing. *)
              let arg_tys, new_env' =
                R.defined_continuation_args_types r cont
                  ~arity:(Flambda.Continuation_handler.param_arity handler)
                  ~freshening:(E.freshening env)
                  ~default_env:(E.get_typing_environment env)
              in
              new_env := new_env';
  (* XXX Need to think about this
              new_env := T.Typing_env.meet !new_env new_env';
  *)
              arg_tys)
            original_handlers
        in
        let new_env = !new_env in
        let handlers = original_handlers in
        let r = original_r in
        let handlers, env, update_use_env =
            handlers, body_env, []
        in
        let handlers, r =
          simplify_let_cont_handlers env r ~handlers ~recursive ~freshening
        in
        let r =
          List.fold_left (fun r (if_present_in_env, then_add_to_env) ->
              R.update_all_continuation_use_environments r
                ~if_present_in_env ~then_add_to_env)
            r
            update_use_env
        in
        begin match handlers with
        | None -> body, r
        | Some handlers -> Let_cont { body; handlers; }, r
        end
      end
    end

  and simplify_let env r (let_expr : Flambda.Let.t) : Expr.t * R.t =
    let module L = Flambda.Let in
    (* CR mshinwell: Consider if we need to resurrect a special fold
       function for lets. *)
    Flambda.Let.pattern_match let_expr ~f:(fun ~bound_var ~body ->
      let r = R.clear_env_extension r in
      let already_lifted_constants = R.get_lifted_constants r in
      let new_bindings, defining_expr, ty, r =
        Simplify_named.simplify_named env r (L.defining_expr let_expr)
          ~result_var:bound_var
      in
      let lifted_constants =
        Symbol.Map.diff (R.get_lifted_constants r) already_lifted_constants
      in
      let env =
        Symbol.Map.fold (fun symbol (ty, _kind, _static_part) env ->
            E.add_symbol_for_lifted_constant env symbol ty)
          lifted_constants
          env
      in
      if not (K.compatible (T.kind ty) ~if_used_at:kind) then begin
        Misc.fatal_errorf "Kind error during simplification of [Let] \
            binding (old kind %a, new kind %a):@ %a"
          K.print new_kind
          K.print kind
          Flambda.Let.print let_expr
      end;
      let defining_expr : Reachable.t =
        match defining_expr with
        | Invalid _ -> defining_expr
        | Reachable _ ->
          if T.is_bottom (E.get_typing_environment env) ty then
            Reachable.invalid ()
          else
            defining_expr
      in
      match defining_expr with
      | Invalid _ ->
        let r = R.map_benefit r (B.remove_code_named defining_expr) in
        Expr.create_invalid (), r
      | Reachable defining_expr ->
        let env =
          E.extend_typing_environment (E.add_variable env bound_var ty)
            ~env_extension:new_env_extension
        in
        let expr, let_creation_result =
          Expr.create_let0 bound_var kind defining_expr
            (simplify_expr env r body)
        in
        let r =
          match let_creation_result with
          | Have_deleted defining_expr ->
            R.map_benefit r (B.remove_code_named defining_expr)
          | Nothing_deleted -> r
        in
        expr, r)

  and simplify_non_recursive_let_cont_handler env r non_rec_handler
        : Expr.t * R.t =
    let already_lifted_constants = R.get_lifted_constants r in
    let cont_handler =
      Non_recursive_let_cont_handler.handler non_rec_handler
    in
    Non_recursive_let_cont_handler.pattern_match non_rec_handler
      ~f:(fun cont ~body ->
        let body, r =
          let env =
            match Continuation_handler.behaviour cont_handler with
            | Unreachable ->
              E.add_unreachable_continuation env cont
            | Alias alias_for ->
              E.add_continuation_alias env cont ~alias_for
            | Unknown ->
              match Let_cont.should_inline_out let_cont with
              | Some non_rec_handler ->
                E.add_continuation_to_inline env cont
                  (Non_recursive_let_cont_handler.handler non_rec_handler)
              | None ->
                E.add_continuation env cont
          in
          simplify_expr env r body
        in
        (* Lifted constants need to be added to the environment now, before
           simplifying the handler, as the types of the handler's arguments
           may involve the corresponding symbols. *)
        let lifted_constants =
          Symbol.Map.diff (R.get_lifted_constants r) already_lifted_constants
        in
        let env =
          Symbol.Map.fold (fun symbol (ty, _kind, _static_part) env ->
              E.add_symbol_for_lifted_constant env symbol ty)
            lifted_constants
            env
        in
        let params_and_handler =
          Continuation_handler.params_and_handler cont_handler
        in
        Continuation_params_and_handler.pattern_match params_and_handler
          ~f:(fun params ~param_relations ~handler ->
            let env = E.add_parameters env params in
            let handler, r = simplify_expr env r handler in
            let params_and_handler =
              Continuation_params_and_handler.create params ~param_relations
                ~handler
            in
            let cont_handler =
              Continuation_handler.with_params_and_handler cont_handler
                params_and_handler
            in
            Let_cont.create_non_recursive cont cont_handler ~body, r)

  and simplify_recursive_let_cont_handlers env r handlers : Expr.t * R.t =


  and simplify_let_cont env r let_cont : Expr.t * R.t =
    match let_cont with
    | Non_recursive { handler; _ } ->
      simplify_non_recursive_let_cont_handler env r handler
    | Recursive handlers ->
      simplify_recursive_let_cont_handlers env r handlers

  and simplify_direct_full_application env r ~callee ~args
        ~(function_decl : Flambda_type.inlinable_function_declaration)
        ~continuation:apply_continuation_param
        ~exn_continuation:apply_exn_continuation
        dbg (inline : Inline_attribute.t) =
    match inline with
    | Never_inline ->
      simplify_function_call_where_callee's_type_unavailable env r call
        ~callee ~args dbg apply
    | Default_inline | Unroll _ | Always_inline ->
      let function_decl = function_decl.term_language_function_decl in
      Function_params_and_body.pattern_match
        (Function_declaration.params_and_body function_decl)
        ~f:(fun ~continuation_param exn_continuation params ~param_relations:_
              ~body ~my_closure ->
          let expr =
            Expr.link_continuations
              ~bind:continuation_param
              ~target:apply_continuation_param
              ~arity:(Function_declaration.result_arity function_decl)
              (Expr.link_continuations
                ~bind:(Exn_continuation.exn_handler exn_continuation)
                ~target:(Exn_continuation.exn_handler apply_exn_continuation)
                ~arity:(Exn_continuation.arity exn_continuation)
                (Expr.bind_parameters_to_simples ~bind:params ~target:args
                  (Expr.create_let my_closure (K.value ())
                    (Named.simple callee) body)))
          in
          let env =
            E.disable_function_inlining (E.add_inlined_debuginfo env dbg)
          in
          simplify_expr env r expr)

  and simplify_direct_partial_application env r ~callee ~args
        ~callee's_closure_id
        ~(function_decl : Flambda_type.inlinable_function_declaration)
        ~continuation:apply_continuation_param
        ~exn_continuation:apply_exn_continuation
        dbg (inline : Inline_attribute.t) (specialise : Specialise_attribute.t)
        apply =
    (* For simplicity, we disallow [@inline] attributes on partial
       applications.  The user may always write an explicit wrapper instead
       with such an attribute. *)
    (* CR-someday mshinwell: Pierre noted that we might like a function to be
       inlined when applied to its first set of arguments, e.g. for some kind
       of type class like thing. *)
    begin match (inline : Flambda.inline_attribute) with
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
    begin match (specialise : Flambda.specialise_attribute) with
    | Always_specialise | Never_specialise ->
      Location.prerr_warning (Debuginfo.to_location dbg)
        (Warnings.Inlining_impossible "[@specialised] attributes may not be \
          used on partial applications")
    | Default_specialise -> ()
    end;
    let function_decl = function_decl.term_language_function_decl in
    Function_params_and_body.pattern_match
      (Function_declaration.params_and_body function_decl)
      ~f:(fun ~continuation_param:_ _exn_continuation params ~param_relations:_
            ~body:_ ~my_closure:_ ->
        (* Since we're not inlining, the continuation parameters and body
           of the function declaration, etc., are irrelevant. *)
        let arity = List.length params in
        assert (arity > List.length args);
        let applied_args, remaining_params =
          Misc.Stdlib.List.map2_prefix (fun arg param ->
              let kind = KP.kind param in
              if not (K.equal kind (K.value ())) then begin
                Misc.fatal_errorf "Non-[value] kind in partial application: %a"
                  Apply.print apply
              end;
              param, arg)
            args params
        in
        let return_arity = Function_declaration.result_arity function_decl in
        begin match return_arity with
        | [kind] when Flambda_kind.equal kind (K.value ()) -> ()
        | _ ->
          Misc.fatal_errorf "Partially-applied function with non-[value] \
              return kind: %a"
            Apply.print apply
        end;
        let wrapper_var = Variable.create "partial" in
        let wrapper_taking_remaining_args =
          let continuation_param = Continuation.create () in
          let args = applied_args @ (List.map KP.simple remaining_params) in
          let call_kind =
            Call_kind.create_direct_function_call callee's_closure_id
              ~return_arity
          in
          let full_application =
            Apply.create ~callee
              ~continuation:continuation_param
              ~exn_continuation
              ~args
              ~call_kind
              ~dbg
              ~inline:Default_inline
              ~specialise:Default_specialise
          in
          let applied_args_with_closure_vars =
            List.map (fun applied_arg ->
                Var_within_closure.create (), applied_arg)
              applied_args
          in
          let body =
            List.fold_left (fun expr (closure_var, applied_arg) ->
                match Simple.must_be_var applied_arg with
                | None -> expr
                | Some applied_arg ->
                  Expr.create_let applied_arg (K.value ())
                    (Named.prim (Unary (Project_var closure_var)) dbg)
                    expr)
              (Expr.create_apply full_application)
              (List.rev applied_args_with_closure_vars)
          in
          let params_and_body =
            Function_params_and_body.create ~continuation_param
              ~exn_continuation
              remaining_params
              ~param_relations:T.Typing_env_extension.empty
              ~body
              ~my_closure:(Variable.create "my_closure")
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
              ~specialise:Default_specialise
              ~is_a_functor:false
          in
          let closure_id = Closure_id.create () in
          let function_decls =
            Function_declarations.create
              (Closure_id.Map.singleton closure_id function_decl)
          in
          let closure_elements =
            Var_within_closure.Map.of_alist applied_args_with_closure_vars
          in
          Set_of_closures.create ~function_decls
            ~set_of_closures_ty:(T.any_value ())
            ~closure_elements
        in
        let apply_cont =
          Apply_cont.create apply_continuation_param [Simple.var wrapper_var]
        in
        Expr.create_let wrapper_var (K.value ())
          (Named.create_set_of_closures wrapper_taking_remaining_args)
          (Expr.create_apply_cont apply_cont)

  and simplify_direct_over_application env r ~callee ~args
        ~callee's_closure_id
        ~(function_decl : Flambda_type.inlinable_function_declaration)
        ~continuation:apply_continuation_param
        ~exn_continuation:apply_exn_continuation
        dbg (inline : Inline_attribute.t) (specialise : Specialise_attribute.t)
        apply =
    let function_decl = function_decl.term_language_function_decl in
    Function_params_and_body.pattern_match
      (Function_declaration.params_and_body function_decl)
      ~f:(fun ~continuation_param:_ _exn_continuation params ~param_relations:_
            ~body:_ ~my_closure:_ ->
        let arity = List.length params in
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
        let func_var_kind = K.value () in
        let func_param = KP.create (Parameter.wrap func_var) func_var_kind in
        let perform_over_application =
          Apply.create ~callee:(Name.var func_var)
            ~continuation:apply_continuation_param
            ~exn_continuation:apply_exn_continuation
            ~args:remaining_args
            ~call_kind:(Call_kind.indirect_function_call_unknown_arity ())
            ~dbg
            ~inline
            ~specialise
        in
        in
        let after_full_application = Continuation.create () in
        let after_full_application_handler =
          let params_and_handler =
            Continuation_params_and_handler.create return_params
              ~param_relations:T.Typing_env_extension.empty
              ~handler:(Expr.create_apply perform_over_application)
          in
          Continuation_handler.create ~params_and_handler
            ~inferred_typing:T.Parameters.empty
            ~stub:false
            ~is_exn_handler:false
        in
        let expr =
          Let_cont.create_non_recursive after_full_application
            after_full_application_handler
            ~body:(Expr.create_apply
              (Apply.with_continuation apply after_full_application))
        in
        simplify env r expr)

  and simplify_inlinable_direct_function_call env r ~callee's_closure_id
        ~(function_decl : T.inlinable_function_declaration)
        ~(set_of_closures : T.set_of_closures)
        call ~callee ~args dbg apply =
    let arity_of_application = Call_kind.return_arity apply.call_kind in
    let result_arity = List.map T.kind function_decl.result in
    let arity_mismatch =
      not (Flambda_arity.equal arity_of_application result_arity)
    in
    if arity_mismatch then begin
      Misc.fatal_errorf "Application of %a (%a):@,function has return \
          arity %a but the application expression is expecting it \
          to have arity %a.  Function declaration is:@,%a"
        Name.print callee
        Simple.List.print args
        Flambda_arity.print result_arity
        Flambda_arity.print arity_of_application
        T.print_inlinable_function_declaration function_decl
    end;
    let r =
      match call with
      | Indirect_unknown_arity ->
        R.map_benefit r B.direct_call_of_indirect_unknown_arity
      | Indirect_known_arity _ ->
        (* CR mshinwell: This should check that the [param_arity] inside
           the call kind is compatible with the kinds of [args]. *)
        R.map_benefit r B.direct_call_of_indirect_known_arity
      | Direct _ -> r
    in
    let provided_num_args = List.length args in
    let num_params = List.length function_decl.params in
    if provided_num_args = num_params then
      simplify_direct_full_application env r
        ~callee ~callee's_closure_id ~function_decl ~set_of_closures
        ~args ~arg_tys ~continuation ~exn_continuation ~dbg ~inline ~specialise
    else if provided_num_args > num_params then
      simplify_direct_over_application env r ~args ~arg_tys ~continuation
        ~exn_continuation ~callee ~callee's_closure_id ~function_decl
        ~set_of_closures ~dbg ~inline ~specialise
    else if provided_num_args > 0 && provided_num_args < num_params then
      simplify_direct_partial_application env r ~callee ~callee's_closure_id
        ~function_decl ~args ~continuation ~exn_continuation ~dbg ~inline
        ~specialise
    else
      Misc.fatal_errorf "Function with %d params when simplifying \
          application expression with %d arguments: %a"
        num_params
        provided_num_args
        Apply.print apply

  and simplify_function_call_where_callee's_type_unavailable env r
        (call : Call_kind.Function_call.t) ~callee ~args dbg apply =
    let call_kind =
      match call with
      | Indirect_unknown_arity ->
        Call_kind.indirect_function_call_known_arity ()
      | Indirect_known_arity { param_arity; return_arity; } ->
        Call_kind.indirect_function_call_known_arity ~param_arity ~return_arity
      | Direct { return_arity; _ } ->
        let param_arity =
          (* Some types have regressed in precision.  Since this used to be a
             direct call, we know exactly how many arguments the function
             takes. *)
          List.map (fun arg ->
              let _arg, ty = S.simplify_simple env arg in
              T.kind ty)
            args
        in
        Call_kind.indirect_function_call_known_arity ~param_arity ~return_arity
    in
    let apply =
      Apply.create ~callee
        ~continuation:(Apply.continuation apply)
        ~exn_continuation:(Apply.exn_continuation apply)
        ~args
        ~call_kind
        ~dbg
        ~inline:(Apply.inline apply)
        ~specialise:(Apply.specialise apply)
    in
    Expr.create_apply apply, r

  and simplify_non_inlinable_direct_function_call env r
        ~(function_decls : T.non_inlinable_function_declarations)
        (call : Call_kind.Function_call.t) ~callee ~args dbg apply =
    (* CR mshinwell: Pierre to implement *)
    ignore function_decls;
    simplify_function_call_where_callee's_type_unavailable env r call
      ~callee ~args dbg apply

  and simplify_function_call env r apply (call : Call_kind.Function_call.t)
        : Expr.t * R.t =
    let callee, callee_ty = simplify_name env (Apply.callee apply) in
    let args = S.simplify_simples_and_drop_types env (Apply.args apply) in
    let type_unavailable () =
      simplify_function_call_where_callee's_type_unavailable env r call
        ~callee ~args dbg apply
    in
    match T.prove_closures (E.get_typing_environment env) callee_ty with
    | Proved closures ->
      begin match Closure_id.Map.get_singleton closures with
      | Some (callee's_closure_id, { set_of_closures = set_ty; }) ->
        let set_ty = T.of_ty_fabricated set_ty in
        let proof =
          T.prove_sets_of_closures (E.get_typing_environment env) set_ty
        in
        begin match proof with
        | Proved (_set_of_closures_name, set_of_closures) ->
          let closures = T.extensibility_contents set_of_closures.closures in
          begin match Closure_id.Map.find callee's_closure_id closures with
          | exception Not_found -> Expr.invalid (), r
          | closure_ty ->
            let closure_ty = T.of_ty_fabricated closure_ty in
            match T.prove_closure (E.get_typing_environment env) closure_ty with
            | Proved { function_decls = Inlinable function_decl; } ->
              simplify_inlinable_direct_function_call env r
                ~callee's_closure_id ~function_decl ~set_of_closures
                call ~callee ~args dbg apply
            | Proved { function_decls = Non_inlinable None; } ->
              type_unavailable ()
            | Proved { function_decls =
                Non_inlinable (Some function_decls); } ->
              simplify_non_inlinable_direct_function_call env r ~function_decls
                ~callee ~args dbg apply
            | Unknown -> type_unavailable ()
            | Invalid -> Expr.invalid (), r
          end
        | Unknown -> type_unavailable ()
        | Invalid -> Expr.invalid (), r
        end
      | None ->
      end
    | Unknown -> type_unavailable ()
    | Invalid -> Expr.invalid (), r

  and simplify_apply_shared env r apply =
    let callee, callee_ty = simplify_name env (Apply.callee apply) in
    let args = S.simplify_simples_and_drop_types env (Apply.args apply) in
    let apply =
      Apply.create ~callee
        ~continuation
        ~exn_continuation
        ~args
        ~call_kind:(Apply.call_kind apply)
        ~dbg
        ~inline:(Apply.inline apply)
        ~specialise:(Apply.specialise apply)
    in
    callee_ty, apply, r

  and simplify_method_call env r apply ~kind ~obj : Expr.t * R.t =
    let callee_ty, apply, r = simplify_apply_shared env r apply in
    let callee_kind = T.kind callee_ty in
    if not (K.is_value callee_kind) then begin
      Misc.fatal_errorf "Method call with callee of wrong kind %a: %a"
        K.print callee_kind
        T.print callee_ty
    end;
    Flambda.create_apply (Apply.method_call kind ~obj), r

  and simplify_c_call env r apply ~alloc:_ ~param_arity:_ ~return_arity:_
        : Expr.t * R.t =
    let callee_ty, apply, r = simplify_apply_shared env r apply in
    let callee_kind = T.kind callee_ty in
    if not (K.is_value callee_kind) then begin
      Misc.fatal_errorf "C call with callee of wrong kind %a: %a"
        K.print callee_kind
        T.print callee_ty
    end;
    Flambda.create_apply apply, r

  and simplify_apply env r apply : Expr.t * R.t =
    match Apply.call_kind apply with
    | Function call -> simplify_function_call env r apply call
    | Method { kind; obj; } -> simplify_method_call env r apply ~kind ~obj
    | C_call { alloc; param_arity; return_arity; } ->
      simplify_c_call env r apply ~alloc ~param_arity ~return_arity

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
        ~f:(fun params ~param_relations:_ ~handler ->
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

  let simplify_switch env r (switch : Flambda.Switch.t) : Expr.t * R.t =
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
