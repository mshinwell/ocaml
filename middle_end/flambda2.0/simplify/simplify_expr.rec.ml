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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

(* CR mshinwell: Need to simplify each [dbg] we come across. *)
(* CR mshinwell: Consider defunctionalising to remove the [k]. *)
(* CR mshinwell: May in any case be able to remove the polymorphic recursion. *)
(* CR mshinwell: See whether resolution of continuation aliases can be made
   more transparent (e.g. through [find_continuation]).  Tricky potentially in
   conjunction with the rewrites. *)

type 'a k = CUE.t -> R.t -> ('a * UA.t)

module Continuation_handler = struct
  include Continuation_handler
  let real_handler t = Some t

  module Opened = struct
    type nonrec t = {
      params : KP.t list;
      handler : Expr.t;
      original : t;
    }

    let create ~params ~handler original = { params; handler; original; }
    let params t = t.params
    let original t = t.original
  end

  let pattern_match t ~f =
    Continuation_params_and_handler.pattern_match (params_and_handler t)
      ~f:(fun params ~handler -> f (Opened.create ~params ~handler t))
end

module Simplify_let_cont = Generic_simplify_let_cont.Make (Continuation_handler)

let rec simplify_let
  : 'a. DA.t -> Let.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc let_expr k ->
  let module L = Flambda.Let in
  (* CR mshinwell: Find out if we need the special fold function for lets. *)
  L.pattern_match let_expr ~f:(fun ~bound_vars ~body ->
    let bindings, dacc =
      Simplify_named.simplify_named dacc ~bound_vars (L.defining_expr let_expr)
    in
    let body, user_data, uacc = simplify_expr dacc body k in
    Simplify_common.bind_let_bound ~bindings ~body, user_data, uacc)

and simplify_one_continuation_handler
  : 'a. DA.t
    -> extra_params_and_args:Continuation_extra_params_and_args.t
    -> Continuation.t
    -> Continuation_handler.Opened.t
    -> 'a k 
    -> Continuation_handler.t * 'a * UA.t
= fun dacc ~(extra_params_and_args : EPA.t)
      cont (cont_handler : Continuation_handler.Opened.t) k ->
  let module CH = Continuation_handler in
  let module CPH = Continuation_params_and_handler in
  (* 
Format.eprintf "handler:@.%a@."
  Expr.print cont_handler.handler;
  *)
  let params = cont_handler.params in
  (*
Format.eprintf "About to simplify handler %a, params %a, EPA %a\n%!"
  Continuation.print cont
  KP.List.print params
  EPA.print extra_params_and_args;
  *)
  let handler, user_data, uacc = simplify_expr dacc cont_handler.handler k in
  let handler, uacc =
  (*
    let () =
      Format.eprintf "For %a: simplified handler: %a\n%!"
        Continuation.print cont Expr.print handler
    in
    *)
    let free_names = Expr.free_names handler in
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
            Name_occurrences.mem_var free_names (KP.var param)
          end)
        params
    in
    let used_extra_params =
      List.filter (fun extra_param ->
          Name_occurrences.mem_var free_names (KP.var extra_param))
        extra_params_and_args.extra_params
    in
    (*
    let () =
      Format.eprintf "For %a: free names %a, \
          used_params %a, EP %a, used_extra_params %a\n%!"
        Continuation.print cont
        Name_occurrences.print free_names
        KP.List.print used_params
        KP.List.print extra_params_and_args.extra_params
        KP.List.print used_extra_params
    in
    *)
    let handler =
      let params = used_params @ used_extra_params in
      CH.with_params_and_handler (CH.Opened.original cont_handler)
        (CPH.create params ~handler)
    in
    let rewrite =
      Apply_cont_rewrite.create ~original_params:params
        ~used_params:(KP.Set.of_list used_params)
        ~extra_params:extra_params_and_args.extra_params
        ~extra_args:extra_params_and_args.extra_args
        ~used_extra_params:(KP.Set.of_list used_extra_params)
    in
    (*
Format.eprintf "Rewrite:@ %a\n%!" Apply_cont_rewrite.print rewrite;
*)
    let uacc =
      UA.map_uenv uacc ~f:(fun uenv ->
        UE.add_apply_cont_rewrite uenv cont rewrite)
    in
(*
Format.eprintf "Finished simplifying handler %a\n%!"
Continuation.print cont;
*)
    handler, uacc
  in
  handler, user_data, uacc

and simplify_non_recursive_let_cont_handler
  : 'a. DA.t -> Non_recursive_let_cont_handler.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc non_rec_handler k ->
  let cont_handler = Non_recursive_let_cont_handler.handler non_rec_handler in
  Non_recursive_let_cont_handler.pattern_match non_rec_handler
    ~f:(fun cont ~body ->
      let simplify_body : _ Simplify_let_cont.simplify_body =
        { simplify_body = simplify_expr; }
      in
      let body, handler, user_data, uacc =
        Simplify_let_cont.simplify_body_of_non_recursive_let_cont dacc
          cont cont_handler ~simplify_body ~body
          ~simplify_continuation_handler_like:simplify_one_continuation_handler
          k
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
        | [] | _ :: _ :: _ -> failwith "TODO" (* CR mshinwell: fix this *)
        | [c] -> c
        (* Only single continuation handler is supported right now *)
      in
      CH.pattern_match cont_handler ~f:(fun cont_handler ->
          let params = CH.Opened.params cont_handler in
          let arity = KP.List.arity params in
          let dacc =
            DA.map_denv dacc ~f:DE.increment_continuation_scope_level
          in
          (* let set = Continuation_handlers.domain rec_handlers in *)
          let body, (handlers, user_data), uacc =
            simplify_expr dacc body (fun cont_uses_env r ->
              let definition_denv =
                (* Probably useless as the arguments types are cleaned
                    of any information, hence can't refer to lifted
                    constants from the body *)
                DE.add_lifted_constants definition_denv
                  ~lifted:(R.get_lifted_constants r)
              in
              let arg_types =
                (* We can't know a good type from the call types *)
                List.map T.unknown arity
              in
              let (cont_uses_env, _apply_cont_rewrite_id) :
                Continuation_uses_env.t * Apply_cont_rewrite_id.t =
                (* We don't know anything, it's like it was called
                   with an arbitrary argument! *)
                CUE.record_continuation_use cont_uses_env
                  cont
                  Non_inlinable (* Maybe simpler ? *)
                  ~typing_env_at_use:(
                    (* not useful as we will have only top *)
                    DE.typing_env definition_denv
                  )
                  ~arg_types
              in
              let denv =
                (* XXX These don't have the same scope level as the
                   non-recursive case *)
                DE.add_parameters definition_denv params ~param_types:arg_types
              in
              let dacc = DA.create denv cont_uses_env r in
              let handler, user_data, uacc =
                simplify_one_continuation_handler dacc
                  ~extra_params_and_args:
                    Continuation_extra_params_and_args.empty
                  cont
                  cont_handler
                  (fun cont_uses_env r ->
                    let user_data, uacc = k cont_uses_env r in
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
  : 'a. DA.t -> Apply.t -> (Function_declaration.t * Rec_info.t) option
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
    let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
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
  : 'a. DA.t -> Apply.t -> callee's_closure_id:Closure_id.t
    -> param_arity:Flambda_arity.t -> result_arity:Flambda_arity.t
    -> recursive:Recursive.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply ~callee's_closure_id ~param_arity ~result_arity
      ~recursive k ->
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
  let wrapper_closure_id = Closure_id.wrap (Variable.create "closure") in
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
    let applied_args_with_closure_vars = (* CR mshinwell: rename *)
      List.map (fun applied_arg ->
          Var_within_closure.wrap (Variable.create "arg"), applied_arg)
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
    let function_decl =
      Function_declaration.create ~params_and_body
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
    Set_of_closures.create function_decls ~closure_elements
  in
  let apply_cont =
    Apply_cont.create (Apply.continuation apply)
      ~args:[Simple.var wrapper_var]
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
      let func_param = KP.create (Parameter.wrap func_var) K.value in
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
  : 'a. DA.t -> Apply.t -> callee's_closure_id:Closure_id.t
    -> param_arity:Flambda_arity.t -> result_arity:Flambda_arity.t
    -> recursive:Recursive.t -> arg_types:T.t list
    -> (Function_declaration.t * Rec_info.t) option
    -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply ~callee's_closure_id ~param_arity ~result_arity
      ~recursive ~arg_types:_ function_decl_opt k ->
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
  let args = Apply.args apply in
  let provided_num_args = List.length args in
  let num_params = List.length param_arity in
  if provided_num_args = num_params then
    simplify_direct_full_application dacc apply function_decl_opt
      ~result_arity k
  else if provided_num_args > num_params then
    simplify_direct_over_application dacc apply ~param_arity ~result_arity k
  else if provided_num_args > 0 && provided_num_args < num_params then
    simplify_direct_partial_application dacc apply
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
  let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
  let apply =
    Apply.with_call_kind apply call_kind
    |> Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
  in
  let expr =
    Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
      (Call_kind.return_arity call_kind) apply
  in
  expr, user_data, uacc

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
  | Proved (callee's_closure_id, func_decl_type) ->
    (* CR mshinwell: We should check that the [set_of_closures] in the
       [closures_entry] structure in the type does indeed contain the
       closure in question. *)
    begin match func_decl_type with
    | Known (Inlinable { function_decl; rec_info; }) ->
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
      (* CR mshinwell: This should go in Typing_env (ditto logic for Rec_info
         in Simplify_simple *)
      let function_decl_rec_info =
        match Simple.rec_info (Apply.callee apply) with
        | None -> rec_info
        | Some newer -> Rec_info.merge rec_info ~newer
      in
(*
Format.eprintf "For call to %a: callee's rec info is %a, rec info from type of function is %a\n%!"
  Simple.print (Apply.callee apply)
  (Misc.Stdlib.Option.print Rec_info.print) (Simple.rec_info (Apply.callee apply))
  Rec_info.print function_decl_rec_info;
*)
      simplify_direct_function_call dacc apply
        ~callee's_closure_id ~arg_types
        ~param_arity:(Function_declaration.params_arity function_decl)
        ~result_arity:(Function_declaration.result_arity function_decl)
        ~recursive:(Function_declaration.recursive function_decl)
        (Some (function_decl, function_decl_rec_info)) k
    | Known (Non_inlinable { param_arity; result_arity; recursive; }) ->
      simplify_direct_function_call dacc apply
        ~callee's_closure_id ~arg_types
        ~param_arity ~result_arity ~recursive
        None k
    | Unknown -> type_unavailable ()
    end
  | Unknown -> type_unavailable ()
  | Invalid ->
    let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
    Expr.create_invalid (), user_data, uacc

and simplify_apply_shared dacc apply : _ Or_bottom.t =
(*
  DA.check_continuation_is_bound dacc (Apply.continuation apply);
  DA.check_exn_continuation_is_bound dacc (Apply.exn_continuation apply);
*)
  let min_name_mode = Name_mode.normal in
  match S.simplify_simple dacc (Apply.callee apply) ~min_name_mode with
  | Bottom, _ty -> Bottom
  | Ok callee, callee_ty ->
    match S.simplify_simples dacc (Apply.args apply) ~min_name_mode with
    | Bottom -> Bottom
    | Ok args_with_types ->
      let args, arg_types = List.split args_with_types in
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
      Ok (callee_ty, apply, arg_types)

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
  let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
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
  let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
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
  | Bottom ->
    let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
    Expr.create_invalid (), user_data, uacc
  | Ok (callee_ty, apply, arg_types) ->
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
    let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
    Expr.create_invalid (), user_data, uacc
  | Ok args_with_types ->
    let args, arg_types = List.split args_with_types in
(* CR mshinwell: Resurrect arity checks
    let args_arity = T.arity_of_list arg_types in
*)
    let dacc, rewrite_id =
      DA.record_continuation_use dacc
        (AC.continuation apply_cont)
        (if Option.is_none (Apply_cont.trap_action apply_cont) then Inlinable
          else Non_inlinable)
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
    let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
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
    let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
    Expr.create_invalid (), user_data, uacc
  in
  (*
  Format.eprintf "Simplifying Switch, env@ %a@ %a\n%!"
    TE.print (DE.typing_env (DA.denv dacc)) Switch_expr.print switch;
    *)
  match S.simplify_simple dacc scrutinee ~min_name_mode with
  | Bottom, _ty -> invalid ()
  | Ok scrutinee, scrutinee_ty ->
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
    let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
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
      let bindings, _dacc =
        Simplify_named.simplify_named dacc ~bound_vars named
      in
      let body = k ~tagged_scrutinee:(Simple.var bound_to) in
      Simplify_common.bind_let_bound ~bindings ~body, user_data, uacc
    in
    let body, user_data, uacc =
      match switch_is_identity with
      | Some dest ->
        create_tagged_scrutinee (fun ~tagged_scrutinee ->
          let apply_cont = Apply_cont.create dest ~args:[tagged_scrutinee] in
          Expr.create_apply_cont apply_cont)
      | None ->
        match switch_is_boolean_not with
        | Some dest ->
          create_tagged_scrutinee (fun ~tagged_scrutinee ->
            let not_scrutinee = Variable.create "not_scrutinee" in
            let apply_cont =
              Apply_cont.create dest ~args:[Simple.var not_scrutinee]
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
  | Let_cont let_cont -> simplify_let_cont dacc let_cont k
  | Apply apply -> simplify_apply dacc apply k
  | Apply_cont apply_cont -> simplify_apply_cont dacc apply_cont k
  | Switch switch -> simplify_switch dacc switch k
  | Invalid _ ->
    let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
    expr, user_data, uacc
