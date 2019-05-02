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

module B = Inlining_cost.Benefit
module E = Simplify_env_and_result.Env
module R = Simplify_env_and_result.Result
module T = Flambda_type

module Named = Flambda.Named

module Make (Simplify_toplevel : Simplify_toplevel_intf.S) = struct
  let freshen_continuation env cont =
    Freshening.add_continuation (E.freshening env) cont

  let simplify_set_of_closures original_env r
        (set_of_closures : Flambda.Set_of_closures.t)
        : Flambda.Set_of_closures.t * T.t * R.t =
  (* CR mshinwell for pchambart: This can be removed now, right?
    let function_decls =
      let module Backend = (val (E.backend original_env) : Backend_intf.S) in
      (* CR-soon mshinwell: Does this affect
         [reference_recursive_function_directly]?
         mshinwell: This should be thought about as part of the wider issue of
         references to functions via symbols or variables. *)
      Freshening.rewrite_recursive_calls_with_symbols (E.freshening original_env)
        set_of_closures.function_decls
        ~make_closure_symbol:Backend.closure_symbol
    in
  *)
    let env = E.increase_closure_depth original_env in
    let function_decls, _set_of_closures_ty, set_of_closures_env =
      Simplify_aux.prepare_to_simplify_set_of_closures ~env
        ~set_of_closures ~function_decls:set_of_closures.function_decls
    in
    let continuation_param_uses = Continuation.Tbl.create 42 in
    let simplify_function closure_id
          (function_decl : Flambda.Function_declaration.t)
          (funs, r) =
      let closure_env = set_of_closures_env in
      let return_cont_params =
        T.Parameters.freshened_params function_decl.result (E.freshening env)
      in
      let continuation_param, closure_env =
        let continuation_param, freshening =
          freshen_continuation closure_env function_decl.continuation_param
        in
        let cont_type =
          Continuation_approx.create_unknown ~name:continuation_param
            ~params:return_cont_params
        in
  Format.eprintf "function return continuation is at level %a\n%!"
    Scope_level.print (E.continuation_scope_level closure_env);
        let closure_env =
          E.add_continuation (E.set_freshening closure_env freshening)
            continuation_param cont_type
        in
        continuation_param, closure_env
      in
      let exn_continuation_param, closure_env =
        let exn_continuation_param, freshening =
          freshen_continuation closure_env function_decl.exn_continuation_param
        in
        let cont_type =
          Continuation_approx.create_unknown ~name:exn_continuation_param
            (* XXX missing freshening? *)
            ~params:(Simplify_aux.params_for_exception_handler ())
        in
        let closure_env = E.increment_continuation_scope_level closure_env in
        let closure_env =
          E.add_continuation (E.set_freshening closure_env freshening)
            exn_continuation_param cont_type
        in
        exn_continuation_param, closure_env
      in
      let closure_env =
        Simplify_aux.prepare_to_simplify_closure ~function_decl
          ~set_of_closures_env:closure_env
      in
      let my_closure, freshening =
        Freshening.add_variable (E.freshening env) function_decl.my_closure
      in
      let closure_env = E.set_freshening closure_env freshening in
      let body, r, return_continuation_uses =
        E.enter_closure closure_env ~closure_id
          ~inline_inside:
            (Inlining_decision.should_inline_inside_declaration function_decl)
          ~dbg:function_decl.dbg
          ~f:(fun body_env ->
            assert (E.inside_set_of_closures_declaration
              function_decls.set_of_closures_origin body_env);
            let body_env = E.increment_continuation_scope_level body_env in
  Format.eprintf "function body starts at level %a\n%!"
    Scope_level.print (E.continuation_scope_level body_env);
            (* We don't need to collect the lifted constants separately; they
               will remain in [r]. *)
            let body, r, uses, _lifted_constants =
              let descr =
                Format.asprintf "the body of %a" Closure_id.print closure_id
              in
              Simplify_toplevel.simplify_toplevel body_env r function_decl.body
                ~continuation:continuation_param
                ~continuation_params:return_cont_params
                ~exn_continuation:exn_continuation_param
                ~descr
                ~scope_level_for_lifted_constants:
                  (E.scope_level_for_lifted_constants env)
            in
            Continuation.Tbl.add continuation_param_uses continuation_param uses;
            body, r, uses)
      in
      let result =
        Join_point.parameters return_continuation_uses
          ~arity:function_decl.return_arity
          (E.freshening set_of_closures_env)
          ~continuation_env_of_definition:(E.get_typing_environment closure_env)
          ~existing_continuation_params:function_decl.result
      in
      let inline : Flambda.inline_attribute =
        match function_decl.inline with
        | Default_inline ->
          if !Clflags.classic_inlining && not function_decl.stub then begin
            (* In classic-inlining mode, the inlining decision is taken at
               definition site (here).  If the function is small enough
               (below the -inline threshold) it will always be inlined. *)
            let inlining_threshold =
              Simplify_aux.initial_inlining_threshold ~round:(E.round env)
            in
            if Inlining_cost.can_inline body inlining_threshold ~bonus:0
            then Always_inline
            else Default_inline
          end else begin
            Default_inline
          end
        | inline -> inline
      in
      let function_decl =
        Flambda.Function_declaration.create ~params:function_decl.params
          ~continuation_param ~exn_continuation_param
          ~return_arity
          ~body ~stub:function_decl.stub ~dbg:function_decl.dbg
          ~inline ~specialise:function_decl.specialise
          ~is_a_functor:function_decl.is_a_functor
          ~closure_origin:function_decl.closure_origin
          ~my_closure
      in
      let ty =
        let params =
          List.map (fun param ->
              Flambda.Typed_parameter.param param,
                Flambda.Typed_parameter.ty param)
            function_decl.params
        in
        (* CR mshinwell: Put this conversion in a function somewhere *)
        let inline : T.inline_attribute =
          match function_decl.inline with
          | Always_inline -> Always_inline
          | Never_inline -> Never_inline
          | Unroll n -> Unroll n
          | Default_inline -> Default_inline
        in
        let specialise : T.specialise_attribute =
          match function_decl.specialise with
          | Always_specialise -> Always_specialise
          | Never_specialise -> Never_specialise
          | Default_specialise -> Default_specialise
        in
        T.create_inlinable_function_declaration
          ~is_classic_mode:!Clflags.classic_inlining
          ~closure_origin:function_decl.closure_origin
          ~continuation_param
          ~exn_continuation_param
          ~params
          ~body
          ~result
          ~result_env_extension
          ~stub:function_decl.stub
          ~dbg:function_decl.dbg
          ~inline
          ~specialise
          ~is_a_functor:function_decl.is_a_functor
          ~invariant_params:(lazy Variable.Set.empty)  (* CR mshinwell: fix *)
          ~size:(lazy None)  (* CR mshinwell: to fix *)
          ~direct_call_surrogate:None  (* CR mshinwell: to fix *)
          ~my_closure
      in
  (* XXX Why was this here?
      let r =
        R.add_or_meet_env_extension r result_env_extension
      in
  *)
      Closure_id.Map.add closure_id (function_decl, ty) funs, r
    in
    let funs_with_types, r =
      Closure_id.Map.fold simplify_function function_decls.funs
        (Closure_id.Map.empty, r)
    in
    let function_decls =
      let funs =
        Closure_id.Map.map (fun (func_decl, _ty) -> func_decl) funs_with_types
      in
      Flambda.Function_declarations.update function_decls ~funs
    in
    let in_closure =
      Flambda.Free_vars.map_vars set_of_closures.free_vars ~f:(fun var ->
        (* XXX This should use a variant of [simplify_named] called
           [simplify_var] which always returns [Variable.t], otherwise
           we're not following aliases via types.  (And below) *)
        Freshening.apply_variable (E.freshening env) var)
    in
    let set_of_closures =
      Flambda.Set_of_closures.create ~function_decls ~in_closure
        ~direct_call_surrogates:Closure_id.Map.empty
    in
    let closures =
      Closure_id.Map.map (fun (_func_decl, ty) -> T.closure ty) funs_with_types
    in
    let closure_elements =
      Var_within_closure.Map.map (fun (free_var : Flambda.Free_var.t) ->
          let var = Freshening.apply_variable (E.freshening env) free_var.var in
          T.alias_type_of_as_ty_value (Name.var var))
        set_of_closures.free_vars
    in
    let can_lift = Var_within_closure.Map.is_empty closure_elements in
    if not can_lift then begin
      let ty =
        T.set_of_closures ~closures:(Exactly closures)
          ~closure_elements:(Exactly closure_elements)
      in
      let term : Named.t = Set_of_closures set_of_closures in
      term, ty, r
    end else begin
      let symbol, r =
        let name = Variable.unique_name result_var in
        let static_part =
          Flambda_static0.Static_part.Set_of_closures set_of_closures
        in
        R.new_lifted_constant r ~name ty static_part
      in
      let simple = Simple.name (Name.symbol symbol) in
      let ty = T.alias_type_of (Flambda_kind.fabricated ()) simple in
      let term : Named.t = Simple simple in
      term, ty, r
    end

  let try_to_reify env r ty ~(term : Flambda.Reachable.t) ~result_var
        ~remove_term ~can_lift =
    match term with
    | Invalid _ -> 
      let ty = T.bottom_like ty in
      [], term, ty, remove_term ()
    | Reachable _ ->
      match T.reify env ty ~allow_free_variables:true with
      | Term (simple, ty) ->
        let term : Named.t = Simple simple in
        [], Flambda.Reachable.reachable term, ty, remove_term ()
      | Lift static_part ->
        if not can_lift then [], term, ty, r
        else
          let symbol, r =
            let name = Variable.unique_name result_var in
            R.new_lifted_constant r ~name ty static_part
          in
          let name = Name.symbol symbol in
          let kind = T.kind ty in
          let ty = T.alias_type_of kind name in
          let term : Named.t = Simple (Simple.name name) in
          [], Flambda.Reachable.reachable term, ty, r
      | Cannot_reify -> [], term, ty, r
      | Invalid ->
        let ty = T.bottom_like ty in
        [], Flambda.Reachable.invalid (), ty, remove_term ()

  let simplify_named env r (tree : Named.t) ~result_var =
    match tree with
    | Simple simple ->
      let simple, ty, r = Simplify_simple.simplify_simple_for_let env r simple in
      [], Flambda.Reachable.reachable (Simple simple), ty, r
    | Prim (prim, dbg) ->
      let term, ty, r =
        Simplify_primitive.simplify_primitive env r prim dbg ~result_var
      in
      let remove_primitive () =
        R.map_benefit r (B.remove_primitive_application prim)
      in
      let effects_and_coeffects_ok =
        Flambda_primitive.With_fixed_value.eligible prim
      in
      try_to_reify (E.get_typing_environment env) r ty
        ~term ~result_var ~remove_term:remove_primitive
        ~can_lift:effects_and_coeffects_ok
    | Set_of_closures set_of_closures ->
      simplify_set_of_closures env r set_of_closures
end
