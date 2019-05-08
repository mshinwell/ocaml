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
module R = Simplify_env_and_result.Result
module T = Flambda_type

module Make (Simplify_toplevel : Simplify_toplevel_intf.S) = struct
  let simplify_function env r closure_id function_decl =
    let params_and_body =
      Function_params_and_body.pattern_match
        (Function_declaration.params_and_body function_decl)
        ~f:(fun ~continuation_param ~exn_continuation params body ~my_closure ->
          let body, r =
            E.enter_closure closure_env ~closure_id params ~my_closure
              ~dbg:(Function_declaration.dbg function_decl)
              ~f:(fun body_env ->
                assert (E.inside_set_of_closures_declaration
                  function_decls.set_of_closures_origin body_env);
                let body, r, _lifted_constants =
                  let descr =
                    Format.asprintf "body of %a" Closure_id.print closure_id
                  in
                  Simplify_toplevel.simplify_toplevel body_env r body
                    ~continuation:continuation_param
                    ~continuation_params:...
                    ~exn_continuation
                    ~descr
                in
                body, r)
          in
          Function_params_and_body.create ~continuation_param
            ~exn_continuation params body ~my_closure)
    in
    let can_inline =
      (* At present, we follow Closure, taking inlining decisions without
         first examining call sites. *)
      match Function_declaration.inline function_decl with
      | Never_inline -> false
      | Always_inline | Default_inline | Unroll _ ->
        if Function_declaration.stub function_decl then true
        else
          let inlining_threshold =
            Simplify_aux.initial_inlining_threshold ~round:(E.round env)
          in
          Inlining_cost.can_inline body inlining_threshold ~bonus:0
    in
    let function_decl =
      Function_declaration.update_params_and_body function_decl params_and_body
    in
    let function_decl_type =
      if can_inline then T.create_inlinable_function_declaration function_decl
      else T.create_non_inlinable_function_declaration ()
    in
    function_decl, function_decl_type

    let closure_env = env in
    let return_cont_params = Function_declaration.result in
    let continuation_param, closure_env =
      let continuation_param = function_decl.continuation_param in
      let cont_type =
        Continuation_approx.create_unknown ~name:continuation_param
          ~params:return_cont_params
      in
      let closure_env =
        E.add_continuation closure_env continuation_param cont_type
      in
      continuation_param, closure_env
    in
    let exn_continuation, closure_env =
      let closure_env =
        E.add_continuation closure_env exn_continuation_param cont_type
      in
      exn_continuation_param, closure_env
    in
    let closure_env =
      Simplify_aux.prepare_to_simplify_closure ~function_decl
        ~set_of_closures_env:closure_env
    in
    let my_closure = function_decl.my_closure in
    let body, r =
      E.enter_closure closure_env ~closure_id
        ~inline_inside:
          (Inlining_decision.should_inline_inside_declaration function_decl)
        ~dbg:function_decl.dbg
        ~f:(fun body_env ->
          assert (E.inside_set_of_closures_declaration
            function_decls.set_of_closures_origin body_env);
          let body, r, _lifted_constants =
            let descr =
              Format.asprintf "the body of %a" Closure_id.print closure_id
            in
            Simplify_toplevel.simplify_toplevel body_env r function_decl.body
              ~continuation:continuation_param
              ~continuation_params:return_cont_params
              ~exn_continuation
              ~descr
          in
          body, r)
    in




  let simplify_set_of_closures env r set_of_closures
        : Flambda.Set_of_closures.t * T.t * R.t =
    let env = E.entering_set_of_closures env in
    let function_decls = Set_of_closures.function_decls set_of_closures in
    let funs = Function_declarations.funs function_decls in
    let funs, fun_types, r =
      Closure_id.Map.fold (fun closure_id function_decl (funs, fun_types, r) ->
          let function_decl, ty, r =
            simplify_function env r closure_id function_decl
          in
          let funs = Closure_id.Map.add closure_id function_decl in
          let fun_types = Closure_id.Map.add closure_id fun_types in
          funs, fun_types, r)
        funs
        (Closure_id.Map.empty, r)
    in
    let function_decls = Function_declarations.create funs in
    let closure_elements, closure_element_types, r =
      Var_within_closure.Map.fold
        (fun var_within_closure simple
             (closure_elements, closure_element_types, r) ->
          let simple, ty, r = Simplify_simple.simplify env r simple in
          let closure_elements =
            Var_within_closure.Map.add var_within_closure simple
              closure_elements
          in
          let closure_element_types =
            Var_within_closure.Map.add var_within_closure ty
              closure_element_types
          in
          closure_elements, closure_element_types r)
        (Set_of_closures.closure_elements set_of_closures)
        (Var_within_closure.Map.empty, Var_within_closure.Map.empty, r)
    in
    let set_of_closures =
      Set_of_closures.create ~function_decls ~closure_elements
    in
    let closure_types =
      Closure_id.Map.mapi (fun closure_id function_decl_type ->
          T.closure closure_id function_decl_type closure_element_types
            ~set_of_closures:???)
        fun_types
    in
    let set_of_closures_type = T.set_of_closures ~closures:closure_types in
    Named.set_of_closures set_of_closures, set_of_closures_type, r

  let try_to_reify env r ty ~(term : Reachable.t) ~result_var
        ~remove_term ~cannot_lift =
    match term with
    | Invalid _ -> 
      let ty = T.bottom_like ty in
      [], term, ty, remove_term r
    | Reachable _ ->
      match T.reify env ty ~allow_free_variables:true with
      | Term (simple, ty) ->
        let term : Named.t = Simple simple in
        [], Reachable.reachable term, ty, remove_term r
      | Lift static_part ->
        if cannot_lift then [], term, ty, r
        else
          let symbol, r =
            let name = Variable.unique_name result_var in
            R.new_lifted_constant r ~name ty static_part
          in
          let ty = T.alias_type_of (T.kind ty) (Name.symbol symbol) in
          let term = Named.simple (Simple.name name) in
          [], Reachable.reachable term, ty, r
      | Cannot_reify -> [], term, ty, r
      | Invalid ->
        let ty = T.bottom_like ty in
        [], Reachable.invalid (), ty, remove_term r

  let simplify_named env r (tree : Named.t) ~result_var =
    (* CR mshinwell: Think about how the lifted constants collected in [r]
       are propagated. *)
    let typing_env = E.get_typing_environment env in
    match tree with
    | Simple simple ->
      let simple, ty, r =
        Simplify_simple.simplify_simple_for_let env r simple
      in
      [], Reachable.reachable (Named.simple simple), ty, r
    | Prim (prim, dbg) ->
      let term, ty, r =
        Simplify_primitive.simplify_primitive env r prim dbg ~result_var
      in
      let remove_primitive r =
        R.map_benefit r (B.remove_primitive_application prim)
      in
      let effects_and_coeffects_ok =
        Flambda_primitive.With_fixed_value.eligible prim
      in
      try_to_reify typing_env r ty ~term ~result_var
        ~remove_term:remove_primitive
        ~cannot_lift:(not effects_and_coeffects_ok)
    | Set_of_closures set_of_closures ->
      let term, ty, r = simplify_set_of_closures env r set_of_closures in
      try_to_reify typing_env r ty ~term:(Reachable.reachable term) ~result_var
        ~remove_term:(fun r -> r)
        ~cannot_lift:false
end
