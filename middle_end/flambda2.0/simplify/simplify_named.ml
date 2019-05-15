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
  let simplify_function env r closure_id function_decls function_decl =
    let params_and_body, r =
      Function_params_and_body.pattern_match
        (Function_declaration.params_and_body function_decl)
        ~f:(fun ~return_continuation exn_continuation params ~body
                ~my_closure ->
          let env = E.enter_closure env closure_id in
          let result_arity = Function_declaration.result_arity function_decl in
          let env = E.add_continuation env return_continuation result_arity in
          let env = E.add_exn_continuation env exn_continuation in
          let env = E.add_parameters_with_unknown_types env params in
          let env = E.add_variable env my_closure (T.any_value ()) in
          assert (E.inside_set_of_closures_declaration env
            (Function_declarations.set_of_closures_origin function_decls));
          let env = E.increment_continuation_scope_level env in
          let body, r =
            Simplify_toplevel.simplify_toplevel env r body
              ~return_continuation
              exn_continuation
              ~scope_level_for_lifted_constants:Scope_level.initial (* XXX *)
          in
          let function_decl =
            Function_params_and_body.create ~return_continuation
              exn_continuation params ~body ~my_closure
          in
          function_decl, r)
    in
    let function_decl =
      Function_declaration.update_params_and_body function_decl params_and_body
    in
    let function_decl_type =
      if Inlining_decision.can_inline env function_decl then
        T.create_inlinable_function_declaration function_decl
      else
        T.create_non_inlinable_function_declaration ()
    in
    function_decl, function_decl_type, r

  let simplify_set_of_closures0 env r set_of_closures ~result_var =
    let function_decls = Set_of_closures.function_decls set_of_closures in
    (* CR mshinwell: Shouldn't [Function_declarations.set_of_closures_origin]
       be on [Set_of_closures]? *)
    let set_of_closures_origin =
      Function_declarations.set_of_closures_origin function_decls
    in
    let env = E.entering_set_of_closures env set_of_closures_origin in
    let funs = Function_declarations.funs function_decls in
    let funs, fun_types, r =
      Closure_id.Map.fold (fun closure_id function_decl (funs, fun_types, r) ->
          let function_decl, ty, r =
            simplify_function env r closure_id function_decls function_decl
          in
          let funs = Closure_id.Map.add closure_id function_decl funs in
          let fun_types = Closure_id.Map.add closure_id ty fun_types in
          funs, fun_types, r)
        funs
        (Closure_id.Map.empty, Closure_id.Map.empty, r)
    in
    let function_decls = Function_declarations.create funs in
    let closure_elements, closure_element_types, r =
      Var_within_closure.Map.fold
        (fun var_within_closure simple
             (closure_elements, closure_element_types, r) ->
          let simple, ty = Simplify_simple.simplify_simple env simple in
          let closure_elements =
            Var_within_closure.Map.add var_within_closure simple
              closure_elements
          in
          let ty_value = T.force_to_kind_value ty in
          let closure_element_types =
            Var_within_closure.Map.add var_within_closure ty_value
              closure_element_types
          in
          closure_elements, closure_element_types, r)
        (Set_of_closures.closure_elements set_of_closures)
        (Var_within_closure.Map.empty, Var_within_closure.Map.empty, r)
    in
    let set_of_closures =
      Set_of_closures.create ~function_decls ~closure_elements
    in
    (* The resulting set-of-closures and closures types are recursive. *)
    let set_of_closures_ty_fabricated =
      T.alias_type_of_as_ty_fabricated (Simple.var result_var)
    in
    let closure_types =
      Closure_id.Map.mapi (fun closure_id function_decl_type ->
          T.closure closure_id function_decl_type closure_element_types
            ~set_of_closures:set_of_closures_ty_fabricated)
        fun_types
    in
    let set_of_closures_type = T.set_of_closures ~closures:closure_types in
    set_of_closures, set_of_closures_type, r

  let simplify_set_of_closures env r set_of_closures ~result_var =
    let set_of_closures, ty, r =
      simplify_set_of_closures0 env r set_of_closures ~result_var
    in
    Named.create_set_of_closures set_of_closures, ty, r

  let simplify_set_of_closures_and_drop_type env r set_of_closures =
    let result_var = Variable.create "set_of_closures" in
    let set_of_closures, _ty, r =
      simplify_set_of_closures0 env r set_of_closures ~result_var
    in
    term, r

  let create_static_part (to_lift : T.to_lift) : Flambda_static.Static_part.t =
    match to_lift with
    | Boxed_float f -> Boxed_float (Const f)
    | Boxed_int32 i -> Boxed_int32 (Const i)
    | Boxed_int64 i -> Boxed_int64 (Const i)
    | Boxed_nativeint i -> Boxed_nativeint (Const i)

  let try_to_reify env r ty ~(term : Reachable.t) ~result_var ~remove_term
        ~cannot_lift =
    match term with
    | Invalid _ -> 
      let ty = T.bottom_like ty in
      [], term, ty, remove_term r
    | Reachable _ ->
      match T.reify env ty ~allow_free_variables:true with
      | Term (simple, ty) ->
        let term = Named.create_simple simple in
        [], Reachable.reachable term, ty, remove_term r
      | Lift to_lift ->
        if cannot_lift then [], term, ty, r
        else
          let symbol, r =
            let name = Variable.unique_name result_var in
            let static_part = create_static_part to_lift in
            R.new_lifted_constant r ~name ty static_part
          in
          let symbol = Simple.symbol symbol in
          let ty = T.alias_type_of (T.kind ty) symbol in
          let term = Named.create_simple symbol in
          [], Reachable.reachable term, ty, r
      | Cannot_reify -> [], term, ty, r
      | Invalid ->
        let ty = T.bottom_like ty in
        [], Reachable.invalid (), ty, remove_term r

  let simplify_named env r (tree : Named.t) ~result_var =
    (* CR mshinwell: Think about how the lifted constants collected in [r]
       are propagated. *)
    let typing_env = E.typing_env env in
    match tree with
    | Simple simple ->
      let simple, ty, r =
        Simplify_simple.simplify_simple_for_let env r simple
      in
      [], Reachable.reachable (Named.create_simple simple), ty, r
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
      let term, ty, r =
        simplify_set_of_closures env r set_of_closures ~result_var
      in
      try_to_reify typing_env r ty ~term:(Reachable.reachable term) ~result_var
        ~remove_term:(fun r -> r)
        ~cannot_lift:false
end
