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
(*   special exception on linking described in the file LICENSDE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

type pre_simplification_types_of_my_closures = {
  set_of_closures : (Name_in_binding_pos.t * Flambda_type.t) option;
  closure_types : T.t Closure_id.Map.t;
}

let simplify_function dacc closure_id function_decl
      pre_simplification_types_of_my_closures =
  (* CR mshinwell: improve efficiency by not opening abstraction 3 times *)
  let param_arity = Function_declaration.params_arity function_decl in
  let result_arity = Function_declaration.result_arity function_decl in
  let denv = DA.denv dacc in
  let params_and_body, r =
    Function_params_and_body.pattern_match
      (Function_declaration.params_and_body function_decl)
      ~f:(fun ~return_continuation exn_continuation params ~body
              ~my_closure ->
(*
Format.eprintf "Closure ID %a, entering closure\n%!"
  Closure_id.print closure_id;
*)
        let denv = DE.enter_closure denv in
        let return_cont_scope = Scope.initial in
        let exn_cont_scope = Scope.next return_cont_scope in
        assert (Scope.equal return_cont_scope
          (DE.get_continuation_scope_level denv));
(*
Format.eprintf "Closure ID %a, done entering closure\n%!"
  Closure_id.print closure_id;
*)
        let denv =
          DE.increment_continuation_scope_level
            (DE.increment_continuation_scope_level denv)
        in
        let denv = DE.add_parameters_with_unknown_types denv params in
        let denv =
          match pre_simplification_types_of_my_closures.set_of_closures with
          | None -> denv
          | Some (set_of_closures, set_of_closures_type) ->
            assert (K.equal (T.kind set_of_closures_type) K.fabricated);
            DE.define_name denv set_of_closures K.fabricated
        in
        let type_of_my_closure =
          match
            Closure_id.Map.find closure_id
              pre_simplification_types_of_my_closures.closure_types
          with
          | exception Not_found ->
            Misc.fatal_errorf "No type given for [my_closure] for closure ID %a"
              Closure_id.print closure_id
          | ty -> ty
        in
(*
Format.eprintf "Closure ID %a, adding type_of_my_closure:@ %a\n%!"
  Closure_id.print closure_id
  T.print type_of_my_closure;
*)
        let denv =
          DE.add_variable denv
            (Var_in_binding_pos.create my_closure Name_occurrence_kind.normal)
            type_of_my_closure
        in
(*
Format.eprintf "denv is now:@ %a\n" DE.print denv;
*)
        let denv =
          match pre_simplification_types_of_my_closures.set_of_closures with
          | None -> denv
          | Some (set_of_closures, set_of_closures_type) ->
            DE.add_equation_on_name denv
              (Name_in_binding_pos.name set_of_closures)
              set_of_closures_type
        in
        let dacc = DA.with_denv dacc denv in
(*
Format.eprintf "Closure ID %a env:@ %a@ function body:@ %a\n%!"
  Closure_id.print closure_id
  DE.print env
  Expr.print body;
*)
        (* CR mshinwell: Should probably look at [cont_uses]? *)
        let body, _cont_uses, r =
(*
          Format.eprintf "Environment inside function %a:\n%a\n%!"
            Closure_id.print closure_id
            T.Typing_env.print (DE.typing_env env);
*)
          try
            Simplify_toplevel.simplify_toplevel dacc body
              ~return_continuation
              ~return_arity:result_arity
              exn_continuation
              ~return_cont_scope
              ~exn_cont_scope
          with Misc.Fatal_error -> begin
            Format.eprintf "\n%sContext is:%s simplifying function \
                with closure ID %a,@ params %a,@ return continuation %a,@ \
                exn continuation %a,@ my_closure %a,@ body:@ %a@ \
                with downwards accumulator:@ %a\n"
              (Flambda_colours.error ())
              (Flambda_colours.normal ())
              Closure_id.print closure_id
              Kinded_parameter.List.print params
              Continuation.print return_continuation
              Exn_continuation.print exn_continuation
              Variable.print my_closure
              Expr.print body
              DA.print dacc;
            raise Misc.Fatal_error
          end
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
    let decision =
      Inlining_decision.make_decision_for_function_declaration denv
        function_decl
    in
    if Inlining_decision.Function_declaration_decision.can_inline decision then
      T.create_inlinable_function_declaration function_decl Rec_info.initial
    else
      T.create_non_inlinable_function_declaration
        ~param_arity ~result_arity
        ~recursive:(Function_declaration.recursive function_decl)
  in
  function_decl, function_decl_type, r

let lift_set_of_closures dacc set_of_closures ~closure_elements_and_types
      ~result_var =
  let set_of_closures_symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create (Variable.unique_name (
        Var_in_binding_pos.var result_var)))
  in
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let funs = Function_declarations.funs function_decls in
  let closure_symbols =
    Closure_id.Map.mapi (fun closure_id _func_decl ->
        Symbol.create (Compilation_unit.get_current_exn ())
          (Linkage_name.create (Closure_id.unique_name closure_id)))
      funs
  in
  let _set_of_closures, dacc, ty, static_structure_types, static_structure =
    Simplify_static.simplify_set_of_closures dacc ~result_dacc:dacc
      set_of_closures ~set_of_closures_symbol ~closure_symbols
      ~closure_elements_and_types
  in
  let r = DA.r dacc in
  let r =
    let lifted_constants =
      Lifted_constant.create_from_static_structure static_structure_types
        static_structure
    in
    List.fold_left (fun r lifted_constant ->
        R.new_lifted_constant r lifted_constant)
      r
      lifted_constants
  in
  let set_of_closures_symbol = Simple.symbol set_of_closures_symbol in
  let term = Named.create_simple set_of_closures_symbol in
  let ty = T.alias_type_of (T.kind ty) set_of_closures_symbol in
  let dacc =
    DA.map_denv dacc ~f:(fun denv -> DE.add_variable denv result_var ty)
  in
  Reachable.reachable term, DA.with_r dacc r, ty

let pre_simplification_types_of_my_closures denv ~funs ~closure_bound_vars
      ~closure_element_types =
  let closure_element_types =
    Var_within_closure.Map.map (fun ty_value ->
        T.erase_aliases_ty_value (DE.typing_env denv)
          ~bound_name:None
          ~allowed:Variable.Set.empty ty_value)
      closure_element_types
  in
  let closure_types_via_variables =
    Closure_id.Map.map (fun bound_var ->
        T.alias_type_of_as_ty_value (Var_in_binding_pos.simple bound_var))
      closure_bound_vars
  in
  let closure_types =
    Closure_id.Map.mapi (fun closure_id function_decl ->
        let function_decl_type =
          let decision =
            Inlining_decision.make_decision_for_function_declaration denv
              function_decl
          in
          if Inlining_decision.Function_declaration_decision.can_inline decision
          then
            T.create_inlinable_function_declaration function_decl
              (Rec_info.create ~depth:1 ~unroll_to:None)
          else
            let param_arity =
              Function_declaration.params_arity function_decl
            in
            let result_arity =
              Function_declaration.result_arity function_decl
            in
            T.create_non_inlinable_function_declaration
              ~param_arity ~result_arity
              ~recursive:(Function_declaration.recursive function_decl)
        in
        T.exactly_this_closure closure_id function_decl_type
          ~all_closures_in_set:closure_types_via_variables
          ~all_closure_vars_in_set:closure_element_types)
      funs
  in
  { closure_bound_vars;
    closure_types;
  }

let simplify_set_of_closures dacc ~(bound_vars : Bindable_let_bound.t)
      set_of_closures =
  let closure_bound_vars =
    Bindable_let_bound.must_be_set_of_closures bound_vars
  in
  (* By simplifying the types of the closure elements, attempt to show that
     the set of closures can be lifted, and hence statically allocated. *)
  let closure_elements, closure_element_types =
    Var_within_closure.Map.fold
      (fun var_within_closure simple
           (closure_elements, closure_element_types) ->
        (* CR mshinwell: This should probably be handled differently, but
           will require some threading through *)
        let min_occurrence_kind =
          Bindable_let_bound.name_occurrence_kind bound_vars
        in
        let simple, ty =
          match
            Simplify_simple.simplify_simple dacc simple ~min_occurrence_kind
          with
          | Bottom, ty ->
            assert (K.equal (T.kind ty) K.value);
            simple, ty
          | Ok simple, ty -> simple, ty
        in
        let closure_elements =
          Var_within_closure.Map.add var_within_closure simple
            closure_elements
        in
        let ty_value = T.force_to_kind_value ty in
        let closure_element_types =
          Var_within_closure.Map.add var_within_closure ty_value
            closure_element_types
        in
        closure_elements, closure_element_types)
      (Set_of_closures.closure_elements set_of_closures)
      (Var_within_closure.Map.empty, Var_within_closure.Map.empty)
  in
  let can_lift =
    Var_within_closure.Map.for_all (fun _ (simple : Simple.t) ->
        match Simple.descr simple with
        | Name (Symbol _) -> true
        | _ -> false)
      closure_elements
  in
  if can_lift then
    lift_set_of_closures dacc set_of_closures
      ~closure_elements_and_types:
        (Some (closure_elements, closure_element_types))
      ~result_var
  else
    let function_decls = Set_of_closures.function_decls set_of_closures in
    let funs = Function_declarations.funs function_decls in
    (* Note that simplifying the bodies of the functions won't change the
       set-of-closures' eligibility for lifting.  That this is so follows
       from the fact that closure elements cannot be deleted without a global
       analysis, as an inlined function's body may reference them out of
       scope of the closure declaration. *)
    let pre_simplification_types_of_my_closures =
      pre_simplification_types_of_my_closures (DA.denv dacc)
        ~funs ~closure_bound_vars ~closure_element_types
    in
    let funs, fun_types, r =
      Closure_id.Map.fold
        (fun closure_id function_decl (funs, fun_types, r) ->
          let dacc = DA.with_r dacc r in
          let function_decl, ty, r =
            simplify_function dacc closure_id function_decl
              pre_simplification_types_of_my_closures
          in
          let funs = Closure_id.Map.add closure_id function_decl funs in
          let fun_types = Closure_id.Map.add closure_id ty fun_types in
          funs, fun_types, r)
        funs
        (Closure_id.Map.empty, Closure_id.Map.empty, DA.r dacc)
    in
    let dacc = DA.with_r dacc r in
    let function_decls = Function_declarations.create funs in
    let set_of_closures =
      Set_of_closures.create ~function_decls ~closure_elements
    in
    let closure_types =
      Closure_id.Map.mapi (fun closure_id function_decl_type ->
          T.closure closure_id function_decl_type closure_element_types
            ~set_of_closures:set_of_closures_ty_fabricated)
        fun_types
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        DE.add_variable denv result_var set_of_closures_type)
    in
    let term = Named.create_set_of_closures set_of_closures in
    let defining_expr = Reachable.reachable term in
    [bound_vars, defining_expr], dacc

let simplify_named0 dacc ~(bound_vars : Bindable_let_bound.t)
      (named : Named.t) =
  match named with
  | Simple simple ->
    let bound_var = Bindable_let_bound.must_be_singleton bound_vars in
    let min_occurrence_kind = Var_in_binding_pos.occurrence_kind bound_var in
    begin match
      Simplify_simple.simplify_simple dacc simple ~min_occurrence_kind
    with
    | Bottom, ty ->
      let defining_expr = Reachable.invalid () in
      [bound_vars, defining_expr, ty], dacc
    | Ok simple, ty ->
      let dacc =
        DA.map_denv dacc ~f:(fun denv -> DE.add_variable denv bound_var ty)
      in
      let defining_expr = Reachable.reachable (Named.create_simple simple) in
      [bound_vars, defining_expr], dacc
    end
  | Prim (prim, dbg) ->
    let bound_var = Bindable_let_bound.must_be_singleton bound_vars in
    let term, env_extension, dacc =
      Simplify_primitive.simplify_primitive dacc prim dbg ~result_var:bound_var
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        let kind = P.result_kind' prim in
        let denv = DE.add_variable denv bound_var (T.unknown kind) in
        DE.extend_typing_environment denv env_extension)
    in
    (* CR mshinwell: Add check along the lines of: types are unknown
       whenever [not (P.With_fixed_value.eligible prim)]
       holds. *)
    let defining_expr, dacc, ty =
      Reification.try_to_reify dacc term ~bound_to:bound_var
    in
    let defining_expr =
      if T.is_bottom (DE.typing_env (DA.denv dacc)) ty then
        Reachable.invalid ()
      else
        defining_expr
    in
    [bound_vars, defining_expr], dacc
  | Set_of_closures set_of_closures ->
    simplify_set_of_closures dacc ~bound_vars set_of_closures

let simplify_named dacc ~bound_vars named =
  try simplify_named0 dacc ~bound_vars named
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sContext is:%s simplifying [Let] binding@  %a = %a@ \
        with downwards accumulator:@ %a\n"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Var_in_binding_pos.print result_var
      Named.print named
      DA.print dacc;
    raise Misc.Fatal_error
  end
