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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

type pre_simplification_types_of_my_closures = {
  internal_closure_types : T.t Closure_id.Map.t;
  closure_types : T.t Closure_id.Map.t;
}

let function_decl_type denv function_decl rec_info =
  let decision =
    Inlining_decision.make_decision_for_function_declaration denv function_decl
  in
  if Inlining_decision.Function_declaration_decision.can_inline decision then
    T.create_inlinable_function_declaration function_decl rec_info
  else
    T.create_non_inlinable_function_declaration
      ~param_arity:(FD.params_arity function_decl)
      ~result_arity:(FD.result_arity function_decl)
      ~recursive:(FD.recursive function_decl)

let pre_simplification_types_of_my_closures denv ~funs ~closure_bound_names
      ~closure_element_types =
  let closure_element_types =
    Var_within_closure.Map.map (fun ty_value ->
        T.erase_aliases_ty_value (DE.typing_env denv)
          ~bound_name:None ~allowed:Variable.Set.empty ty_value)
      closure_element_types
  in
  let closure_types_as_seen_from_own_body =
    Closure_id.Map.map (fun name ->
        T.alias_type_of K.value (Name_in_binding_pos.to_simple name))
      closure_bound_names
  in
  let closure_types =
    Closure_id.Map.mapi (fun closure_id function_decl ->
        let function_decl_type_as_seen_from_own_body =
          function_decl_type denv function_decl
            (Rec_info.create ~depth:1 ~unroll_to:None)
        in
        T.exactly_this_closure closure_id
          function_decl_type_as_seen_from_own_body
          ~all_closures_in_set:closure_types_as_seen_from_own_body
          ~all_closure_vars_in_set:closure_element_types)
      funs
  in
  { internal_closure_types = closure_types_as_seen_from_own_body;
    closure_types;
  }

let type_closure_elements_and_make_lifting_decision dacc ~min_occurrence_kind
      set_of_closures =
  (* By computing the types of the closure elements, attempt to show that
     the set of closures can be lifted, and hence statically allocated.
     Note that simplifying the bodies of the functions won't change the
     set-of-closures' eligibility for lifting.  That this is so follows
     from the fact that closure elements cannot be deleted without a global
     analysis, as an inlined function's body may reference them out of
     scope of the closure declaration. *)
  let closure_elements, closure_element_types =
    Var_within_closure.Map.fold
      (fun closure_var simple (closure_elements, closure_element_types) ->
        let simple, ty =
          match S.simplify_simple dacc simple ~min_occurrence_kind with
          | Bottom, ty ->
            assert (K.equal (T.kind ty) K.value);
            simple, ty
          | Ok simple, ty -> simple, ty
        in
        let closure_elements =
          Var_within_closure.Map.add closure_var simple closure_elements
        in
        let ty_value = T.force_to_kind_value ty in
        let closure_element_types =
          Var_within_closure.Map.add closure_var ty_value closure_element_types
        in
        closure_elements, closure_element_types)
      (Set_of_closures.closure_elements set_of_closures)
      (Var_within_closure.Map.empty, Var_within_closure.Map.empty)
  in
  let can_lift =
    Var_within_closure.Map.for_all (fun _ simple -> Simple.is_symbol simple)
      closure_elements
  in
  can_lift, closure_elements, closure_element_types

let simplify_function dacc closure_id_this_function function_decl
      pre_simplification_types_of_my_closures ~closure_bound_names =
  let denv = DA.denv dacc in
  let params_and_body, r =
    Function_params_and_body.pattern_match (FD.params_and_body function_decl)
      ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure ->
        let denv = DE.enter_closure denv in
        let return_cont_scope = Scope.initial in
        let exn_cont_scope = Scope.next return_cont_scope in
        assert (Scope.equal return_cont_scope
          (DE.get_continuation_scope_level denv));
        let denv = DE.increment_continuation_scope_level_twice denv in
        let denv = DE.add_parameters_with_unknown_types denv params in
        let denv =
          Closure_id.Map.fold (fun _closure_id bound_name denv ->
              DE.define_name denv bound_name K.value)
            closure_bound_names
            denv
        in
        let denv =
          Closure_id.Map.fold (fun closure_id closure_type denv ->
              if Closure_id.equal closure_id closure_id_this_function then
                denv
              else
                match Closure_id.Map.find closure_id closure_bound_names with
                | exception Not_found ->
                  Misc.fatal_errorf "No bound variable for closure ID %a"
                    Closure_id.print closure_id
                | bound_name ->
                  match Name_in_binding_pos.to_var bound_name with
                  | None -> denv
                  | Some var -> DE.add_variable denv var closure_type)
            pre_simplification_types_of_my_closures.closure_types
            denv
        in
        let denv =
          match Closure_id.Map.find closure_id_this_function
            pre_simplification_types_of_my_closures.closure_types
          with
          | exception Not_found ->
            Misc.fatal_errorf "No closure type for this function \
                (closure ID %a)"
              Closure_id.print closure_id_this_function
          | closure_type ->
            DE.add_variable denv
              (Var_in_binding_pos.create my_closure Name_occurrence_kind.normal)
              closure_type
        in
        let dacc = DA.with_denv dacc denv in
        (* CR mshinwell: Should probably look at [cont_uses]? *)
        let body, _cont_uses, r =
          try
            Simplify_toplevel.simplify_toplevel dacc body
              ~return_continuation
              ~return_arity:(FD.result_arity function_decl)
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
              Closure_id.print closure_id_this_function
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
  let function_decl = FD.update_params_and_body function_decl params_and_body in
  function_decl, function_decl_type denv function_decl Rec_info.initial, r

let simplify_set_of_closures0 dacc ~result_dacc set_of_closures
      ~closure_bound_names ~closure_elements ~closure_element_types =
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let funs = Function_declarations.funs function_decls in
  let pre_simplification_types_of_my_closures =
    pre_simplification_types_of_my_closures (DA.denv dacc)
      ~funs ~closure_bound_names ~closure_element_types
  in
  let all_closures_in_set =
    pre_simplification_types_of_my_closures.internal_closure_types
  in
  let funs, fun_types, r =
    Closure_id.Map.fold (fun closure_id function_decl (funs, fun_types, r) ->
        let function_decl, ty, r =
          simplify_function (DA.with_r dacc r) closure_id function_decl
            pre_simplification_types_of_my_closures ~closure_bound_names
        in
        let funs = Closure_id.Map.add closure_id function_decl funs in
        let fun_types = Closure_id.Map.add closure_id ty fun_types in
        funs, fun_types, r)
      funs
      (Closure_id.Map.empty, Closure_id.Map.empty, DA.r dacc)
  in
  let closure_types_by_bound_name =
    Closure_id.Map.fold (fun closure_id function_decl_type closure_types ->
        match Closure_id.Map.find closure_id closure_bound_names with
        | exception Not_found ->
          Misc.fatal_errorf "No bound variable for closure ID %a"
            Closure_id.print closure_id
        | bound_name ->
          let closure_type =
            T.exactly_this_closure closure_id function_decl_type
              ~all_closures_in_set
              ~all_closure_vars_in_set:closure_element_types
          in
          Name_in_binding_pos.Map.add bound_name closure_type closure_types)
      fun_types
      Name_in_binding_pos.Map.empty
  in
  let dacc =
    DA.map_denv (DA.with_r result_dacc r) ~f:(fun denv ->
      let denv =
        Closure_id.Map.fold (fun _closure_id bound_name denv ->
            DE.define_name denv bound_name K.value)
          closure_bound_names
          denv
      in
      Name_in_binding_pos.Map.fold (fun bound_name closure_type denv ->
          DE.add_name denv bound_name closure_type)
        closure_types_by_bound_name
        denv)
  in
  let set_of_closures =
    Set_of_closures.create (Function_declarations.create funs) ~closure_elements
  in
  set_of_closures, closure_types_by_bound_name, dacc

let simplify_and_lift_set_of_closures dacc ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types =
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let closure_symbols =
    Closure_id.Map.mapi (fun closure_id _func_decl ->
        Symbol.create (Compilation_unit.get_current_exn ())
          (Linkage_name.create (Closure_id.unique_name closure_id)))
      (Function_declarations.funs function_decls)
  in
  let _set_of_closures, dacc, static_structure_types, static_structure =
    Simplify_static.simplify_set_of_closures0 dacc ~result_dacc:dacc
      set_of_closures ~closure_symbols ~closure_elements ~closure_element_types
  in
  let r =
    let lifted_constants =  (* CR mshinwell: Add "s" to "Lifted_constant" *)
      Lifted_constant.create_from_static_structure static_structure_types
        static_structure
    in
    List.fold_left R.new_lifted_constant (DA.r dacc) lifted_constants
  in
  let denv, bindings =
    Closure_id.Map.fold (fun closure_id bound_var (denv, bindings) ->
        match Closure_id.Map.find closure_id closure_symbols with
        | exception Not_found ->
          Misc.fatal_errorf "No closure symbol for closure ID %a"
            Closure_id.print closure_id
        | closure_symbol ->
          let simple = Simple.symbol closure_symbol in
          let defining_expr = Named.create_simple simple in
          let typ = T.alias_type_of K.value simple in
          let denv = DE.add_variable denv bound_var typ in
          let bound_var = Bindable_let_bound.singleton bound_var in
          denv, (bound_var, Reachable.reachable defining_expr) :: bindings)
      closure_bound_vars
      (DA.denv dacc, [])
  in
  bindings, DA.with_denv (DA.with_r dacc r) denv

let simplify_non_lifted_set_of_closures dacc ~bound_vars ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types =
  let closure_bound_names =
    Closure_id.Map.map Name_in_binding_pos.var closure_bound_vars
  in
  let set_of_closures, _closure_types_by_bound_name, dacc =
    simplify_set_of_closures0 dacc ~result_dacc:dacc set_of_closures
      ~closure_bound_names ~closure_elements ~closure_element_types
  in
  let defining_expr =
    Reachable.reachable (Named.create_set_of_closures set_of_closures)
  in
  [bound_vars, defining_expr], dacc

let simplify_set_of_closures dacc ~(bound_vars : Bindable_let_bound.t)
      set_of_closures =
  let closure_bound_vars =
    Bindable_let_bound.must_be_set_of_closures bound_vars
  in
  (* CR mshinwell: This should probably be handled differently, but
     will require some threading through *)
  let min_occurrence_kind =
    Bindable_let_bound.name_occurrence_kind bound_vars
  in
  let can_lift, closure_elements, closure_element_types =
    type_closure_elements_and_make_lifting_decision dacc ~min_occurrence_kind
      set_of_closures
  in
  if can_lift then
    simplify_and_lift_set_of_closures dacc ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types
  else
    simplify_non_lifted_set_of_closures dacc ~bound_vars ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types

let simplify_named0 dacc ~(bound_vars : Bindable_let_bound.t)
      (named : Named.t) =
  match named with
  | Simple simple ->
    let bound_var = Bindable_let_bound.must_be_singleton bound_vars in
    let min_occurrence_kind = Var_in_binding_pos.occurrence_kind bound_var in
    begin match S.simplify_simple dacc simple ~min_occurrence_kind with
    | Bottom, _ty ->
      let defining_expr = Reachable.invalid () in
      [bound_vars, defining_expr], dacc
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
       whenever [not (P.With_fixed_value.eligible prim)] holds. *)
    let defining_expr, dacc, ty =
      Reification.try_to_reify dacc term ~bound_to:bound_var
    in
    let defining_expr =
      if T.is_bottom (DE.typing_env (DA.denv dacc)) ty then Reachable.invalid ()
      else defining_expr
    in
    [bound_vars, defining_expr], dacc
  | Set_of_closures set_of_closures ->
    simplify_set_of_closures dacc ~bound_vars set_of_closures

let simplify_named dacc ~bound_vars named =
  try simplify_named0 dacc ~bound_vars named
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sContext is:%s simplifying [Let] binding@ %a =@ %a@ \
        with downwards accumulator:@ %a\n"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Bindable_let_bound.print bound_vars
      Named.print named
      DA.print dacc;
    raise Misc.Fatal_error
  end
