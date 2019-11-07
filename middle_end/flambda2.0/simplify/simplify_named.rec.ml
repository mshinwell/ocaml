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

let compute_closure_element_types_inside_function ~env_outside_function
      ~env_inside_function ~closure_element_types =
  Var_within_closure.Map.fold
    (fun clos_var type_outside_function
         (env_inside_function, types_inside_function) ->
      let var = Variable.create "clos_var" in
      let env_inside_function =
        let var = Var_in_binding_pos.create var NM.in_types in
        TE.add_definition env_inside_function
          (Name_in_binding_pos.var var)
          K.value
      in
      let env_extension =
        T.make_suitable_for_environment type_outside_function
          env_outside_function
          ~suitable_for:env_inside_function
          ~bind_to:var
      in
      let env_inside_function =
        TE.add_env_extension env_inside_function ~env_extension
      in
      let types_inside_function =
        Var_within_closure.Map.add clos_var
          (T.alias_type_of K.value (Simple.var var))
          types_inside_function
      in
      env_inside_function, types_inside_function)
    closure_element_types
    (env_inside_function, Var_within_closure.Map.empty)

let function_decl_type ~denv_outside_function:denv function_decl rec_info =
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

let compute_closure_types_inside_function ~denv_outside_function
     ~all_function_decls_in_set ~closure_bound_names
     ~closure_element_types_inside_function =
  let closure_bound_names_inside =
    Closure_id.Map.map Name_in_binding_pos.rename closure_bound_names
  in
  let closure_types_via_aliases =
    Closure_id.Map.map (fun name ->
        T.alias_type_of K.value (Name_in_binding_pos.to_simple name))
      closure_bound_names_inside
  in
  let all_function_decls_in_set =
    (* CR mshinwell: Is this [Rec_info] correct for functions that
       aren't the one being simplified (i.e. others in the same
       mutually-recursive set)? *)
    Closure_id.Map.map (fun function_decl ->
        function_decl_type ~denv_outside_function function_decl
          (Rec_info.create ~depth:1 ~unroll_to:None))
      all_function_decls_in_set
  in
  let closure_types_inside_function =
    Closure_id.Map.mapi (fun closure_id _function_decl ->
        T.exactly_this_closure closure_id
          ~all_function_decls_in_set
          ~all_closures_in_set:closure_types_via_aliases
          ~all_closure_vars_in_set:closure_element_types_inside_function)
      all_function_decls_in_set
  in
  closure_bound_names_inside, closure_types_inside_function

let bind_closure_types_inside_function ~denv_outside_function
      ~denv_inside_function:denv ~all_function_decls_in_set ~closure_bound_names
      ~closure_element_types_inside_function =
  let closure_bound_names_inside, closure_types_inside_function =
    compute_closure_types_inside_function ~denv_outside_function
      ~all_function_decls_in_set ~closure_bound_names
      ~closure_element_types_inside_function
  in
  let denv =
    Closure_id.Map.fold (fun _closure_id bound_name denv ->
        let name = Name_in_binding_pos.name bound_name in
        let irrelevant = not (Name_in_binding_pos.is_symbol bound_name) in
        let bound_name =
          Name_in_binding_pos.create name
            (if irrelevant then NM.in_types else NM.normal)
        in
        DE.define_name denv bound_name K.value)
      closure_bound_names_inside
      denv
  in
  let denv =
    Closure_id.Map.fold (fun closure_id closure_type denv ->
        match Closure_id.Map.find closure_id closure_bound_names_inside with
        | exception Not_found ->
          Misc.fatal_errorf "No bound variable for closure ID %a"
            Closure_id.print closure_id
        | bound_name ->
          DE.add_equation_on_name denv
            (Name_in_binding_pos.name bound_name)
            closure_type)
      closure_types_inside_function
      denv
  in
  closure_bound_names_inside, denv

let denv_inside_function ~denv_outside_function ~denv_after_enter_closure
      ~params ~my_closure closure_id ~all_function_decls_in_set
      ~closure_bound_names ~closure_element_types =
  let env_outside_function = DE.typing_env denv_outside_function in
  let denv =
    DE.increment_continuation_scope_level_twice denv_after_enter_closure
  in
  let env_inside_function, closure_element_types_inside_function =
    compute_closure_element_types_inside_function ~env_outside_function
      ~env_inside_function:(DE.typing_env denv) ~closure_element_types
  in
  let denv = DE.with_typing_env denv env_inside_function in
  let closure_bound_names_inside, denv =
    bind_closure_types_inside_function ~denv_outside_function
      ~denv_inside_function:denv ~all_function_decls_in_set ~closure_bound_names
      ~closure_element_types_inside_function
  in
  let denv = DE.add_parameters_with_unknown_types denv params in
  match Closure_id.Map.find closure_id closure_bound_names_inside with
  | exception Not_found ->
    Misc.fatal_errorf "No closure name for closure ID %a"
      Closure_id.print closure_id
  | name ->
    let name = Name_in_binding_pos.name name in
    DE.add_variable denv
      (Var_in_binding_pos.create my_closure NM.normal)
      (T.alias_type_of K.value (Simple.name name))

let simplify_function dacc closure_id function_decl ~all_function_decls_in_set
      ~closure_bound_names ~closure_element_types =
  let name = Format.asprintf "%a" Closure_id.print closure_id in
  Profile.record_call ~accumulate:true name (fun () ->
    let denv_after_enter_closure = DE.enter_closure (DA.denv dacc) in
    let params_and_body, r =
      Function_params_and_body.pattern_match (FD.params_and_body function_decl)
        ~f:(fun ~return_continuation exn_continuation params ~body
                ~my_closure ->
          let dacc =
            DA.map_denv dacc ~f:(fun denv_outside_function ->
              denv_inside_function ~denv_outside_function
                ~denv_after_enter_closure ~params ~my_closure
                closure_id ~all_function_decls_in_set ~closure_bound_names
                ~closure_element_types)
          in
          match
            Simplify_toplevel.simplify_toplevel dacc body
              ~return_continuation
              ~return_arity:(FD.result_arity function_decl)
              exn_continuation
              ~return_cont_scope:Scope.initial
              ~exn_cont_scope:(Scope.next Scope.initial)
          with
          | body, _cont_uses, r ->
            (* CR mshinwell: Should probably look at [cont_uses]? *)
            let function_decl =
              Function_params_and_body.create ~return_continuation
                exn_continuation params ~body ~my_closure
            in
            function_decl, r
          | exception Misc.Fatal_error ->
            if !Clflags.flambda2_context_on_error then begin
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
                DA.print dacc
            end;
            raise Misc.Fatal_error)
    in
    let function_decl =
      FD.update_params_and_body function_decl params_and_body
    in
    let ty =
      function_decl_type ~denv_outside_function:(DA.denv dacc) function_decl
        Rec_info.initial
    in
    function_decl, ty, r)

let simplify_set_of_closures0 dacc ~result_dacc set_of_closures
      ~closure_bound_names ~closure_elements ~closure_element_types =
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let all_function_decls_in_set = Function_declarations.funs function_decls in
  let all_function_decls_in_set, fun_types, r =
    Closure_id.Map.fold
      (fun closure_id function_decl
           (result_function_decls_in_set, fun_types, r) ->
        let function_decl, ty, r =
          simplify_function (DA.with_r dacc r) closure_id function_decl
            ~all_function_decls_in_set ~closure_bound_names
            ~closure_element_types
        in
        let result_function_decls_in_set =
          Closure_id.Map.add closure_id function_decl
            result_function_decls_in_set
        in
        let fun_types = Closure_id.Map.add closure_id ty fun_types in
        result_function_decls_in_set, fun_types, r)
      all_function_decls_in_set
      (Closure_id.Map.empty, Closure_id.Map.empty, DA.r dacc)
  in
  let closure_types_by_bound_name =
    let closure_types_via_aliases =
      Closure_id.Map.map (fun name ->
          T.alias_type_of K.value (Name_in_binding_pos.to_simple name))
        closure_bound_names
    in
    Closure_id.Map.fold (fun closure_id _function_decl_type closure_types ->
        match Closure_id.Map.find closure_id closure_bound_names with
        | exception Not_found ->
          Misc.fatal_errorf "No bound variable for closure ID %a"
            Closure_id.print closure_id
        | bound_name ->
          let closure_type =
            T.exactly_this_closure closure_id
              ~all_function_decls_in_set:fun_types
              ~all_closures_in_set:closure_types_via_aliases
              ~all_closure_vars_in_set:closure_element_types
          in
          Name_in_binding_pos.Map.add bound_name closure_type closure_types)
      fun_types
      Name_in_binding_pos.Map.empty
  in
  (* XXX Maybe [my_closure] is missing here... *)
  let dacc =
    DA.map_denv (DA.with_r result_dacc r) ~f:(fun denv ->
      let denv =
        Closure_id.Map.fold (fun _closure_id bound_name denv ->
            DE.define_name denv bound_name K.value)
          closure_bound_names
          denv
      in
      Name_in_binding_pos.Map.fold (fun bound_name closure_type denv ->
          DE.add_equation_on_name denv (Name_in_binding_pos.to_name bound_name)
            closure_type)
        closure_types_by_bound_name
        denv)
  in
  let set_of_closures =
    Set_of_closures.create
      (Function_declarations.create all_function_decls_in_set)
      ~closure_elements
  in
  set_of_closures, closure_types_by_bound_name, dacc

let simplify_and_lift_set_of_closures dacc ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types =
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let closure_symbols =
    Closure_id.Map.mapi (fun closure_id _func_decl ->
        let name =
          closure_id
          |> Closure_id.rename
          |> Closure_id.to_string
          |> Linkage_name.create 
        in
        Symbol.create (Compilation_unit.get_current_exn ()) name)
      (Function_declarations.funs function_decls)
  in
  let _set_of_closures, dacc, static_structure_types, static_structure =
    Simplify_static.simplify_set_of_closures0 dacc ~result_dacc:dacc
      set_of_closures ~closure_symbols ~closure_elements ~closure_element_types
  in
  let r =
    let lifted_constants =  (* CR mshinwell: Add "s" to "Lifted_constant" *)
      Lifted_constant.create_from_static_structure
        (DE.typing_env (DA.denv dacc))
        static_structure_types
        static_structure
    in
    List.fold_left R.new_lifted_constant (DA.r dacc) lifted_constants
  in
  let denv =
    (* CR mshinwell: Isn't this line redundant?  The fold just above adds
       the constants. *)
    DE.add_lifted_constants (DA.denv dacc) ~lifted:(R.get_lifted_constants r)
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
      (denv, [])
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
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      DE.add_lifted_constants denv ~lifted:(R.get_lifted_constants (DA.r dacc)))
  in
  [bound_vars, defining_expr], dacc

let type_closure_elements_and_make_lifting_decision dacc ~min_name_mode
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
          match S.simplify_simple dacc simple ~min_name_mode with
          | Bottom, ty ->
            assert (K.equal (T.kind ty) K.value);
            simple, ty
          | Ok simple, ty -> simple, ty
        in
        let closure_elements =
          Var_within_closure.Map.add closure_var simple closure_elements
        in
        let closure_element_types =
          Var_within_closure.Map.add closure_var ty closure_element_types
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

let simplify_set_of_closures dacc ~(bound_vars : Bindable_let_bound.t)
      set_of_closures =
  let closure_bound_vars =
    Bindable_let_bound.must_be_set_of_closures bound_vars
  in
  (* CR mshinwell: This should probably be handled differently, but
     will require some threading through *)
  let min_name_mode =
    Bindable_let_bound.name_mode bound_vars
  in
  let can_lift, closure_elements, closure_element_types =
    type_closure_elements_and_make_lifting_decision dacc ~min_name_mode
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
    let min_name_mode = Var_in_binding_pos.name_mode bound_var in
    begin match S.simplify_simple dacc simple ~min_name_mode with
    | Bottom, ty ->
      let dacc =
        DA.map_denv dacc ~f:(fun denv ->
          DE.add_variable denv bound_var (T.bottom (T.kind ty)))
      in
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
    if !Clflags.flambda2_context_on_error then begin
      Format.eprintf "\n%sContext is:%s simplifying [Let] binding@ %a =@ %a@ \
          with downwards accumulator:@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Bindable_let_bound.print bound_vars
        Named.print named
        DA.print dacc
    end;
    raise Misc.Fatal_error
  end
