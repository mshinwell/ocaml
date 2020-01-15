(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

module Context_for_multiple_sets_of_closures : sig
  (* This module deals with a sub-problem of the problem of simplifying multiple
     possibly-recursive sets of closures, namely determining typing and
     contextual information that is the same no matter which set of closures in
     a given recursive group is being simplified. *)

  type t

  val create
     : dacc_prior_to_sets:DA.t
    -> all_sets_of_closures:Set_of_closures.t list
    -> closure_element_types_all_sets:T.t Var_within_closure.Map.t list
    -> t

  val dacc_inside_functions : t -> DA.t

  val closure_bound_names_inside_functions_all_sets
     : t
    -> Name_in_binding_pos.t Closure_id.Map.t list
end = struct
  type t = {
    dacc_inside_functions : DA.t;
    closure_bound_names_inside_functions_all_sets
      : Name_in_binding_pos.t Closure_id.Map.t list;
  }

  let dacc_inside_functions t = t.dacc_inside_functions

  let closure_bound_names_inside_functions_all_sets t =
    t.closure_bound_names_inside_functions_all_sets

  let compute_closure_element_types_inside_function ~env_prior_to_sets
        ~env_inside_function ~closure_element_types =
    Var_within_closure.Map.fold
      (fun clos_var type_prior_to_sets
           (env_inside_function, types_inside_function) ->
        let var = Variable.create "clos_var" in
        let env_inside_function =
          let var = Var_in_binding_pos.create var NM.in_types in
          TE.add_definition env_inside_function
            (Name_in_binding_pos.var var)
            K.value
        in
        let env_extension =
          T.make_suitable_for_environment type_prior_to_sets
            env_prior_to_sets
            ~suitable_for:env_inside_function
            ~bind_to:(Name.var var)
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

  let function_decl_type ~denv_prior_to_sets function_decl ?code_id rec_info =
    let decision =
      Inlining_decision.make_decision_for_function_declaration
        denv_prior_to_sets function_decl
    in
    let code_id = Option.value code_id ~default:(FD.code_id function_decl) in
    if Inlining_decision.Function_declaration_decision.can_inline decision then
      T.create_inlinable_function_declaration
        ~code_id
        ~param_arity:(FD.params_arity function_decl)
        ~result_arity:(FD.result_arity function_decl)
        ~stub:(FD.stub function_decl)
        ~dbg:(FD.dbg function_decl)
        ~inline:(FD.inline function_decl)
        ~is_a_functor:(FD.is_a_functor function_decl)
        ~recursive:(FD.recursive function_decl)
        ~rec_info
    else
      T.create_non_inlinable_function_declaration
        ~code_id
        ~param_arity:(FD.params_arity function_decl)
        ~result_arity:(FD.result_arity function_decl)
        ~recursive:(FD.recursive function_decl)

  let compute_closure_types_inside_functions ~denv_prior_to_sets
       ~all_function_decls_all_sets ~closure_bound_names_all_sets
       ~closure_element_types_inside_functions_all_sets
       ~old_to_new_code_ids_all_sets =
    let closure_bound_names_all_sets_inside =
      (* When not lifting (i.e. the bound names are variables), we need to
         create a fresh set of irrelevant variables, since the let-bound
         names are not in scope for the closure definition(s). *)
      List.map (fun closure_bound_names ->
          Closure_id.Map.map Name_in_binding_pos.rename closure_bound_names)
        closure_bound_names_all_sets
    in
    let closure_types_via_aliases_all_sets =
      List.map (fun closure_bound_names ->
          Closure_id.Map.map (fun name ->
              T.alias_type_of K.value (Name_in_binding_pos.to_simple name))
            closure_bound_names_all_sets_inside)
        closure_bound_names_all_sets
    in
    let all_function_decls_all_sets =
      List.map (fun all_function_decls_in_set ->
          (* CR mshinwell: [Rec_info] may be wrong. *)
          Closure_id.Map.map (fun function_decl ->
              let new_code_id =
                Code_id.Map.find (FD.code_id function_decl)
                  old_to_new_code_ids_all_sets
              in
              function_decl_type ~denv_prior_to_sets function_decl
                ~code_id:new_code_id (Rec_info.create ~depth:1 ~unroll_to:None))
            all_function_decls_in_set)
        all_function_decls_all_sets
    in
    let closure_types_inside_functions =
      List.map2
        (fun all_function_decls_in_set
             (closure_types_via_aliases,
              closure_element_types_inside_function) ->
          Closure_id.Map.mapi (fun closure_id _function_decl ->
              T.exactly_this_closure closure_id
                ~all_function_decls_in_set
                ~all_closures_in_set:closure_types_via_aliases
                ~all_closure_vars_in_set:closure_element_types_inside_function)
            all_function_decls_in_set)
        all_function_decls_all_sets
        (List.combine closure_types_via_aliases_all_sets
          closure_element_types_inside_functions_all_sets)
    in
    closure_bound_names_all_sets_inside, closure_types_inside_functions

  let bind_closure_types_inside_functions denv_inside_functions
        ~closure_bound_names_inside_functions_all_sets
        ~closure_types_inside_functions_all_sets =
    let denv_inside_functions =
      List.fold_left (fun denv closure_bound_names_inside ->
          Closure_id.Map.fold (fun _closure_id bound_name denv ->
              let name = Name_in_binding_pos.name bound_name in
              let irrelevant = not (Name_in_binding_pos.is_symbol bound_name) in
              let bound_name =
                Name_in_binding_pos.create name
                  (if irrelevant then NM.in_types else NM.normal)
              in
              (* The name may be bound already when reifying the types of
                 continuation parameters at toplevel. *)
              (* CR mshinwell: update out of date comment.  Do we still need
                 [define_name_if_undefined] here? *)
              DE.define_name_if_undefined denv bound_name K.value)
            closure_bound_names_inside
            denv)
        closure_bound_names_inside_functions_all_sets
        denv_inside_functions
    in
    List.fold_left2
      (fun denv closure_bound_names_inside_functions_one_set
            closure_types_inside_functions_one_set ->
        Closure_id.Map.fold (fun closure_id closure_type denv ->
          match
            Closure_id.Map.find closure_id
              closure_bound_names_inside_functions_one_set
          with
          | exception Not_found ->
            Misc.fatal_errorf "No closure name for closure ID %a.@ \
                closure_bound_names_inside_functions_one_set = %a."
              Closure_id.print closure_id
              (Closure_id.Map.print Name_in_binding_pos.print)
              closure_bound_names_inside_functions_one_set
          | bound_name ->
            DE.add_equation_on_name denv
              (Name_in_binding_pos.name bound_name)
              closure_type)
        closure_types_inside_functions_one_set
        denv)
      denv_inside_functions
      closure_bound_names_inside_functions_all_sets
      closure_types_inside_functions_all_sets

  let compute_old_to_new_code_ids_all_sets ~all_function_decls_all_sets =
    List.fold_left
      (fun old_to_new_code_ids_all_sets all_function_decls_in_set ->
        Closure_id.Map.fold (fun _ function_decl old_to_new_code_ids ->
            let old_code_id = FD.code_id function_decl in
            let new_code_id = Code_id.rename old_code_id in
            Code_id.Map.add old_code_id new_code_id old_to_new_code_ids)
          all_function_decls_in_set
          old_to_new_code_ids_all_sets)
      Code_id.Map.empty
      all_function_decls_all_sets

  let bind_existing_code_to_new_code_ids denv ~old_to_new_code_ids_all_sets =
    Code_id.Map.fold (fun old_code_id new_code_id denv ->
        let params_and_body = DE.find_code denv old_code_id in
        DE.define_code denv ~code_id:new_code_id ~newer_version_of:old_code_id
          ~params_and_body)
      old_to_new_code_ids_all_sets
      denv

  let create ~dacc_prior_to_sets ~all_sets_of_closures
        ~closure_element_types_all_sets =
    let old_to_new_code_ids =
      compute_old_to_new_code_ids_all_sets ~all_function_decls_all_sets
    in
    let closure_bound_names_inside_functions_all_sets,
        closure_types_inside_functions_all_sets =
      compute_closure_types_inside_functions ~denv_prior_to_sets
        ~all_function_decls_all_sets ~closure_bound_names_all_sets
        ~closure_element_types_inside_function_all_sets
        ~old_to_new_code_ids_all_sets
    in
    let env_prior_to_sets = DA.typing_env dacc_prior_to_sets in
    let denv_inside_functions =
      dacc_prior_to_sets
      |> DA.denv
      |> DE.enter_closure
      |> DE.increment_continuation_scope_level_twice
    in
    let env_inside_functions,
        closure_element_types_all_sets_inside_functions_rev =
      List.fold_left 
        (fun (env_inside_functions,
              closure_element_types_all_sets_inside_functions_rev)
             closure_element_types ->
          let env_inside_functions, closure_element_types_inside_function =
            compute_closure_element_types_inside_function ~env_prior_to_sets
              ~env_inside_function:env_inside_function ~closure_element_types
          in
          env_inside_functions,
            closure_element_types_inside_function
              :: closure_element_types_all_sets_inside_functions_rev)
        (DE.typing_env denv_inside_functions, [])
        closure_element_types_all_sets
    in
    let closure_element_types_all_sets_inside_functions =
      List.rev closure_element_types_all_sets_inside_functions_rev
    in
    let dacc_inside_functions =
      env_inside_functions
      |> DE.with_typing_env denv
      |> bind_existing_code_to_new_code_ids ~old_to_new_code_ids_all_sets
      |> bind_closure_types_inside_functions
           ~closure_bound_names_inside_functions_all_sets
           ~closure_types_inside_functions_all_sets
      |> DA.with_denv dacc_prior_to_sets
    in
    { dacc_inside_functions;
      closure_bound_names_inside_functions_all_sets;
      closure_types_inside_functions_all_sets;
    }
end

let dacc_inside_function context r ~params ~my_closure closure_id =
  let dacc =
    DA.map_denv (Context_for_multiple_sets.dacc_inside_functions context)
      ~f:(fun denv ->
        let dacc = DE.add_parameters_with_unknown_types denv params in
        let closure_bound_names_inside =
          Context_for_multiple_sets.closure_bound_names_inside_functions context
        in
        match Closure_id.Map.find closure_id closure_bound_names_inside with
        | exception Not_found ->
          Misc.fatal_errorf "No closure name for closure ID %a.@ \
              closure_bound_names = %a.@ closure_bound_names_inside = %a."
            Closure_id.print closure_id
            (Closure_id.Map.print Name_in_binding_pos.print) closure_bound_names
            (Closure_id.Map.print Name_in_binding_pos.print)
            closure_bound_names_inside
        | name ->
          let name = Name_in_binding_pos.name name in
          DE.add_variable denv
            (Var_in_binding_pos.create my_closure NM.normal)
            (T.alias_type_of K.value (Simple.name name)))
  in
  DA.with_r dacc r

type simplify_function_result = {
  function_decl : FD.t;
  new_code_id : Code_id.t;
  params_and_body : Function_params_and_body.t;
  function_type : T.Function_declaration_type.t;
  r : R.t;
}

let simplify_function context r closure_id function_decl =
  let name = Format.asprintf "%a" Closure_id.print closure_id in
  Profile.record_call ~accumulate:true name (fun () ->
    let code_id = FD.code_id function_decl in
    let params_and_body = DE.find_code (DA.denv dacc) code_id in
    let params_and_body, r =
      Function_params_and_body.pattern_match params_and_body
        ~f:(fun ~return_continuation exn_continuation params ~body
                ~my_closure ->
          let dacc =
            dacc_inside_function context r ~params ~my_closure closure_id
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
            let params_and_body =
              Function_params_and_body.create ~return_continuation
                exn_continuation params ~body ~my_closure
            in
            params_and_body, r
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
    let old_code_id = code_id in
    let new_code_id = Code_id.Map.find old_code_id old_to_new_code_ids in
    let function_decl = FD.update_code_id function_decl new_code_id in
    let function_type =
      let denv_prior_to_sets =
        DA.denv dacc
        |> DE.define_code ~code_id:new_code_id ~newer_version_of:old_code_id
          ~params_and_body
        |> DE.add_lifted_constants ~lifted:(R.get_lifted_constants r)
      in
      function_decl_type ~denv_prior_to_sets function_decl Rec_info.initial
    in
    { function_decl;
      new_code_id;
      params_and_body;
      function_type;
      r;
    })

type simplify_set_of_closures0_result = {
  set_of_closures : Flambda.Set_of_closures.t;
  closure_types_by_bound_name : Flambda_type.t Name_in_binding_pos.Map.t;
  newer_versions_of : Code_id.t Code_id.Map.t;
  code : Flambda.Function_params_and_body.t Code_id.Map.t;
  dacc : Downwards_acc.t;
}

let simplify_set_of_closures0 context set_of_closures ~closure_bound_names =
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let all_function_decls_in_set = Function_declarations.funs function_decls in
  let all_function_decls_in_set, code, fun_types, r =
    Closure_id.Map.fold
      (fun closure_id function_decl
           (result_function_decls_in_set, code, fun_types, r) ->
        let { function_decl; new_code_id; params_and_body; function_type; r; } =
          simplify_function context r closure_id function_decl
        in
        let result_function_decls_in_set =
          Closure_id.Map.add closure_id function_decl
            result_function_decls_in_set
        in
        let code = Code_id.Map.add new_code_id params_and_body code in
        let fun_types = Closure_id.Map.add closure_id function_type fun_types in
        result_function_decls_in_set, code, fun_types, r)
      all_function_decls_in_set
      (Closure_id.Map.empty, Code_id.Map.empty, Closure_id.Map.empty, DA.r dacc)
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
  (* CR-someday mshinwell: If adding function return types, a call to
     [T.make_suitable_for_environment] would be needed here, as the return
     types could name the irrelevant variables bound to the closures.  (We
     could further add equalities between those irrelevant variables and the
     bound closure variables themselves.) *)
  let dacc =
    DA.map_denv (DA.with_r dacc r) ~f:(fun denv ->
      denv
      |> DE.add_lifted_constants ~lifted:(R.get_lifted_constants r)
      |> Closure_id.Map.fold (fun _closure_id bound_name denv ->
             DE.define_name_if_undefined denv bound_name K.value)
           closure_bound_names
      |> Name_in_binding_pos.Map.fold (fun bound_name closure_type denv ->
             let bound_name = Name_in_binding_pos.to_name bound_name in
             DE.add_equation_on_name denv bound_name closure_type)
           closure_types_by_bound_name)
  in
  let set_of_closures =
    Function_declarations.create all_function_decls_in_set
    |> Set_of_closures.create ~closure_elements
  in
  let newer_versions_of = Code_id.invert_map old_to_new_code_ids in
  { set_of_closures;
    closure_types_by_bound_name;
    newer_versions_of;
    code;
    dacc;
  }

let simplify_and_lift_set_of_closures dacc ~closure_bound_vars_inverse
      ~closure_bound_vars set_of_closures ~closure_elements =
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
  let closure_element_types =
    Var_within_closure.Map.map (fun closure_element ->
        match Simple.descr closure_element with
        | Const _ | Name (Symbol _) -> T.alias_type_of K.value closure_element
        | Name (Var var) ->
          match Variable.Map.find var closure_bound_vars_all_sets_inverse with
          | exception Not_found ->
            assert (DE.mem_variable (DA.denv dacc) var);
            T.alias_type_of K.value closure_element
          | closure_id ->
            let closure_symbol =
              Closure_id.Map.find closure_id closure_symbols
            in
            T.alias_type_of K.value (Simple.symbol closure_symbol))
      closure_elements
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      Closure_id.Map.fold (fun _closure_id closure_symbol denv ->
          DE.now_defining_symbol denv closure_symbol)
        closure_symbols
        denv)
  in
  let _set_of_closures, dacc, types_of_symbols, bound_symbols, static_const =
    Simplify_static_const.simplify_set_of_closures0 dacc
      set_of_closures ~closure_symbols ~closure_elements ~closure_element_types
      ~all_function_decls_in_other_sets:Closure_id.Map.empty
      ~all_closure_bound_names_in_other_sets:Closure_id.Map.empty
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      Closure_id.Map.fold (fun _closure_id closure_symbol denv ->
          DE.no_longer_defining_symbol denv closure_symbol)
        closure_symbols
        denv)
  in
  let set_of_closures_lifted_constant =
    Lifted_constant.create (DA.denv dacc) bound_symbols static_const
      ~types_of_symbols
  in
  let dacc =
    DA.map_r dacc ~f:(fun r ->
      R.new_lifted_constant r set_of_closures_lifted_constant)
  in
  let r = DA.r dacc in
  let denv =
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

let simplify_non_lifted_set_of_closures0 context ~bound_vars ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types =
  let closure_bound_names =
    Closure_id.Map.map Name_in_binding_pos.var closure_bound_vars
  in
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let all_function_decls_in_set = Function_declarations.funs function_decls in
  let { set_of_closures;
        closure_types_by_bound_name = _;
        newer_versions_of;
        code;
        dacc;
      } =
    simplify_set_of_closures0 context set_of_closures ~closure_bound_names
  in
  let defining_expr =
    Reachable.reachable (Named.create_set_of_closures set_of_closures)
  in
  (* CR mshinwell: This next part should probably be shared between the
     lifted and non-lifted cases; we always need the new code in the
     environment and [r]. *)
  let dacc =
    DA.map_r dacc ~f:(fun r ->
      R.new_lifted_constant r
        (Lifted_constant.create_pieces_of_code (DA.denv dacc)
          code ~newer_versions_of))
  in
  let dacc =
    (* CR mshinwell: This seems weird.  Should there ever be lifted constants
       in the [r] component of a [dacc] that are not in the [denv] component
       of a dacc?  If not maybe we could enforce that via a check.
       (This comment predates the addition of the code to [dacc]
       just above.) *)
    DA.map_denv dacc ~f:(fun denv ->
      DE.add_lifted_constants denv
        ~lifted:(R.get_lifted_constants (DA.r dacc)))
  in
  [bound_vars, defining_expr], dacc

type lifting_decision_result = {
  can_lift : bool;
  closure_elements : Simple.t Var_within_closure.Map.t;
  closure_element_types : T.t Var_within_closure.Map.t;
}

let type_closure_elements_and_make_lifting_decision_for_one_set dacc
      ~min_name_mode ~closure_bound_vars_inverse set_of_closures =
  (* By computing the types of the closure elements, attempt to show that
     the set of closures can be lifted, and hence statically allocated.
     Note that simplifying the bodies of the functions won't change the
     set-of-closures' eligibility for lifting.  That this is so follows
     from the fact that closure elements cannot be deleted without a global
     analysis, as an inlined function's body may reference them out of
     scope of the closure declaration. *)
  let closure_elements, closure_element_types =
    List.fold_left
      (fun (closure_elements, closure_element_types) set_of_closures ->
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
          (closure_elements, closure_element_types))
      (Var_within_closure.Map.empty, Var_within_closure.Map.empty)
      all_sets_of_closures
  in
  (* Note that [closure_bound_vars_inverse] doesn't need to include
     variables binding closures in other mutually-recursive sets, since if
     we get here in the case where we are considering lifting a set that has
     not been lifted before, there are never any other mutually-recursive
     sets ([Named.t] does not allow them). *)
  let can_lift =
    Var_within_closure.Map.for_all (fun _ simple ->
        match Simple.descr simple with
        | Const _ | Name (Symbol _) -> true
        | Name (Var var) -> Variable.Map.mem var closure_bound_vars_inverse)
      closure_elements
  in
  { can_lift;
    closure_elements;
    closure_element_types;
  }

let type_closure_elements_for_previously_lifted_set dacc
      ~min_name_mode set_of_closures =
  let ({ can_lift; _ } as result) =
    type_closure_elements_and_make_lifting_decision_for_one_set dacc
      ~min_name_mode ~closure_bound_vars_inverse:Variable.Map.empty
      set_of_closures
  in
  assert can_lift;
  result

let simplify_non_lifted_set_of_closures dacc
      ~(bound_vars : Bindable_let_bound.t) set_of_closures =
  let closure_bound_vars =
    Bindable_let_bound.must_be_set_of_closures bound_vars
  in
  (* CR mshinwell: This should probably be handled differently, but
     will require some threading through *)
  let min_name_mode =
    Bindable_let_bound.name_mode bound_vars
  in
  let closure_bound_vars_all_sets_inverse =
    Closure_id.Map.fold (fun closure_id var closure_bound_vars_inverse ->
        Variable.Map.add (Var_in_binding_pos.var var) closure_id
          closure_bound_vars_inverse)
      closure_bound_vars
      Variable.Map.empty
  in
  let { can_lift; closure_elements; closure_element_types; } =
    type_closure_elements_and_make_lifting_decision_for_one_set dacc
      ~min_name_mode ~closure_bound_vars_all_sets_inverse set_of_closures
  in
  let context =
    Context_for_multiple_sets_of_closures.create
      ~denv_prior_to_sets:(DA.denv dacc)
      ~all_sets_of_closures:[set_of_closures]
  in
  match can_lift with
  | Can_lift ->
    simplify_and_lift_set_of_closures context ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types
  | Cannot_lift ->
    simplify_non_lifted_set_of_closures0 context ~bound_vars ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types

let simplify_lifted_set_of_closures0 dacc context ~closure_symbols
      set_of_closures =
  let closure_bound_names =
    Closure_id.Map.map Name_in_binding_pos.symbol closure_symbols
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      Closure_id.Map.fold (fun _closure_id symbol denv ->
          DE.define_symbol_if_undefined denv symbol K.value)
        closure_symbols
        denv)
  in
  let { set_of_closures;
        closure_types_by_bound_name;
        newer_versions_of;
        code;
        dacc;
      } =
    simplify_set_of_closures0 context set_of_closures ~closure_bound_names
  in
  (* CR mshinwell: See comment in simplify_non_lifted_set_of_closures about
     the following *)
  let dacc =
    DA.map_r dacc ~f:(fun r ->
      R.new_lifted_constant r
        (Lifted_constant.create_pieces_of_code (DA.denv dacc)
          code ~newer_versions_of))
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      DE.add_lifted_constants denv
        ~lifted:(R.get_lifted_constants (DA.r dacc)))
  in
  let code =
    Code_id.Map.mapi (fun code_id params_and_body : Static_const.code ->
        { params_and_body = Present params_and_body;
          newer_version_of = Code_id.Map.find_opt code_id newer_versions_of;
        })
      code
  in
  let bound_symbols_component : Bound_symbols.Code_and_set_of_closures.t =
    Code_and_set_of_closures {
      code_ids = Code_id.Map.keys code;
      closure_symbols;
    }
  in
  let code_and_set_of_closures : Static_const.code_and_set_of_closures =
    Code_and_set_of_closures {
      code;
      set_of_closures = Some set_of_closures;
    }
  in
  bound_symbols_component, code_and_set_of_closures, dacc

let simplify_lifted_sets_of_closures dacc ~orig_bound_symbols ~orig_static_const
      (bound_symbols_components : Bound_symbols.Code_and_set_of_closures.t list)
      (code_and_sets_of_closures : Static_const.code_and_set_of_closures list) =
  if List.compare_lengths bound_symbols sets <> 0 then begin
    Misc.fatal_errorf "Differing number of bound symbols and static constant \
        set-of-closures definitions for@ %a@ =@ %a"
      Bound_symbols.print orig_bound_symbols
      Static_const.print orig_static_const
  end;
  let dacc =
    (* Unlike in the cases above that start from [Let]-bindings, in this case
       the code may be in the same definition as the closure(s), so we must
       also add such code to the environment.  (See [Static_const].) *)
    List.fold_left2
      (fun dacc
           ({ code_ids; closure_symbols; }
             : Bound_symbols.Code_and_set_of_closures.t)
           ({ code; set_of_closures; }
             : Static_const.code_and_set_of_closures) ->
        (* CR mshinwell: Check closure IDs between [closure_symbols] and
           [set_of_closures] too. *)
        let code_ids' = Code_id.Map.keys code in
        if not (Code_id.Set.equal code_ids code_ids') then begin
          Misc.fatal_errorf "Mismatch on declared code IDs (%a and %a):@ %a"
            Code_id.Set.print code_ids
            Code_id.Set.print code_ids'
            Static_const.print static_const
        end;
        Code_id.Map.fold
          (fun code_id
               ({ params_and_body; newer_version_of; } : Static_const.code)
               dacc ->
            (* CR mshinwell: Add check to ensure there are no
               unbound names in the code, since we're not simplifying on the
               way down. *)
            let define_code denv =
              match params_and_body with
              | Deleted -> denv
              | Present params_and_body ->
                DE.define_code denv ?newer_version_of ~code_id
                  ~params_and_body
            in
            let dacc = DA.map_denv dacc ~f:define_code in
            dacc)
          code
          dacc)
      dacc
      bound_symbols_components code_and_sets_of_closures
  in
  let sets_of_closures =
    List.filter_map (fun { code = _; set_of_closures; } -> set_of_closures)
      code_and_sets_of_closures
  in
  let closure_elements_and_types =
    List.map
      (fun ({ code = _; set_of_closures; }
            : Static_const.code_and_set_of_closures) ->
        Option.map (fun set_of_closures ->
            let { Simplify_named.
                  can_lift;
                  closure_elements;
                  closure_element_types;
                } =
              Simplify_named.type_closure_elements_for_previously_lifted_set
                dacc ~min_name_mode:Name_mode.normal set_of_closures
            in
            assert can_lift;
            closure_elements, closure_element_types)
          set_of_closures)
      code_and_sets_of_closures
  in
  let context =
    Simplify_sets_of_closures.Context_for_multiple_sets.create
      ~dacc_prior_to_sets:dacc
      sets_of_closures
      ~closure_element_types_all_sets:(List.map snd closure_elements_and_types)
  in
  let bound_symbols_components_rev, code_and_sets_of_closures_rev, dacc =
    List.fold_left2
      (fun (bound_symbols_components_rev, code_and_sets_of_closures_rev, dacc)
           (({ code_ids; closure_symbols; } as bound_symbol_component)
             : Bound_symbols.Code_and_set_of_closures.t)
           (({ code; set_of_closures; } as code_and_set_of_closures)
             : Static_const.code_and_set_of_closures) ->
        let bound_symbol_component, code_and_set_of_closures, dacc =
          match set_of_closures with
          | None ->
            (* We don't currently simplify code on the way down.  [Un_cps] will
               however check the code to ensure there are no unbound names. *)
            bound_symbol_component, code_and_set_of_closures, dacc
          | Some set_of_closures ->
            simplify_lifted_set_of_closures0 dacc context ~closure_symbols
              set_of_closures
        in
        bound_symbol_component :: bound_symbols_components_rev,
          code_and_set_of_closures :: code_and_sets_of_closures_rev,
          dacc)
      ([], [], dacc)
      bound_symbols_components code_and_sets_of_closures
  in
  let bound_symbols : Bound_symbols.t =
    Sets_of_closures (List.rev bound_symbols_components_rev)
  in
  let static_const : Static_const.t =
    Sets_of_closures (List.rev code_and_sets_of_closures_rev)
  in
  bound_symbols, static_const, dacc
