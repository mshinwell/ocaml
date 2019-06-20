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

open! Flambda.Import

module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type
module TE = T.Typing_env
module UA = Upwards_acc

type pre_simplification_types_of_my_closures = {
  set_of_closures : (Name.t * Flambda_type.t) option;
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
        let dacc =
          DA.add_continuation dacc return_continuation
            ~definition_scope_level:return_cont_scope
            result_arity
        in
        let dacc =
          DA.add_exn_continuation dacc exn_continuation
            ~definition_scope_level:exn_cont_scope
        in
(*
Format.eprintf "Closure ID %a, done entering closure\n%!"
  Closure_id.print closure_id;
*)
        let denv =
          DE.increment_continuation_scope_level
            (DE.increment_continuation_scope_level denv)
        in
        let denv = DE.add_parameters_with_unknown_types denv params in
(*
Format.eprintf "Closure ID %a, adding type_of_my_closure:@ %a\n%!"
  Closure_id.print closure_id
  T.print (type_of_my_closure closure_id);
*)
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
        let denv = DE.add_variable denv my_closure type_of_my_closure in
        let denv =
          match pre_simplification_types_of_my_closures.set_of_closures with
          | None -> denv
          | Some (set_of_closures, set_of_closures_type) ->
            DE.add_equation_on_name denv set_of_closures set_of_closures_type
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
      T.create_inlinable_function_declaration function_decl
        (Rec_info.create ~depth:0 ~unroll_to:None)
    else
      T.create_non_inlinable_function_declaration
        ~param_arity ~result_arity
        ~recursive:(Function_declaration.recursive function_decl)
  in
  function_decl, function_decl_type, r

let lift_set_of_closures dacc set_of_closures ~closure_elements_and_types
      ~(result_vars : Bindable_let_bound.t) =
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
  let term = Named.create_simple set_of_closures_symbol in
  let ty = T.alias_type_of (T.kind ty) set_of_closures_symbol in
  let dacc =
    DA.map_denv dacc ~f:(fun denv -> DE.add_variable denv result_var ty)
  in
  Reachable.reachable term, DA.with_r dacc r

let pre_simplification_types_of_my_closures denv ~funs ~closure_element_types =
  (* CR mshinwell: This variable is problematic; we must not let it appear
     in any term (e.g. by introducing a [Project_closure]). *)
  let set_of_closures_var = Variable.create "set_of_closures" in
  let set_of_closures_ty_fabricated =
    T.alias_type_of_as_ty_fabricated (Simple.var set_of_closures_var)
  in
  let closure_element_types =
    Var_within_closure.Map.map (fun ty_value ->
        T.erase_aliases_ty_value (DE.typing_env denv)
          ~allowed:Variable.Set.empty ty_value)
      closure_element_types
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
        T.closure closure_id function_decl_type closure_element_types
          ~set_of_closures:set_of_closures_ty_fabricated)
      funs
  in
  let set_of_closures_type = T.set_of_closures ~closures:closure_types in
  { set_of_closures = Some (Name.var set_of_closures_var, set_of_closures_type);
    closure_types;
  }

let simplify_set_of_closures dacc set_of_closures
      ~(result_vars : Bindable_let_bound.t) =
  (* By simplifying the types of the closure elements, attempt to show that
     the set of closures can be lifted, and hence statically allocated. *)
  let closure_elements, closure_element_types =
    Var_within_closure.Map.fold
      (fun var_within_closure simple
           (closure_elements, closure_element_types) ->
        (* CR mshinwell: This should probably be handled differently, but
           will require some threading through *)
        let simple, ty =
          match S.simplify_simple dacc simple with
          | Bottom kind ->
            assert (K.equal kind K.value);
            simple, T.bottom kind
          | Ok (simple, ty) -> simple, ty
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
      ~result_vars
  else
    let function_decls = Set_of_closures.function_decls set_of_closures in
    let funs = Function_declarations.funs function_decls in
    (* Note that simplifying the bodies of the functions won't change the
       set-of-closures' eligibility for lifting.  That this is so follows
       from the fact that closure elements (at least in the presence of
       out-of-scope inlining, which will be implemented in due course) cannot
       be deleted without a global analysis. *)
(*
Format.eprintf "Environment outside functions:\n%a\n%!"
  T.Typing_env.print (DE.typing_env env);
*)
    let pre_simplification_types_of_my_closures =
      pre_simplification_types_of_my_closures (DA.denv dacc)
        ~funs ~closure_element_types
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
    Reachable.reachable term, dacc

let simplify_named0 dacc (named : Named.t)
      ~(result_vars : Bindable_let_bound.t) =
(*Format.eprintf "Simplifying binding of %a\n%!" Variable.print result_var;*)
  match named with
  | Simple simple ->
(*let orig_simple = simple in*)
    let dacc, result_occurrence_kind =
      DA.map_denv dacc ~f:(fun denv ->
        match result_vars with
        | Singleton result_var ->
          DE.add_variable denv result_var ty,
            Var_in_binding_pos.occurrence_kind result_var
        | Set_of_closures _ ->
          Misc.fatal_errorf "[Simple]s can only be bound to [Singleton]s:@ %a"
            print named)
    in
    begin match S.simplify_simple dacc simple ~result_occurrence_kind with
    | Bottom kind -> Reachable.invalid (), dacc, T.bottom kind
    | Ok (simple, ty) ->
(*Format.eprintf "Simplified %a --> %a, type %a\n%!"
  Simple.print orig_simple
  Simple.print simple
  T.print ty;*)
      Reachable.reachable (Named.create_simple simple), dacc
    end
  | Prim (prim, dbg) ->
    let result_var =
      match result_vars with
      | Singleton result_var -> result_var
      | Set_of_closures _ ->
        Misc.fatal_errorf "[Prim]s can only be bound to [Singleton]s:@ %a"
          print named
    in
    let term, env_extension, dacc =
      Simplify_primitive.simplify_primitive dacc prim dbg ~result_var
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        let kind = Flambda_primitive.result_kind' prim in
        let denv = DE.add_variable denv result_var (T.unknown kind) in
        DE.extend_typing_environment denv env_extension)
    in
    (* CR mshinwell: Think about whether [cannot_lift] is actually needed.
       It seems like maybe not: if we don't know the result of the primitive
       then the type should be Unknown. *)
    let cannot_lift =
      not (Flambda_primitive.With_fixed_value.eligible prim)
    in
    Reification.try_to_reify dacc term ~bound_to:result_var ~cannot_lift
  | Set_of_closures set_of_closures ->
    simplify_set_of_closures dacc set_of_closures ~result_vars

let simplify_named dacc named result_vars =
  let named, dacc, ty = simplify_named0 dacc named ~result_vars in
  let named : Reachable.t =
    match named with
    | Invalid _ -> named
    | Reachable _ ->
      let denv = DA.denv dacc in
      if T.is_bottom (DE.typing_env denv) ty then Reachable.invalid ()
      else named
  in
  named, dacc
