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
module R = Simplify_env_and_result.Result
module T = Flambda_type
module TE = T.Typing_env
module UA = Upwards_acc

let simplify_function dacc closure_id function_decl ~type_of_my_closure =
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
        let scope = Scope.initial in
        let dacc =
          DA.add_continuation dacc return_continuation
            ~definition_scope_level:scope
            result_arity
        in
        let dacc =
          DA.add_exn_continuation dacc exn_continuation
            ~definition_scope_level:scope
        in
        let denv = DE.enter_closure denv in
(*
Format.eprintf "Closure ID %a, done entering closure\n%!"
  Closure_id.print closure_id;
*)
        let denv = DE.add_parameters_with_unknown_types denv params in
(*
Format.eprintf "Closure ID %a, adding type_of_my_closure:@ %a\n%!"
  Closure_id.print closure_id
  T.print (type_of_my_closure closure_id);
*)
        let type_of_my_closure =
          type_of_my_closure closure_id ~param_arity ~result_arity
        in
        let denv = DE.add_variable denv my_closure type_of_my_closure in
        let denv = DE.increment_continuation_scope_level denv in
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
              scope
          with Misc.Fatal_error -> begin
            Format.eprintf "\n%sContext is:%s simplifying function \
                with closure ID %a,@ params %a,@ return continuation %a,@ \
                exn continuation %a,@ my_closure %a,@ body:@ %a@ \
                with downwards accumulator:@ %a\n"
              (Misc.Color.bold_red ())
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
    if Inlining_decision.can_inline denv function_decl then
      T.create_inlinable_function_declaration function_decl
    else
      T.create_non_inlinable_function_declaration
        ~param_arity ~result_arity
  in
  function_decl, function_decl_type, r

let lift_set_of_closures dacc set_of_closures ~closure_elements_and_types
      ~result_var =
  let set_of_closures_symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create (Variable.unique_name result_var))
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

let simplify_set_of_closures dacc set_of_closures ~result_var =
  (* By simplifying the types of the closure elements, attempt to show that
     the set of closures can be lifted, and hence statically allocated. *)
  let closure_elements, closure_element_types =
    Var_within_closure.Map.fold
      (fun var_within_closure simple
           (closure_elements, closure_element_types) ->
        let simple, ty = Simplify_simple.simplify_simple dacc simple in
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
        match simple with
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
    let internal_closure_element_types =
      Var_within_closure.Map.map (fun ty_value ->
          T.erase_aliases_ty_value ty_value ~allowed:Variable.Set.empty)
        closure_element_types
    in
    let type_of_my_closure closure_id ~param_arity ~result_arity =
      (* CR mshinwell: Think more: what should the set of closures type be? *)
      T.closure closure_id
        (T.create_non_inlinable_function_declaration ~param_arity ~result_arity)
        internal_closure_element_types
        ~set_of_closures:(T.unknown_as_ty_fabricated ())
    in
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
    let funs, fun_types, r =
      Closure_id.Map.fold
        (fun closure_id function_decl (funs, fun_types, r) ->
          let dacc = DA.with_r dacc r in
          let function_decl, ty, r =
            simplify_function dacc closure_id function_decl ~type_of_my_closure
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
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        DE.add_variable denv result_var set_of_closures_type)
    in
    let term = Named.create_set_of_closures set_of_closures in
    Reachable.reachable term, dacc, set_of_closures_type

let simplify_named0 dacc (named : Named.t) ~result_var =
(*Format.eprintf "Simplifying binding of %a\n%!" Variable.print result_var;*)
  match named with
  | Simple simple ->
(*let orig_simple = simple in*)
    let simple, ty = Simplify_simple.simplify_simple dacc simple in
(*Format.eprintf "Simplified %a --> %a, type %a\n%!"
  Simple.print orig_simple
  Simple.print simple
  T.print ty;*)
    let dacc =
      DA.map_denv dacc ~f:(fun denv -> DE.add_variable denv result_var ty)
    in
    Reachable.reachable (Named.create_simple simple), dacc, ty
  | Prim (prim, dbg) ->
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
    simplify_set_of_closures dacc set_of_closures ~result_var

let simplify_named dacc named ~result_var =
  let named, dacc, ty = simplify_named0 dacc named ~result_var in
  let named : Reachable.t =
    match named with
    | Invalid _ -> named
    | Reachable _ ->
      let denv = DA.denv dacc in
      if T.is_bottom (DE.typing_env denv) ty then Reachable.invalid ()
      else named
  in
  named, dacc, ty
