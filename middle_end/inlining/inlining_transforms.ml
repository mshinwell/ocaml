(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
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

(*

let which_function_parameters_can_we_specialise ~params ~args
      ~args_tys ~(invariant_params:Variable.Set.t Variable.Map.t lazy_t)
      ~specialised_args =
  assert (List.length params = List.length args);
  assert (List.length args = List.length args_tys);
  List.fold_right2 (fun (var, arg) ty
    (worth_specialising_args, spec_args) ->
      let spec_args =
        if Variable.Map.mem var (Lazy.force invariant_params) ||
           Variable.Set.mem var specialised_args
        then
          Variable.Map.add var arg spec_args
        else
          spec_args
      in
      let worth_specialising_args =
        if T.useful ty
          && Variable.Map.mem var (Lazy.force invariant_params)
        then
          Variable.Set.add var worth_specialising_args
        else
          worth_specialising_args
      in
      worth_specialising_args, spec_args)
    (List.combine params args) args_tys
    (Variable.Set.empty, Variable.Map.empty)

*)

(* XXX Needs fixing
let fold_over_projections_of_vars_bound_by_closures
      ~closure_id_being_applied
      ~lhs_of_application
      ~(function_decls : Flambda.Function_declarations.t)
      ~init
      ~f =
  let function_names = Variable.Map.keys function_decls.funs in
  Variable.Map.fold
    (fun _function_var (function_decl : Flambda.Function_declaration.t) acc ->
       let (--) = Variable.Set.diff in
       let function_free_vars = function_decl.free_variables in
       let function_params = Parameter.Set.vars function_decl.params in
       let bound_variables =
         (function_free_vars -- function_params) -- function_names
       in
       Variable.Set.fold
         (fun var acc ->
            let expr : Flambda.named =
              Project_var {
                closure = lhs_of_application;
                closure_id = closure_id_being_applied;
                var = Var_within_closure.wrap var;
              }
            in
            f ~acc ~var ~expr)
         bound_variables
         acc)
    function_decls.funs
    init
*)

let set_inline_attribute_on_all_apply body inline specialise =
  Flambda.Expr.Mappers.Toplevel_only.map_expr (function
      | Apply apply -> Apply { apply with inline; specialise }
      | expr -> expr)
    body

(** Assign fresh names for a function's parameters and rewrite the body to
    use these new names. *)
let copy_of_function's_body_with_freshened_params _env
      ~(function_decl : T.inlinable_function_declaration) =
  let params = function_decl.params in
  let param_vars =
    List.map (fun (param, _ty) -> Parameter.var param) params
  in
  (* We cannot avoid the substitution in the case where we are inlining
     inside the function itself.  This can happen in two ways: either
     (a) we are inlining the function itself directly inside its declaration;
     or (b) we are inlining the function into an already-inlined copy.
     For (a) we cannot short-cut the substitution by freshening since the
     original [params] may still be referenced; for (b) we cannot do it
     either since the freshening may already be renaming the parameters for
     the first inlining of the function. *)
(* CR mshinwell: Re-enable once functions are implemented
  if E.does_not_bind env param_vars
    && E.does_not_freshen env param_vars
  then
    params, function_decl.body
  else
*)
    let freshened_params =
      List.map (fun (param, ty) ->
          let param =
            Parameter.map_var (fun var -> Variable.rename var) param
          in
          param, ty)
        params
    in
    let freshened_param_vars =
      List.map (fun (param, _ty) -> Parameter.var param) freshened_params
    in
    let subst =
      Variable.Map.of_list (List.combine param_vars freshened_param_vars)
    in
    let body =
      Flambda.Expr.toplevel_substitution subst function_decl.body
    in
    freshened_params, body

(** Inline a function by copying its body into a context where it becomes
    closed.  That is to say, we bind the free variables of the body
    (= "variables bound by the closure"), and any function identifiers
    introduced by the corresponding set of closures. *)
let inline_by_copying_function_body ~env ~r
      ~(set_of_closures : T.set_of_closures)
      ~callee
      ~(inline_requested : Flambda.inline_attribute)
      ~(specialise_requested : Flambda.specialise_attribute)
      ~callee's_closure_id
      ~(function_decl : T.inlinable_function_declaration) ~args
      ~continuation ~dbg =
  ignore set_of_closures;
  ignore callee's_closure_id;
  assert (E.mem_name env callee);
  assert (List.for_all (E.mem_simple env) args);
  let r =
    if function_decl.stub then r
    else R.map_benefit r B.remove_call
  in
  let freshened_params, body =
    copy_of_function's_body_with_freshened_params env ~function_decl
  in
  let body =
    if function_decl.stub &&
       ((inline_requested <> Flambda.Default_inline)
        || (specialise_requested <> Flambda.Default_specialise))
    then
      (* When the function inlined function is a stub, the annotation
         is reported to the function applications inside the stub.
         This allows to report the annotation to the application the
         original programmer really intended: the stub is not visible
         in the source. *)
      set_inline_attribute_on_all_apply body
        inline_requested specialise_requested
    else
      body
  in
  (* Arrange for the continuation through which the function returns to be
     that supplied at the call site. *)
  let handlers =
    (* CR mshinwell: this occurs in [Inlining_decision] too, factor out *)
    let return_arity = List.map (fun ty -> T.kind ty) function_decl.result in
    let parameter_types = T.unknown_types_from_arity return_arity in
    Flambda_utils.make_let_cont_alias
      ~name:function_decl.continuation_param
      ~alias_of:continuation
      ~parameter_types
  in
  let body : Flambda.Expr.t = Let_cont { body; handlers; } in
  let bindings_for_params_to_args =
    (* Bind the function's parameters to the arguments from the call site. *)
    let bindings =
      List.map2 (fun (param, ty) arg ->
          let var = Parameter.var param in
          let kind = T.kind ty in
          var, kind, Flambda.Named.Simple arg)
        freshened_params
        args
    in
    Flambda.Expr.bind ~body ~bindings
  in
  let expr =
    Flambda.Expr.create_let function_decl.my_closure
      (Flambda_kind.value ())
      (Simple (Simple.name callee))
      bindings_for_params_to_args
  in
  let env = E.activate_freshening (E.set_never_inline env) in
  let env = E.set_inline_debuginfo ~dbg env in
  (E.simplify_expr env) env r expr

(*

let inline_by_copying_function_declaration ~env ~r
    ~(function_decls : Flambda.Function_declarations.t)
    ~callee
    ~(inline_requested : Flambda.inline_attribute)
    ~callee's_closure_id
    ~(function_decl : Flambda.Function_declaration.t)
    ~args ~args_tys ~continuation
    ~(invariant_params:Variable.Set.t Variable.Map.t lazy_t)
    ~(specialised_args : Flambda.specialised_to Variable.Map.t)
    ~direct_call_surrogates ~dbg ~simplify =
  let function_decls =
    (* To simplify a substitution (see comment below), rewrite any references
       to closures in the set being defined that go via symbols, so they go
       via closure variables instead. *)
    let make_closure_symbol =
      let module Backend = (val (E.backend env) : Backend_intf.S) in
      Backend.closure_symbol
    in
    Freshening.rewrite_recursive_calls_with_symbols
      (Freshening.activate Freshening.empty)
      ~make_closure_symbol
      function_decls
  in
  let original_function_decls = function_decls in
  let specialised_args_set = Variable.Map.keys specialised_args in
  let worth_specialising_args, specialisable_args =
    which_function_parameters_can_we_specialise
      ~params:(Parameter.List.vars function_decl.params) ~args ~args_tys
      ~invariant_params
      ~specialised_args:specialised_args_set
  in
  (* Arguments of functions that are not directly called but are
     aliased to arguments of a directly called one may need to be
     marked as specialised. *)
  let specialisable_args_with_aliases =
    Variable.Map.fold (fun arg outside_var map ->
        match Variable.Map.find arg (Lazy.force invariant_params) with
        | exception Not_found -> map
        | set ->
          Variable.Set.fold (fun alias map ->
              Variable.Map.add alias outside_var map)
            set map)
      specialisable_args specialisable_args
  in
  (* The other closures from the same set of closures may have
     specialised arguments. Those refer to variables that may not be
     bound anymore in the current environment. The only allowed
     remaining specialised arguments after duplicating a function are
     those that either comes from the free variables of set of
     closures or the arguments of the closure being applied (and
     propagated transitively to other functions). This is ensured by
     the fact that no closure not directly required by the closure
     being applied are kept in the set. If an argument of an other
     function of the set does not come from the closure being applied
     then, that function cannot be applied (unreachable from the one
     being aplied).

     For specialised arguments of other function to reference a valid
     value, they need to be rewritten accordingly to the ones of the
     closure being applied. *)
  let specialisable_renaming =
    Variable.Map.fold (fun param outside_var map ->
        match Variable.Map.find param specialised_args with
        | exception Not_found ->
          (* Newly specialised argument: no other function argument
             may need renaming for that one *)
          map
        | original_spec_to ->
          match original_spec_to.var with
          | Some original_outside_var ->
            let spec_to =
              { original_spec_to with var = Some outside_var; }
            in
            Variable.Map.add original_outside_var spec_to map
          | None ->
            Misc.fatal_errorf "No equality to variable for specialised arg %a"
              Variable.print param)
      specialisable_args_with_aliases Variable.Map.empty
  in
  if Variable.Set.subset worth_specialising_args specialised_args_set
  then
    (* Don't duplicate the function definition if we would make its
       specialisation information worse.  (Note that this judgement is made
       based only on those arguments found to be invariant with known-useful
       types, rather than on all invariant arguments.) *)
    None
  else
    let set_of_closures_var = new_var "dup_set_of_closures" in
    let required_functions =
      Flambda_utils.closures_required_by_entry_point ~backend:(E.backend env)
        ~entry_point:closure_id_being_applied
        function_decls
    in
    (* The free variable map for the duplicated declaration(s) maps the
       "internal" names used within the function bodies to fresh names,
       which in turn are bound to projections from the set of closures being
       copied.  We add these bindings using [Let] around the new
       set-of-closures declaration. *)
    let free_vars, free_vars_for_lets =
<<<<<<< HEAD:middle_end/inlining/inlining_transforms.ml
      fold_over_projections_of_vars_bound_by_closure ~callee's_closure_id
        ~callee ~function_decls ~init:(Variable.Map.empty, [])
=======
      let function_decls =
        Flambda.create_function_declarations
          ~funs:(Variable.Map.filter
                   (fun var _ -> Variable.Set.mem var required_functions)
                   function_decls.funs)
      in
      fold_over_projections_of_vars_bound_by_closures
        ~closure_id_being_applied
        ~lhs_of_application
        ~function_decls
        ~init:(Variable.Map.empty, [])
>>>>>>> ocaml/trunk:middle_end/inlining_transforms.ml
        ~f:(fun ~acc:(map, for_lets) ~var:internal_var ~expr ->
          let from_closure : Flambda.Free_var.t =
            { var = new_var "from_closure";
              projection = None;
            }
          in
          Variable.Map.add internal_var from_closure map,
            (from_closure.var, expr)::for_lets)
    in
<<<<<<< HEAD:middle_end/inlining/inlining_transforms.ml
    let required_functions =
      Flambda.Function_declarations.closures_required_by_entry_point
        ~backend:(E.backend env)
        ~entry_point:callee's_closure_id
        function_decls
    in
=======
>>>>>>> ocaml/trunk:middle_end/inlining_transforms.ml
    let funs =
      Variable.Map.filter (fun func _ ->
          Variable.Set.mem func required_functions)
        function_decls.funs
    in
    let free_vars, free_vars_for_lets, original_vars =
      (* Bind all the closures from the original (non-specialised) set as
         free variables in the set.  This means that we can reference them
         when some particular recursive call cannot be specialised.  See
         detailed comment below. *)
      Variable.Map.fold (fun fun_var _fun_decl
                (free_vars, free_vars_for_lets, original_vars) ->
          let var = Variable.create "closure" in
          let original_closure : Flambda.Named.t =
            Move_within_set_of_closures
              { closure = callee;
                move = Closure_id.Map.add callee's_closure_id
                  (Closure_id.wrap fun_var) Closure_id.Map.empty;
              }
          in
          let internal_var = Variable.rename ~append:"_original" fun_var in
          let free_vars =
            Variable.Map.add internal_var
              ({ var; projection = None; } : Flambda.Free_var.t)
              free_vars
          in
          free_vars,
            (var, original_closure) :: free_vars_for_lets,
            Variable.Map.add fun_var internal_var original_vars)
        funs
        (free_vars, free_vars_for_lets, Variable.Map.empty)
    in
    let direct_call_surrogates =
      Closure_id.Map.fold (fun existing surrogate surrogates ->
          let existing = Closure_id.unwrap existing in
          let surrogate = Closure_id.unwrap surrogate in
          if Variable.Map.mem existing funs
            && Variable.Map.mem surrogate funs
          then
            Variable.Map.add existing surrogate surrogates
          else
            surrogates)
        direct_call_surrogates
        Variable.Map.empty
    in
    let function_decls =
      Flambda.Function_declarations.update ~funs function_decls
    in
    let all_functions_parameters =
      Flambda.Function_declarations.all_functions_parameters function_decls
    in
    let specialisable_args =
      Variable.Map.merge (fun param v1 v2 ->
          match v1, v2 with
          | None, None -> None
          | Some var, _ ->
            (* New specialised argument being introduced. *)
            let spec_to : Flambda.specialised_to =
              { var = Some var;
                projection = None;
              }
            in
            Some spec_to
          | None, Some (spec_to : Flambda.specialised_to) ->
            (* Renaming an existing specialised argument. *)
            match spec_to.var with
            | Some var ->
              if Variable.Set.mem param all_functions_parameters then
                match Variable.Map.find var specialisable_renaming with
                | exception Not_found ->
                  Misc.fatal_errorf
                    "Missing renaming for specialised argument of a function \
                      being duplicated but not directly applied: %a -> %a.@ \
                      Closure ID being applied = %a.@ \
                      required_functions = %a.@ \
                      specialisable_renaming = %a@ \
                      specialisable_args_with_aliases = %a@ \
                      Original function declarations = %a@ \
                      Filtered function declarations = %a@ \
                      Original specialised args = %a"
                    Variable.print param
                    Flambda.print_specialised_to spec_to
                    Closure_id.print callee's_closure_id
                    Variable.Set.print required_functions
                    (Variable.Map.print Flambda.print_specialised_to)
                      specialisable_renaming
                    (Variable.Map.print Variable.print)
                      specialisable_args_with_aliases
                    Flambda.Function_declarations.print original_function_decls
                    Flambda.Function_declarations.print function_decls
                    (Variable.Map.print Flambda.print_specialised_to)
                      specialised_args
                | argument_from_the_current_application ->
                  Some argument_from_the_current_application
              else
                None
            | None ->
              Misc.fatal_errorf "No equality to variable for specialised arg %a"
                Variable.print param)
        specialisable_args_with_aliases specialised_args
    in
    let functions'_specialised_params =
      Flambda_utils.parameters_specialised_to_the_same_variable
        ~function_decls
        ~specialised_args:specialisable_args
    in
    let rewrite_function (fun_decl:Flambda.Function_declaration.t) =
      (* First rewrite every use of the closure(s) defined by the current set
         of closures to free variable(s) corresponding to the original
         (non-specialised) closure(s).

         Then for each call to such closures, if the arguments to the call are
         obviously the same as the arguments to which we are specialising the
         function, redirect the call to the specialised function.

         In a function like [List.map]:
         {[
           let rec specialised_map f l =
             match l with
             | [] -> []
             | h :: t -> f h :: specialised_map f t
         ]} ( with [f] a specialised argument )

         The first step turns it into:
         {[
           let map_original = map in
           let rec specialised_map f l =
             match l with
             | [] -> []
             | h :: t -> f h :: map_original f t
         ]}
         and the second recognizes the call to [map_original] as a call
         preserving the specialised arguments (here [f]). So it is
         replaced by [specialised_map f t].

         In the case of [map] this is a circuituous means of achieving the
         desired result, but in general, this provides a way of handling
         situations where some recursive calls (for example in subfunctions)
         are made with arguments different from the specialised arguments.
         The two-pass approach is convenient since the first pass performs
         a correct code transformation without optimisation; and then the
         second just performs the optimisation on a best-effort basis.
      *)
      let body_substituted =
        (* The use of [Freshening.rewrite_recursive_calls_with_symbols] above
           ensures that we catch all calls to the functions being defined
           in the current set of closures. *)
        Flambda.Expr.toplevel_substitution original_vars fun_decl.body
      in
      let body =
        Flambda.Expr.Mappers.Toplevel_only.map_expr (fun (expr : Flambda.Expr.t) ->
            match expr with
            | Apply apply ->
              begin match apply.call_kind with
              | Indirect -> expr
              | Direct { closure_id; return_arity; } ->
                (* We recognize the potential recursive calls using the
                   closure ID rather than [apply.func] because the latter can be
                   aliases to the function (through a symbol for instance; the
                   fact that we've now rewritten such symbols to variables
                   doesn't squash any aliases) rather than being the closure var
                   directly. *)
                let closure_var = Closure_id.unwrap closure_id in
                begin match
                  Variable.Map.find closure_var functions'_specialised_params
                with
                | exception Not_found -> expr
                | specialised_params ->
                  (* This is a call to one of the functions from the set being
                     specialised. *)
                  let apply_is_preserving_specialised_args =
                    List.length apply.args = List.length specialised_params
                      && List.for_all2 (fun arg param ->
                          match
                            (arg : Flambda_utils.specialised_to_same_as)
                          with
                          | Not_specialised -> true
                          | Specialised_and_aliased_to args ->
                            (* This is using one of the aliases of [param]. This
                               is not necessarily the exact same variable as
                               the original parameter---in particular when the
                               set contains multiply-recursive functions. *)
                            Variable.Set.mem param args)
                        specialised_params
                        apply.args
                  in
                  if apply_is_preserving_specialised_args then
                    Flambda.Apply
                      { apply with
                        func = closure_var;
                        call_kind = Direct { closure_id; return_arity; };
                      }
                  else
                    expr
                end
              end
            | _ -> expr)
          body_substituted
      in
      Flambda.Function_declaration.update_body fun_decl ~body
    in
    let funs =
      Variable.Map.map rewrite_function function_decls.funs
    in
    let function_decls =
      Flambda.Function_declarations.update ~funs function_decls
    in
    let set_of_closures =
      (* This is the new set of closures, with more precise specialisation
         information than the one being copied. *)
      Flambda.Set_of_closures.create ~function_decls ~free_vars
        ~specialised_args:specialisable_args
        ~direct_call_surrogates
    in
    (* Generate a copy of the function application, including the function
       declaration(s), but with variables (not yet bound) in place of the
       arguments. *)
    let duplicated_application : Flambda.Expr.t =
      let project_closure : Projection.Project_closure.t =
        { set_of_closures = set_of_closures_var;
          closure_id = Closure_id.Set.singleton callee's_closure_id;
        }
      in
      let func = new_var "dup_func" in
      let body : Flambda.Expr.t =
        Flambda.Expr.create_let set_of_closures_var
          (Set_of_closures set_of_closures)
          (Flambda.Expr.create_let func (Project_closure project_closure)
            (Apply {
              kind = Function;
              continuation;
              func;
              args;
              call_kind = Direct {
                closure_id = callee's_closure_id;
                return_arity = function_decl.return_arity;
              };
              dbg;
              inline = inline_requested;
              specialise = Default_specialise;
            }))
      in
      Flambda.Expr.bind ~bindings:free_vars_for_lets ~body
    in
    let env = E.activate_freshening (E.set_never_inline env) in
    Some (simplify env r duplicated_application)
*)
