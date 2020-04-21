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

(* CR mshinwell: Unused closure variables should be deleted prior to
   simplification of sets of closures, taking the used-var-in-closures
   set from the previous round. *)

let function_decl_type denv function_decl ?code_id rec_info =
  let decision =
    Inlining_decision.make_decision_for_function_declaration
      denv function_decl
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

module Context_for_multiple_sets_of_closures : sig
  (* This module deals with a sub-problem of the problem of simplifying multiple
     possibly-recursive sets of closures, namely determining typing and
     contextual information that is the same no matter which set of closures in
     a given recursive group is being simplified. *)

  type t

  val create
     : dacc_prior_to_sets:DA.t
    -> all_sets_of_closures:Set_of_closures.t list
    -> closure_bound_names_all_sets:Name_in_binding_pos.t Closure_id.Map.t list
    -> closure_element_types_all_sets:T.t Var_within_closure.Map.t list
    -> t

  val dacc_inside_functions : t -> DA.t

  val dacc_prior_to_sets : t -> DA.t

  val old_to_new_code_ids_all_sets : t -> Code_id.t Code_id.Map.t

  val new_to_old_code_ids_all_sets : t -> Code_id.t Code_id.Map.t

  val closure_bound_names_inside_functions_all_sets
     : t
    -> Name_in_binding_pos.t Closure_id.Map.t list
end = struct
  type t = {
    dacc_prior_to_sets : DA.t;
    dacc_inside_functions : DA.t;
    closure_bound_names_inside_functions_all_sets
      : Name_in_binding_pos.t Closure_id.Map.t list;
    old_to_new_code_ids_all_sets : Code_id.t Code_id.Map.t;
  }

  let dacc_prior_to_sets t = t.dacc_prior_to_sets
  let dacc_inside_functions t = t.dacc_inside_functions

  let old_to_new_code_ids_all_sets t = t.old_to_new_code_ids_all_sets

  let new_to_old_code_ids_all_sets t =
    Code_id.invert_map (old_to_new_code_ids_all_sets t)

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
          TE.add_env_extension env_inside_function env_extension
        in
        let types_inside_function =
          Var_within_closure.Map.add clos_var
            (T.alias_type_of K.value (Simple.var var))
            types_inside_function
        in
        env_inside_function, types_inside_function)
      closure_element_types
      (env_inside_function, Var_within_closure.Map.empty)

  let compute_closure_types_inside_functions ~denv
       ~all_sets_of_closures ~closure_bound_names_all_sets
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
      List.map (fun closure_bound_names_inside ->
          Closure_id.Map.map (fun name ->
              T.alias_type_of K.value (Name_in_binding_pos.to_simple name))
            closure_bound_names_inside)
        closure_bound_names_all_sets_inside
    in
    let closure_types_inside_functions =
      List.map2
        (fun set_of_closures
             (closure_types_via_aliases,
              closure_element_types_inside_function) ->
          let function_decls = Set_of_closures.function_decls set_of_closures in
          let all_function_decls_in_set =
            (* CR mshinwell: [Rec_info] may be wrong. *)
            Closure_id.Map.map (fun function_decl ->
                let new_code_id =
                  Code_id.Map.find (FD.code_id function_decl)
                    old_to_new_code_ids_all_sets
                in
                function_decl_type denv function_decl
                  ~code_id:new_code_id
                  (Rec_info.create ~depth:1 ~unroll_to:None))
              (Function_declarations.funs function_decls)
          in
          Closure_id.Map.mapi (fun closure_id _function_decl ->
              T.exactly_this_closure closure_id
                ~all_function_decls_in_set
                ~all_closures_in_set:closure_types_via_aliases
                ~all_closure_vars_in_set:closure_element_types_inside_function)
            all_function_decls_in_set)
        all_sets_of_closures
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
        denv_inside_functions
        closure_bound_names_inside_functions_all_sets
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

  let compute_old_to_new_code_ids_all_sets ~all_sets_of_closures =
    List.fold_left
      (fun old_to_new_code_ids_all_sets set_of_closures ->
        let function_decls = Set_of_closures.function_decls set_of_closures in
        Closure_id.Map.fold (fun _ function_decl old_to_new_code_ids ->
            let old_code_id = FD.code_id function_decl in
            let new_code_id = Code_id.rename old_code_id in
            Code_id.Map.add old_code_id new_code_id old_to_new_code_ids)
          (Function_declarations.funs function_decls)
          old_to_new_code_ids_all_sets)
      Code_id.Map.empty
      all_sets_of_closures

  let bind_existing_code_to_new_code_ids denv ~old_to_new_code_ids_all_sets =
    Code_id.Map.fold (fun old_code_id new_code_id denv ->
        let params_and_body = DE.find_code denv old_code_id in
        DE.define_code denv ~code_id:new_code_id ~newer_version_of:old_code_id
          ~params_and_body)
      old_to_new_code_ids_all_sets
      denv

  let create ~dacc_prior_to_sets ~all_sets_of_closures
        ~closure_bound_names_all_sets ~closure_element_types_all_sets =
    let denv = DA.denv dacc_prior_to_sets in
    let denv_inside_functions =
      denv
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
            compute_closure_element_types_inside_function
              ~env_prior_to_sets:(DE.typing_env denv)
              ~env_inside_function:env_inside_functions ~closure_element_types
          in
          env_inside_functions,
            closure_element_types_inside_function
              :: closure_element_types_all_sets_inside_functions_rev)
        (DE.typing_env denv_inside_functions, [])
        closure_element_types_all_sets
    in
    let closure_element_types_inside_functions_all_sets =
      List.rev closure_element_types_all_sets_inside_functions_rev
    in
    let old_to_new_code_ids_all_sets =
      compute_old_to_new_code_ids_all_sets ~all_sets_of_closures
    in
    let closure_bound_names_inside_functions_all_sets,
        closure_types_inside_functions_all_sets =
      compute_closure_types_inside_functions ~denv
        ~all_sets_of_closures ~closure_bound_names_all_sets
        ~closure_element_types_inside_functions_all_sets
        ~old_to_new_code_ids_all_sets
    in
    let dacc_inside_functions =
      env_inside_functions
      |> DE.with_typing_env denv_inside_functions
      |> bind_existing_code_to_new_code_ids ~old_to_new_code_ids_all_sets
      |> bind_closure_types_inside_functions
           ~closure_bound_names_inside_functions_all_sets
           ~closure_types_inside_functions_all_sets
      |> DA.with_denv dacc_prior_to_sets
    in
    { dacc_prior_to_sets;
      dacc_inside_functions;
      closure_bound_names_inside_functions_all_sets;
      old_to_new_code_ids_all_sets;
    }
end

module C = Context_for_multiple_sets_of_closures

let dacc_inside_function context r ~params ~my_closure closure_id
      ~closure_bound_names_inside_function =
  let dacc =
    DA.map_denv (C.dacc_inside_functions context) ~f:(fun denv ->
      let denv = DE.add_parameters_with_unknown_types denv params in
      match
        Closure_id.Map.find closure_id closure_bound_names_inside_function
      with
      | exception Not_found ->
        Misc.fatal_errorf "No closure name for closure ID %a.@ \
            closure_bound_names_inside_function = %a."
          Closure_id.print closure_id
          (Closure_id.Map.print Name_in_binding_pos.print)
          closure_bound_names_inside_function
      | name ->
        let name = Name_in_binding_pos.name name in
        DE.add_variable denv
          (Var_in_binding_pos.create my_closure NM.normal)
          (T.alias_type_of K.value (Simple.name name)))
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      Closure_id.Map.fold (fun _closure_id bound_name denv ->
          Name.pattern_match (Name_in_binding_pos.to_name bound_name)
            ~var:(fun _var -> denv)
            ~symbol:(fun closure_symbol ->
              DE.now_defining_symbol denv closure_symbol))
        closure_bound_names_inside_function
        denv)
  in
  DA.with_r dacc r

type simplify_function_result = {
  function_decl : FD.t;
  new_code_id : Code_id.t;
  params_and_body : Function_params_and_body.t;
  function_type : T.Function_declaration_type.t;
}

(* The traversal order for a set of closures binding (either [Let] or
   [Let_symbol]) is as follows:

   1. Downwards traversal of the code of the function(s) named in the
      binding.
   2. Upwards traversal of the code of the function(s), rebuilding the
      body/bodies.
   3. Calculation of the type(s) of the function(s), including the inlining
      decision.
   4. Downwards traversal of the expression subsequent to the set of
      closures binding.
   5. Upwards traversal of the same.

   We cannot do the downwards traversal of the expression subsequent to the
   set of closures before the upwards rebuilding of the functions' bodies,
   since we need the simplified versions of the bodies to compute the
   function's types, which are in turn required in the environment for such
   downwards traversal.

   As a result of this ordering we can only guarantee to have seen all uses
   of a closure variable named in a set of closures definition if that
   definition is not itself under a lambda.  It follows that closure variable
   removal is only performed in these circumstances.  (Set-of-closures
   definitions that are lifted will have their variables removed, as by
   the time they are placed, we are never under a lambda.)
*)

(* XXX We need to make sure the placement code for [Let_symbol] actually
   does the removal.  In fact is that the only place that should do it? *)

let simplify_function context r closure_id function_decl
      ~closure_bound_names_inside_function code_age_relation
      ~after_traversal =
  let old_code_id = FD.code_id function_decl in
  let params_and_body =
    DE.find_code (DA.denv (C.dacc_prior_to_sets context)) old_code_id
  in
  Function_params_and_body.pattern_match params_and_body
    ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure ->
      let dacc =
        dacc_inside_function context r ~params ~my_closure closure_id
          ~closure_bound_names_inside_function
      in
      let dacc =
        DA.map_denv dacc ~f:(fun denv ->
          denv
          |> DE.map_typing_env ~f:(fun typing_env ->
            (* CR mshinwell: Tidy up propagation to avoid union *)
            TE.code_age_relation typing_env
            |> Code_age_relation.union code_age_relation
            |> TE.with_code_age_relation typing_env)
          |> DE.add_lifted_constants ~lifted:(R.get_lifted_constants r))
      in
      Simplify_toplevel.simplify_toplevel dacc body
        ~return_continuation
        ~return_arity:(FD.result_arity function_decl)
        exn_continuation
        ~return_cont_scope:Scope.initial
        ~exn_cont_scope:(Scope.next Scope.initial)
        ~after_traversal:(fun dacc_after_body ~rebuild:rebuild_body ->
          let code_age_relation = DA.code_age_relation dacc_after_body in
          after_traversal ~dacc_after_body ~code_age_relation
            ~rebuild:(fun uacc ~after_rebuild ->
              rebuild_body uacc ~after_rebuild:(fun body uacc ->
                let dbg = Function_params_and_body.debuginfo params_and_body in
                (* CR mshinwell: Should probably look at [cont_uses]? *)
                let params_and_body =
                  Function_params_and_body.create ~return_continuation
                    exn_continuation params ~dbg ~body ~my_closure
                in
                let new_code_id =
                  C.old_to_new_code_ids_all_sets context
                  |> Code_id.Map.find old_code_id
                in
                let function_decl =
                  FD.update_code_id function_decl new_code_id
                in
                let function_type =
                  (* We need to use [dacc_after_body] to ensure that all
                     [code_ids] in [function_decl] are available for the
                     inlining decision code called by [function_decl_type]. *)
                  function_decl_type (DA.denv dacc_after_body) function_decl
                    Rec_info.initial
                in
                let result =
                  { function_decl;
                    new_code_id;
                    params_and_body;
                    function_type;
                  }
                in
                after_rebuild result uacc))))

type simplify_set_of_closures0_result = {
  set_of_closures : Flambda.Set_of_closures.t;
  code : Flambda.Function_params_and_body.t Code_id.Map.t;
  dacc : Downwards_acc.t;
}

let rebuild_set_of_closures dacc ~closure_bound_names ~fun_types
      ~closure_elements ~closure_element_types ~prior_lifted_constants
      ~all_function_decls_in_set code code_age_relation r
      ~used_closure_vars ~after_rebuild =
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
      |> DE.map_typing_env ~f:(fun typing_env ->
        TE.with_code_age_relation typing_env code_age_relation)
      |> Closure_id.Map.fold (fun _closure_id bound_name denv ->
            DE.define_name_if_undefined denv bound_name K.value)
          closure_bound_names
      |> DE.add_lifted_constants ~lifted:(R.get_lifted_constants r)
      |> Name_in_binding_pos.Map.fold (fun bound_name closure_type denv ->
            let bound_name = Name_in_binding_pos.to_name bound_name in
            DE.add_equation_on_name denv bound_name closure_type)
          closure_types_by_bound_name)
  in
  let dacc =
    DA.with_r dacc
      (R.add_prior_lifted_constants (DA.r dacc) prior_lifted_constants)
  in
  let set_of_closures =
    let closure_elements =
      let used_closure_vars = R.used_closure_vars (UA.r uacc) in
      Var_within_closure.Map.filter (fun closure_var ->
          Var_within_closure.Set.mem closure_var used_closure_vars)
        closure_elements
    in
    Function_declarations.create all_function_decls_in_set
    |> Set_of_closures.create ~closure_elements
  in
  after_rebuild code set_of_closures uacc

(* CR mshinwell: Take [dacc] from [C.dacc_prior_to_sets]? *)
let simplify_set_of_closures0 dacc context set_of_closures
      ~closure_bound_names ~closure_bound_names_inside ~closure_elements
      ~closure_element_types ~after_traversal ~after_rebuild =
  let r, prior_lifted_constants =
    R.get_and_clear_lifted_constants (DA.r dacc)
  in
  let dacc = DA.with_r dacc r in
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let rec simplify_functions ~all_function_decls_in_set
        code_age_relation r ~rebuild_prev_function =
    (* CR-someday mshinwell: Add [Map.choose_and_remove] *)
    match Closure_id.Map.choose all_function_decls_in_set with
    | (closure_id, function_decl) ->
      simplify_function context r closure_id function_decl
        ~closure_bound_names_inside_function:closure_bound_names_inside
        code_age_relation
        ~after_traversal:(fun ~dacc_after_body ~code_age_relation ~rebuild ->
          let all_function_decls_in_set =
            Closure_id.Map.remove closure_id all_function_decls_in_set
          in
          simplify_functions ~all_function_decls_in_set
            code_age_relation (DA.r dacc_after_body)
            ~rebuild_prev_function:(fun ~all_function_decls_in_set ~code
                  ~fun_types uacc ~after_rebuild ->
              rebuild uacc ~after_rebuild:(fun result uacc ->
                let result_function_decls_in_set =
                  Closure_id.Map.add closure_id result.function_decl
                    result_function_decls_in_set
                in
                let code =
                  Code_id.Map.add new_code_id result.params_and_body code
                in
                let fun_types =
                  Closure_id.Map.add closure_id result.function_type fun_types
                in
                rebuild_prev_function ~all_function_decls_in_set ~code
                  ~fun_types uacc ~after_rebuild)))
    | exception Not_found ->
      after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
        rebuild_prev_function ~all_function_decls_in_set:Closure_id.Map.empty
          ~code:Code_id.Map.empty
          ~fun_types:Closure_id.Map.empty
          uacc ~after_rebuild)
  in
  simplify_functions
    ~all_function_decls_in_set:(Function_declaration.funs function_decls)
    (DA.code_age_relation dacc) (DA.r dacc)
    ~rebuild_prev_function:after_rebuild

(* XXX Need to call rebuild_set_of_closures here *)

let simplify_and_lift_set_of_closures dacc ~closure_bound_vars_inverse
      ~closure_bound_vars set_of_closures ~closure_elements
      ~after_traversal =
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
  let closure_bound_names =
    Closure_id.Map.map Name_in_binding_pos.symbol closure_symbols
  in
  let closure_element_types =
    Var_within_closure.Map.map (fun closure_element ->
        Simple.pattern_match closure_element
          ~const:(fun _ -> T.alias_type_of K.value closure_element)
          ~name:(fun name ->
            Name.pattern_match name
              ~var:(fun var ->
                match Variable.Map.find var closure_bound_vars_inverse with
                | exception Not_found ->
                  assert (DE.mem_variable (DA.denv dacc) var);
                  T.alias_type_of K.value closure_element
                | closure_id ->
                  let closure_symbol =
                    Closure_id.Map.find closure_id closure_symbols
                  in
                  T.alias_type_of K.value (Simple.symbol closure_symbol))
              ~symbol:(fun _sym -> T.alias_type_of K.value closure_element)))
      closure_elements
  in
  let context =
    C.create ~dacc_prior_to_sets:dacc
      ~all_sets_of_closures:[set_of_closures]
      ~closure_bound_names_all_sets:[closure_bound_names]
      ~closure_element_types_all_sets:[closure_element_types]
  in
  let closure_bound_names_inside =
    match C.closure_bound_names_inside_functions_all_sets context with
    | [closure_bound_names_inside] -> closure_bound_names_inside
    | _ -> assert false
  in
  simplify_set_of_closures0 dacc context set_of_closures
    ~closure_bound_names ~closure_bound_names_inside ~closure_elements
    ~closure_element_types
    ~after_traversal:(fun code dacc ~rebuild ->
      let closure_symbols_set =
        Symbol.Set.of_list (Closure_id.Map.data closure_symbols)
      in
      assert (Symbol.Set.cardinal closure_symbols_set
        = Closure_id.Map.cardinal closure_symbols);
      let types_of_symbols =
        Symbol.Set.fold (fun symbol types_of_symbols ->
            let typ = DE.find_symbol (DA.denv dacc_before_body) symbol in
            Symbol.Map.add symbol typ types_of_symbols)
          closure_symbols_set
          Symbol.Map.empty
      in
      let bound_symbols : Bound_symbols.t =
        Sets_of_closures [{
          code_ids = Code_id.Map.keys code;
          closure_symbols;
        }]
      in
      let static_const : SC.t =
        let code =
          Code_id.Map.mapi (fun code_id params_and_body : SC.Code.t ->
              let newer_version_of =
                Code_id.Map.find code_id
                  (C.new_to_old_code_ids_all_sets context)
              in
              { params_and_body = Present params_and_body;
                newer_version_of = Some newer_version_of;
              })
            code
        in
        Sets_of_closures [{
          code;
          (* CR mshinwell: This should probably be using [set_of_closures]
             with the closure elements updated. *)
          set_of_closures;
        }]
      in
      let set_of_closures_lifted_constant =
        Lifted_constant.create (DA.denv dacc_before_body) bound_symbols
          static_const ~types_of_symbols:(Types types_of_symbols)
      in
      let r =
        R.new_lifted_constant (DA.r dacc) set_of_closures_lifted_constant
      in
      let denv =
        DE.add_lifted_constants (DA.denv dacc)
          ~lifted:[set_of_closures_lifted_constant]
      in
      let denv =
        Closure_id.Map.fold (fun closure_id bound_var denv ->
            match Closure_id.Map.find closure_id closure_symbols with
            | exception Not_found ->
              Misc.fatal_errorf "No closure symbol for closure ID %a"
                Closure_id.print closure_id
            | closure_symbol ->
              let simple = Simple.symbol closure_symbol in
              let typ = T.alias_type_of K.value simple in
              DE.add_variable denv bound_var typ)
          closure_bound_vars
          denv
      in
      let dacc = DA.with_denv (DA.with_r dacc r) denv in
      after_traversal dacc ~rebuild)
    ~after_rebuild:(fun ~all_function_decls_in_set ~code ~fun_types uacc ->
      let uacc =
        (* If we have removed closure vars, we need to update the lifted
           constant in [r].  We don't need to provide updated types for such
           constant as we are now on the upwards traversal; the typing
           information was used on the downwards traversal.
           Note that out of the various cases of simplifying sets of
           closures (non-lifted remaining so, non-lifted being lifted, and
           already lifted) it is only the "non-lifted being lifted" case
           where we have to do this.  In the other cases no lifted constants
           are involved, either because no lifting is happening, or because
           an existing [Let_symbol] binding is being simplified and the
           result of the simplification will be placed there.  (Unlike in the
           case of lifting a previously non-lifted set, where a lifted constant
           must be generated, so the definition can be floated up and placed
           at the nearest appropriate [Let_symbol] binding.) *)
        if Set_of_closures.same_closure_vars set_of_closures new_set_of_closures
        then uacc
        else
          (* CR mshinwell: share computation of bound_symbols and static_const
             with above *)
          let bound_symbols : Bound_symbols.t =
            Sets_of_closures [{
              code_ids = Code_id.Map.keys code;
              closure_symbols;
            }]
          in
          let static_const : SC.t =
            let code =
              Code_id.Map.mapi (fun code_id params_and_body : SC.Code.t ->
                  let newer_version_of =
                    Code_id.Map.find code_id
                      (C.new_to_old_code_ids_all_sets context)
                  in
                  { params_and_body = Present params_and_body;
                    newer_version_of = Some newer_version_of;
                  })
                code
            in
            Sets_of_closures [{
              code;
              set_of_closures = new_set_of_closures;
            }]
          in
          let set_of_closures_lifted_constant =
            Lifted_constant.create bound_symbols static_const
              ~types_of_symbols:Unavailable_on_upwards_traversal
          in
          UA.map_r uacc ~f:(fun r ->
            R.replace_lifted_constant r bound_symbols static_const)
      in
      let bindings =
        Closure_id.Map.fold (fun closure_id bound_var bindings ->
            match Closure_id.Map.find closure_id closure_symbols with
            | exception Not_found ->
              Misc.fatal_errorf "No closure symbol for closure ID %a"
                Closure_id.print closure_id
            | closure_symbol ->
              let simple = Simple.symbol closure_symbol in
              let defining_expr = Named.create_simple simple in
              let bound_var = Bindable_let_bound.singleton bound_var in
              (bound_var, Reachable.reachable defining_expr) :: bindings)
          closure_bound_vars
          []
      in
      after_rebuild bindings body user_data uacc)

let simplify_non_lifted_set_of_closures0 dacc ~bound_vars ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types
      ~after_traversal =
  let closure_bound_names =
    Closure_id.Map.map Name_in_binding_pos.var closure_bound_vars
  in
  let context =
    C.create ~dacc_prior_to_sets:dacc
      ~all_sets_of_closures:[set_of_closures]
      ~closure_bound_names_all_sets:[closure_bound_names]
      ~closure_element_types_all_sets:[closure_element_types]
  in
  let closure_bound_names_inside =
    (* CR mshinwell: Share with previous function *)
    match C.closure_bound_names_inside_functions_all_sets context with
    | [closure_bound_names_inside] -> closure_bound_names_inside
    | _ -> assert false
  in
  simplify_set_of_closures0 (C.dacc_prior_to_sets context) context
    set_of_closures ~closure_bound_names ~closure_bound_names_inside
    ~closure_elements ~closure_element_types
    ~after_traversal:(fun code dacc ~rebuild ->
      (* CR mshinwell: This next part should probably be shared between the
         lifted and non-lifted cases; we always need the new code in the
         environment and [r]. *)
      let lifted_constant =
        Lifted_constant.create_pieces_of_code (DA.denv dacc)
          code ~newer_versions_of:(C.new_to_old_code_ids_all_sets context)
      in
      let dacc =
        DA.map_r dacc ~f:(fun r -> R.new_lifted_constant r lifted_constant)
      in
      let dacc =
        DA.map_denv dacc ~f:(fun denv ->
          DE.add_lifted_constants denv ~lifted:[lifted_constant])
      in
      after_traversal dacc ~rebuild)
    ~after_rebuild:(fun _code set_of_closures uacc ->
      let defining_expr =
        Reachable.reachable (Named.create_set_of_closures set_of_closures)
      in
      after_rebuild [bound_vars, defining_expr] body uacc)

type lifting_decision_result = {
  can_lift : bool;
  closure_elements : Simple.t Var_within_closure.Map.t;
  closure_element_types : T.t Var_within_closure.Map.t;
}

let type_closure_elements_and_make_lifting_decision_for_one_set dacc
      ~min_name_mode ~closure_bound_vars_inverse set_of_closures =
  (* By computing the types of the closure elements, attempt to show that
     the set of closures can be lifted, and hence statically allocated.
     At present we make the conservative assumption that simplifying the
     function bodies will not affect their eligibility for lifting. *)
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
  (* Note that [closure_bound_vars_inverse] doesn't need to include
     variables binding closures in other mutually-recursive sets, since if
     we get here in the case where we are considering lifting a set that has
     not been lifted before, there are never any other mutually-recursive
     sets ([Named.t] does not allow them). *)
  let can_lift =
    Var_within_closure.Map.for_all (fun _ simple ->
        Simple.pattern_match simple
          ~const:(fun _ -> true)
          ~name:(fun name ->
            Name.pattern_match name
              ~var:(fun var -> Variable.Map.mem var closure_bound_vars_inverse)
              ~symbol:(fun _sym -> true)))
      closure_elements
  in
  { can_lift;
    closure_elements;
    closure_element_types;
  }

let type_closure_elements_for_previously_lifted_set dacc
      ~min_name_mode set_of_closures =
  (* N.B. The returned [can_lift] might not be [true], since the closure
     variables might actually assigned to be [Variable]s, in the case of a
     non-constant lifted closure. *)
  type_closure_elements_and_make_lifting_decision_for_one_set dacc
    ~min_name_mode ~closure_bound_vars_inverse:Variable.Map.empty
    set_of_closures

let simplify_non_lifted_set_of_closures dacc
      ~(bound_vars : Bindable_let_bound.t) set_of_closures
      ~after_traversal =
  let closure_bound_vars =
    Bindable_let_bound.must_be_set_of_closures bound_vars
  in
  (* CR mshinwell: This should probably be handled differently, but
     will require some threading through *)
  let min_name_mode =
    Bindable_let_bound.name_mode bound_vars
  in
  let closure_bound_vars_inverse =
    Closure_id.Map.fold (fun closure_id var closure_bound_vars_inverse ->
        Variable.Map.add (Var_in_binding_pos.var var) closure_id
          closure_bound_vars_inverse)
      closure_bound_vars
      Variable.Map.empty
  in
  (* CR mshinwell: [closure_element_types] is barely worth keeping *)
  let { can_lift; closure_elements; closure_element_types; } =
    type_closure_elements_and_make_lifting_decision_for_one_set dacc
      ~min_name_mode ~closure_bound_vars_inverse set_of_closures
  in
  if can_lift then
    simplify_and_lift_set_of_closures dacc ~closure_bound_vars_inverse
      ~closure_bound_vars set_of_closures ~closure_elements
      ~after_traversal ~after_rebuild
  else
    simplify_non_lifted_set_of_closures0 dacc ~bound_vars ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types
      ~after_traversal ~after_rebuild

let simplify_lifted_set_of_closures0 context ~closure_symbols
      ~closure_bound_names_inside ~closure_elements ~closure_element_types
      set_of_closures ~after_traversal =
  let closure_bound_names =
    Closure_id.Map.map Name_in_binding_pos.symbol closure_symbols
  in
  let dacc =
    DA.map_denv (C.dacc_prior_to_sets context) ~f:(fun denv ->
      Closure_id.Map.fold (fun _closure_id symbol denv ->
          DE.define_symbol_if_undefined denv symbol K.value)
        closure_symbols
        denv)
  in
  simplify_set_of_closures0 dacc context set_of_closures ~closure_bound_names
    ~closure_bound_names_inside ~closure_elements ~closure_element_types
    ~after_traversal:(fun code dacc ~rebuild ->
      let dacc =
        DA.map_denv dacc ~f:(fun denv ->
          (* CR mshinwell: factor out *)
          Code_id.Map.fold (fun code_id params_and_body denv ->
              let newer_version_of =
                Code_id.Map.find code_id
                  (C.new_to_old_code_ids_all_sets context)
              in
              DE.define_code ~newer_version_of denv ~code_id ~params_and_body)
            code
            denv)
      in
      after_traversal dacc ~rebuild)
    ~after_rebuild:(fun code set_of_closures uacc ->
      let code =
        Code_id.Map.mapi (fun code_id params_and_body : SC.Code.t ->
            let newer_version_of =
              C.new_to_old_code_ids_all_sets context
              |> Code_id.Map.find_opt code_id
            in
            { params_and_body = Present params_and_body;
              newer_version_of;
            })
          code
      in
      let bound_symbols_component : Bound_symbols.Code_and_set_of_closures.t =
        { code_ids = Code_id.Map.keys code;
          closure_symbols;
        }
      in
      let code_and_set_of_closures : SC.Code_and_set_of_closures.t =
        { code;
          set_of_closures;
        }
      in
      after_rebuild bound_symbols_component code_and_set_of_closures uacc)

let simplify_lifted_sets_of_closures dacc ~orig_bound_symbols ~orig_static_const
      (bound_symbols_components : Bound_symbols.Code_and_set_of_closures.t list)
      (code_and_sets_of_closures : SC.Code_and_set_of_closures.t list)
      ~after_traversal =
  if List.compare_lengths bound_symbols_components code_and_sets_of_closures
       <> 0
  then begin
    Misc.fatal_errorf "Differing number of bound symbols and static constant \
        set-of-closures definitions for@ %a@ =@ %a"
      Bound_symbols.print orig_bound_symbols
      SC.print orig_static_const
  end;
  let dacc =
    (* Unlike in the cases above that start from [Let]-bindings, in this case
       the code may be in the same definition as the closure(s), so we must
       also add such code to the environment.  (See [Static_const].) *)
    List.fold_left2
      (fun dacc
           ({ code_ids; closure_symbols = _; }
             : Bound_symbols.Code_and_set_of_closures.t)
           ({ code; set_of_closures = _; }
             : SC.Code_and_set_of_closures.t) ->
        (* CR mshinwell: Check closure IDs between [closure_symbols] and
           [set_of_closures] too. *)
        let code_ids' = Code_id.Map.keys code in
        if not (Code_id.Set.equal code_ids code_ids') then begin
          Misc.fatal_errorf "Mismatch on declared code IDs (%a and %a):@ %a"
            Code_id.Set.print code_ids
            Code_id.Set.print code_ids'
            SC.print orig_static_const
        end;
        Code_id.Map.fold
          (fun code_id ({ params_and_body; newer_version_of; } : SC.Code.t)
               dacc ->
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
    List.map
      (fun ({ set_of_closures; _ } : SC.Code_and_set_of_closures.t) ->
        set_of_closures)
      code_and_sets_of_closures
  in
  let closure_bound_names_all_sets =
    List.map
      (fun ({ code_ids = _; closure_symbols; }
             : Bound_symbols.Code_and_set_of_closures.t) ->
        Closure_id.Map.map Name_in_binding_pos.symbol closure_symbols)
      bound_symbols_components
  in
  let closure_elements_and_types_all_sets =
    List.map
      (fun ({ code = _; set_of_closures; } : SC.Code_and_set_of_closures.t) ->
        let { can_lift = _;
              closure_elements;
              closure_element_types;
            } =
          type_closure_elements_for_previously_lifted_set
            dacc ~min_name_mode:Name_mode.normal set_of_closures
        in
        closure_elements, closure_element_types)
      code_and_sets_of_closures
  in
  let closure_element_types_all_sets =
    List.map snd closure_elements_and_types_all_sets
  in
  let context =
    C.create
      ~dacc_prior_to_sets:dacc
      ~all_sets_of_closures:sets_of_closures
      ~closure_bound_names_all_sets
      ~closure_element_types_all_sets
  in
  let closure_bound_names_inside_all_sets =
    (* CR mshinwell: make naming consistent *)
    C.closure_bound_names_inside_functions_all_sets context
  in
  (* The pattern of recursion in [simplify_sets] is analogous to that in
     [simplify_functions], above. *)
  let rec simplify_sets dacc
      ~bound_symbols_components
      ~code_and_sets_of_closures
      ~closure_bound_names_inside_all_sets
      ~closure_elements_and_types_all_sets
      ~rebuild_prev_set =
    match bound_symbols_components, code_and_sets_of_closures,
      closure_bound_names_inside_all_sets, closure_elements_and_types_all_sets
    with
    | (({ code_ids = _; closure_symbols; } as bound_symbol_component)
          : Bound_symbols.Code_and_set_of_closures.t)
        :: bound_symbols_components,
      (({ code = _; set_of_closures; } as code_and_set_of_closures)
          : SC.Code_and_set_of_closures.t)
        :: code_and_sets_of_closures,
      closure_bound_names_inside :: closure_bound_names_inside_all_sets,
      (closure_elements, closure_element_types)
        :: closure_elements_and_types_all_sets ->
      if Set_of_closures.is_empty set_of_closures then begin
        (* We don't currently simplify code on the way down.  [Un_cps] will
           however check the code to ensure there are no unbound names. *)
        simplify_sets dacc
          ~bound_symbols_components
          ~code_and_sets_of_closures
          ~closure_bound_names_inside_all_sets
          ~closure_elements_and_types_all_sets
          ~rebuild_prev_set
      end else begin
        simplify_lifted_set_of_closures0 context ~closure_symbols
          ~closure_bound_names_inside ~closure_elements
          ~closure_element_types set_of_closures
          ~after_traversal:(fun bound_symbols_component
              code_and_set_of_closures dacc ~rebuild ->
            let result_bound_symbols_components_rev =
              bound_symbol_component :: result_bound_symbols_components_rev
            in
            let result_code_and_sets_of_closures_rev =
              code_and_set_of_closures :: result_code_and_sets_of_closures_rev
            in
            simplify_sets dacc
              ~bound_symbols_components
              ~code_and_sets_of_closures
              ~closure_bound_names_inside_all_sets
              ~closure_elements_and_types_all_sets
              ~rebuild_prev_set:rebuild)
          ~after_rebuild:(fun bound_symbols code_and_set_of_closures
              ~bound_symbols_components_rev ~code_and_sets_of_closures_rev
              uacc ->
            let bound_symbols_components_rev =
              bound_symbols :: bound_symbols_components_rev
            in
            let code_and_sets_of_closures_rev =
              code_and_set_of_closures :: code_and_sets_of_closures_rev
            in
            rebuild_prev_set ~bound_symbols_components_rev
              ~code_and_sets_of_closures_rev uacc)
      end
    | [], [], [], [] ->
      after_traversal dacc ~rebuild:(rebuild_prev_set
        ~bound_symbols_components_rev:[]
        ~code_and_sets_of_closures_rev:[])
    | _, _, _, _ -> Misc.fatal_error "Lists of incorrect lengths"
  in
  simplify_sets dacc
    ~bound_symbols_components
    ~code_and_sets_of_closures
    ~closure_bound_names_inside_all_sets
    ~closure_elements_and_types_all_sets
    ~rebuild_prev_set:(fun ~bound_symbols_components_rev
        ~code_and_sets_of_closures_rev uacc ~after_rebuild ->
      let bound_symbols : Bound_symbols.t =
        Sets_of_closures (List.rev bound_symbols_components_rev)
      in
      let static_const : SC.t =
        Sets_of_closures (List.rev code_and_sets_of_closures_rev)
      in
      after_rebuild bound_symbols static_const uacc)
