(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module E = Inline_and_simplify_aux.Env

module Definition = struct
  type t =
    | Existing_outer_var of Variable.t
    | Projection_from_existing_specialised_arg of Projectee.Var_and_projectee.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      ...

  end)
end

module What_to_specialise = struct
  type t = {
    (* [definitions] is indexed by (fun_var, group) *)
    definitions : Definition.t list Variable.Pair.Map.t;
    set_of_closures : Flambda.set_of_closures;
  }

  let create ~set_of_closures =
    { definitions = [];
      set_of_closures;
    }

  let new_specialised_arg t ~fun_var ~group ~definition =
    let key = fun_var, group in
    let definitions =
      match Variable.Pair.Map.find key t with
      | exception Not_found -> []
      | definitions -> definitions
    in
    Variable.Pair.Map.add (fun_var, group) (definition :: definitions) t
end

module type S = sig
  val pass_name : string
  val variable_suffix : string

  val precondition
     : env:Inline_and_simplify_aux.Env.t
    -> set_of_closures:Flambda.set_of_closures
    -> bool

  val what_to_specialise
     : env:Inline_and_simplify_aux.Env.t
    -> set_of_closures:Flambda.set_of_closures
    -> What_to_specialise.t
end

module Processed_what_to_specialise = struct
  type for_one_function = {
    new_projection_definitions_indexed_by_new_inner_vars
      : Definition.t Variable.Map.t;
    all_new_projections : Definition.Set.t;
    new_inner_to_new_outer_vars_indexed_by_group
      : Flambda.specialised_to Variable.Map.t Variable.Map.t;
    total_number_of_args : int;
  }

  type fun_var_and_group = {
    fun_var : Variable.t;
    group : Variable.t;
  }

  type t = {
    set_of_closures : Flambda.set_of_closures;
    existing_projections_via_specialised_args_indexed_by_fun_var
      : Projectee.Var_and_projectee.Set.t Variable.Map.t;
    new_lifted_projection_definitions
      : Projection.t Variable.Map.t;
    new_outer_vars_indexed_by_new_lifted_projection_definitions
      : Variable.t Projection.Map.t;
    functions : for_one_function Variable.Map.t;
  }

  let really_add_new_specialised_arg t ~fun_var
        ~defining_expr_in_terms_of_existing_outer_vars ~projection
        ~for_one_function =
    (* We know here that a new specialised argument must be added.  This
       needs a "new inner var" and a "new outer var".  However if there
       is already a lifted projection being introduced around the set
       of closures (corresponding to another new specialised argument),
       we should re-use its "new outer var" to avoid duplication of
       projection definitions. *)
    let new_outer_var, (t : t) =
      match
        Projection.Map.find defining_expr_in_terms_of_existing_outer_vars
          t.new_outer_vars_indexed_by_new_lifted_projection_defining_exprs
      with
      | new_outer_var -> new_outer_var, t
      | exception Not_found ->
        (* We are adding a new lifted definition: generate a fresh
           "new outer var". *)
        let new_outer_var = Variable.rename group ~suffix:"_new_outer" in
        { t with
          new_outer_vars_indexed_by_new_lifted_projection_defining_exprs =
            Variable.Map.add
              defining_expr new_outer_var
              t.new_outer_vars_indexed_by_new_lifted_projection_defining_exprs;
          new_lifted_projection_definitions =
            Variable.Map.add
              new_outer_var defining_expr
              t.new_lifted_projection_definitions;
        }
    in
    let new_inner_var = Variable.rename group ~suffix:"_new_inner" in
    let new_inner_to_new_outer_vars_indexed_by_group =
      let for_this_group =
        match
          Variable.Map.find group
            for_one_function.new_inner_to_new_outer_vars_indexed_by_group
        with
        | exception Not_found -> Variable.Map.empty
        | for_this_group -> for_this_group
      in
      let for_this_group =
        Variable.Map.add new_inner_var new_outer_var for_this_group
      in
      Variable.Map.add group for_this_group
        for_one_function.new_inner_to_new_outer_vars_indexed_by_group
    in
    let for_one_function : for_one_function =
      { for_one_function with
        new_projection_defining_exprs_indexed_by_new_inner_vars =
          Variable.Map.add new_inner_var defining_expr
            for_one_function.
              new_projection_defining_exprs_indexed_by_new_inner_vars;
        all_new_projections =
          Projectee.Var_and_projectee.Set.add projection
            for_one_function.all_new_projections;
        new_inner_to_new_outer_vars_indexed_by_group;
        total_number_of_args = for_one_function.total_number_of_args + 1;
      }
    in
    { t with
      functions = Variable.Map.add fun_var for_one_function t.functions;
    }

  let new_specialised_arg t ~fun_var ~definition =
    let for_one_function : for_one_function =
      match Variable.Map.find fun_var t.functions with
      | exception Not_found ->
        begin
          match Variable.Map.find t.set_of_closures.function_decls.funs
        with
        | exception Not_found -> assert false
        | (function_decl : Flambda.function_declaration) ->
          { new_projection_defining_exprs_indexed_by_new_inner_vars =
              Variable.Map.empty;
            all_new_projections = Projectee.Var_and_projectee.Set.empty;
            new_inner_to_new_outer_vars_indexed_by_group = Variable.Map.empty;
            (* The "+ 1" is just in case there is a closure environment
               parameter added later. *)
            total_number_of_args = List.length function_decl.params + 1;
          }
      | for_one_function -> for_one_function
    in
    (* Determine whether there already exists a specialised argument (either
       an existing one or a newly-added one) that is known to be equal to
       the one proposed to this function.  If so, we use that instead. *)
    let exists_already =
      let exists_already =
        match
          Variable.Map.find fun_var
            t.existing_projections_via_specialised_args_indexed_by_fun_var
        with
        | exception Not_found -> false
        | projections ->
          Projectee.Var_and_projectee.Set.mem projection projections
      in
      if exists_already then true
      else
        Projectee.Var_and_projectee.Set.mem projection
          for_one_function.all_new_projections
    in
    if exists_already then
      t
    else
      new_specialised_arg t ~fun_var
        ~defining_expr_in_terms_of_existing_outer_vars ~projection

  let create ~env ~(what_to_specialise : W.t) =
    let t : t =
      {
      }
    in
    (* It is important to limit the number of arguments added: if arguments
       end up being passed on the stack, tail call optimization will be
       disabled (see asmcomp/selectgen.ml).
       
       For each group of new specialised args provided by [T], either all or
       none of them will be added.  (This is to avoid the situation where we
       add extra arguments but yet fail to eliminate an original one by
       stopping part-way through the specialised args addition.) *)
    let by_group =
      Variable.Pair.Map.fold (fun (fun_var, group) definition by_group ->
          let fun_vars_and_definitions =
            match Variable.Map.find group by_group with
            | exception Not_found -> []
            | fun_vars_and_definitions -> fun_vars_and_definitions
          in
          Variable.Map.add group
            ((fun_var, definition)::fun_vars_and_definitions)
            by_group)
        Variable.Map.empty
        what_to_specialise.definitions
    in
    let module Backend = (val (E.backend env) : Backend_intf.S) in
    Variable.Map.fold (fun _group fun_vars_and_definitions t ->
        let original_t = t
        let t =
          List.fold_left (fun t (fun_var, definition) ->
              new_specialised_arg t ~fun_var ~definition)
            t
            fun_vars_and_definitions
        in
        let some_function_has_too_many_args =
          List.exists (fun (for_one_function : for_one_function) ->
              t.total_number_of_args
                > Backend.max_sensible_number_of_arguments)
            t.functions
        in
        if some_function_has_too_many_args then
          original_t  (* drop this group *)
        else
          t)
      t
      by_group
end

module P = Processed_what_to_specialise
module W = What_to_specialise

module Make (T : S) = struct
  let () = Pass_wrapper.register ~pass_name:T.pass_name

  let create_wrapper ~fun_var ~(set_of_closures : Flambda.set_of_closures)
      ~(function_decl : Flambda.function_declaration)
      ~new_specialised_args_indexed_by_new_outer_vars
      ~new_inner_to_new_outer_vars =
    let new_fun_var = Variable.rename fun_var ~append:T.variable_suffix in
    (* To avoid increasing the free variables of the wrapper, for
       general cleanliness, we restate the definitions of the
       newly-specialised arguments in the wrapper itself in terms of the
       original specialised arguments.  The variables bound to these
       definitions are called the "specialised args bound in the wrapper".
       Note that the domain of [params_renaming] is a (non-strict) superset
       of the "inner vars" of the original specialised args. *)
    let params = Variable.Set.of_list function_decl.params in
    let params_renaming =
      Variable.Map.of_list
        (List.map (fun param ->
            let new_param = Variable.rename param ~append:T.variable_suffix in
            param, new_param)
          function_decl.params)
    in
    let wrapper_params =
      List.map (fun param -> Variable.Map.find param params_renaming)
        function_decl.params
    in
    (*  1. Renaming of existing specialised arguments: these form the
        parameters of the wrapper.

        Existing specialised    set_of_closures.        Existing outer
        arguments of the        -------------------->   specialised arguments
        main function             spec_args             of the main function

                                                                |
                                                 existing_outer |
                 +                               _vars_to_      |
                                                 wrapper_params |
                                                 _renaming      v

        Other parameters of     -------------------->   Parameters of the
        the main function          params_renaming      wrapper, some of
                                                        which will be
                                                        specialised args
    *)
    (* CR mshinwell: update comment to reflect the fact that we need to
       also apply the inverse free_vars mapping, since we're pushing
       expressions down into the function body. *)
    let existing_outer_vars_to_wrapper_params_renaming =
      let existing_specialised_args_inverse =
        let specialised_args =
          (* There might be more than one specialised arg coming from
             the same existing outer variable, but that doesn't matter.
             What does matter is that this situation might occur across
             functions (one arg in one function and one arg in another
             both specialised to the same thing), and we must choose the
             correct arg in that case. *)
          (* CR-soon mshinwell: Maybe this nonsense could be improved if
             [Extract_projections] didn't rewrite the definitions to use
             the outer variables? *)
          Variable.Map.filter_map set_of_closures.specialised_args
            ~f:(fun inner_var (spec_to : Flambda.specialised_to) ->
              if Variable.Set.mem inner_var params then Some spec_to.var
              else None)
        in
        Variable.Map.transpose_keys_and_data specialised_args
      in
      let existing_free_vars_inverse =
        let free_vars =
          Variable.Map.map (fun (spec_to : Flambda.specialised_to) ->
              spec_to.var)
            set_of_closures.free_vars
        in
        Variable.Map.transpose_keys_and_data free_vars
      in
      let for_spec_args =
        Variable.Map.filter_map existing_specialised_args_inverse
          ~f:(fun _existing_outer_var existing_inner_var ->
            match Variable.Map.find existing_inner_var params_renaming with
            | exception Not_found ->
              (* This specialised argument is not an argument of the
                 current function. *)
              None
            | wrapper_param -> Some wrapper_param)
      in
      (* In case a free variable and a specialised arg have the same
         outer variable, we give the specialised arg priority, since this
         map is going to be used to rewrite terms to wrapper params. *)
      Variable.Map.union (fun _outer_var spec_arg _free_var -> Some spec_arg)
        for_spec_args existing_free_vars_inverse
    in
    (*  2. Renaming of newly-introduced specialised arguments: the fresh
        variables are used for the [let]-bindings in the wrapper.

        Specialised args
        bound in the wrapper

                ^
                |
                |
                |

        New specialised args                            New specialised
        inner (which are all    -------------------->   args outer
        parameters of the       new_inner_to_new
        main function)            _outer_vars

    *)
    let new_outer_vars_to_spec_args_bound_in_the_wrapper_renaming =
      (* Bottom right to top left in diagram 2 above. *)
      Variable.Map.fold (fun new_inner_var (spec_to : Flambda.specialised_to)
                renaming ->
          let new_outer_var = spec_to.var in
          let inner_var_of_wrapper =
            Variable.rename new_inner_var ~append:T.variable_suffix
          in
          assert (not (Variable.Map.mem new_outer_var renaming));
          Variable.Map.add new_outer_var inner_var_of_wrapper renaming)
        new_inner_to_new_outer_vars
        Variable.Map.empty
    in
    let spec_args_bound_in_the_wrapper =
      (* N.B.: in the order matching the new specialised argument parameters
         to the main function. *)
      Variable.Map.data
        new_outer_vars_to_spec_args_bound_in_the_wrapper_renaming
    in
    let wrapper_body : Flambda.expr =
      let apply : Flambda.expr =
        Apply {
          func = new_fun_var;
          args = wrapper_params @ spec_args_bound_in_the_wrapper;
          kind = Direct (Closure_id.wrap new_fun_var);
          dbg = Debuginfo.none;
          inline = Default_inline;
        }
      in
      Variable.Map.fold (fun new_outer_var
            defining_expr_using_existing_outer_vars wrapper_body ->
          (* The defining expression is currently in terms of the
             existing outer vars (the variables to which the existing
             specialised args were specialised); we must rewrite it to use
             the parameters of the wrapper. *)
          let defining_expr =
            Flambda_utils.toplevel_substitution_named
              existing_outer_vars_to_wrapper_params_renaming
              defining_expr_using_existing_outer_vars
          in
          match
            Variable.Map.find new_outer_var
              new_outer_vars_to_spec_args_bound_in_the_wrapper_renaming
          with
          | exception Not_found -> assert false
          | new_inner_var_of_wrapper ->
            Flambda.create_let new_inner_var_of_wrapper defining_expr
              wrapper_body)
      new_specialised_args_indexed_by_new_outer_vars
      apply
    in
    let new_function_decl =
      Flambda.create_function_declaration
        ~params:wrapper_params
        ~body:wrapper_body
        ~stub:true
        ~dbg:Debuginfo.none
        ~inline:Default_inline
        ~is_a_functor:false
    in
    new_fun_var, new_function_decl, params_renaming

  let rewrite_function_decl ~env ~backend ~fun_var ~set_of_closures
      ~(function_decl : Flambda.function_declaration)
      ~(for_one_function : for_one_function) =
    if function_decl.stub then
      None
    else
      let closure_id = Closure_id.wrap fun_var in
      let (_ : int), new_specialised_args_indexed_by_new_outer_vars =
        let module Backend = (val backend : Backend_intf.S) in
        let max_args = Backend.max_sensible_number_of_arguments in
        List.fold_left (fun (num_params, new_spec_args) add_all_or_none ->
            let num_new_args =
              (* CR mshinwell: For [Unbox_specialised_args] this doesn't
                 take into account the fact that we expect to delete the
                 specialised argument(s) being unboxed (although we might
                 not be able to, so this is currently conservative). *)
              Variable.Map.cardinal add_all_or_none
            in
            let new_num_params = num_params + num_new_args in
            (* CR mshinwell: consider sorting the groups in some way,
               maybe by decreasing total benefit. *)
            if new_num_params > max_args then
              num_params, new_spec_args
            else
              try
                let new_spec_args =
                  Variable.Map.disjoint_union new_spec_args add_all_or_none
                in
                new_num_params, new_spec_args
              with _exn ->
                Misc.fatal_error "Augment_specialised_args: groups of \
                    new specialised args overlap")
          (List.length function_decl.params, Variable.Map.empty)
          what_to_specialise.new_specialised_args_indexed_by_new_outer_vars
      in
      if Variable.Map.cardinal
          new_specialised_args_indexed_by_new_outer_vars < 1
      then
        None
      else
        let new_inner_to_new_outer_vars =
          Variable.Map.filter (fun _new_inner_var
                    (new_outer_var : Flambda.specialised_to) ->
              Variable.Map.mem new_outer_var.var
                new_specialised_args_indexed_by_new_outer_vars)
            what_to_specialise.new_inner_to_new_outer_vars
        in
        let new_fun_var, wrapper, params_renaming =
          create_wrapper ~fun_var ~set_of_closures ~function_decl
            ~new_specialised_args_indexed_by_new_outer_vars
            ~new_inner_to_new_outer_vars
        in
        let all_params =
          let new_params =
            Variable.Set.elements (Variable.Map.keys
              new_inner_to_new_outer_vars)
          in
          function_decl.params @ new_params
        in
        let rewritten_function_decl =
          Flambda.create_function_declaration
            ~params:all_params
            ~body:function_decl.body
            ~stub:function_decl.stub
            ~dbg:function_decl.dbg
            ~inline:function_decl.inline
            ~is_a_functor:function_decl.is_a_functor
        in
        Some (
          new_fun_var, rewritten_function_decl, wrapper,
            new_specialised_args_indexed_by_new_outer_vars,
            new_inner_to_new_outer_vars,
            params_renaming)

  let check_invariants ~(set_of_closures : Flambda.set_of_closures)
        ~original_set_of_closures =
    if !Clflags.flambda_invariant_checks then begin
      Variable.Map.iter (fun fun_var
                (function_decl : Flambda.function_declaration) ->
          let params = Variable.Set.of_list function_decl.params in
          Variable.Map.iter (fun inner_var
                      (outer_var : Flambda.specialised_to) ->
                if Variable.Set.mem inner_var params then begin
                  assert (not (Variable.Set.mem outer_var.var
                    function_decl.free_variables));
                  match outer_var.projectee with
                  | None -> ()
                  | Some (projection, _projectee) ->
                    if not (Variable.Set.mem projection params) then begin
                      Misc.fatal_errorf "Augment_specialised_args (%s): \
                          specialised argument (%a -> %a) references a \
                          projection variable that is not a specialised \
                          argument of the function %a. @ The set of closures \
                          before the transformation was:@  %a. @ The set of \
                          closures after the transformation was:@ %a."
                        T.pass_name
                        Variable.print inner_var
                        Flambda.print_specialised_to outer_var
                        Variable.print fun_var
                        Flambda.print_set_of_closures original_set_of_closures
                        Flambda.print_set_of_closures set_of_closures
                    end
                end)
            set_of_closures.specialised_args)
        set_of_closures.function_decls.funs
    end

  let rewrite_set_of_closures_core ~backend ~env
        ~(set_of_closures : Flambda.set_of_closures) =
    match T.precondition ~backend ~env ~set_of_closures with
    | None -> None
    | Some user_data ->
      let original_set_of_closures = set_of_closures in
      let funs, new_specialised_arg_defns_indexed_by_new_outer_vars,
          specialised_args, done_something =
        Variable.Map.fold
          (fun fun_var function_decl
                (funs, new_specialised_args_indexed_by_new_outer_vars,
                 new_inner_to_new_outer_vars, done_something) ->
            match
              rewrite_function_decl ~backend ~env ~set_of_closures ~fun_var
                ~function_decl ~user_data
            with
            | None ->
              let funs = Variable.Map.add fun_var function_decl funs in
              funs, new_specialised_args_indexed_by_new_outer_vars,
                new_inner_to_new_outer_vars, done_something
            | Some (
                new_fun_var, rewritten_function_decl, wrapper,
                new_specialised_args_indexed_by_new_outer_vars',
                new_inner_to_new_outer_vars',
                params_renaming) ->
              let funs =
                assert (not (Variable.Map.mem new_fun_var funs));
                Variable.Map.add new_fun_var rewritten_function_decl
                  (Variable.Map.add fun_var wrapper funs)
              in
              let new_specialised_args_indexed_by_new_outer_vars =
                Variable.Map.union (fun _var _def1 def2 ->
                    (* CR mshinwell: This should ensure [def1] and [def2]
                       are the same.  Even better, change the interface
                       to this module so we can express that we don't need
                       to add a lifted definition since it's already there. *)
                    Some def2)
                  new_specialised_args_indexed_by_new_outer_vars
                  new_specialised_args_indexed_by_new_outer_vars'
              in
              let new_inner_to_new_outer_vars =
                (* This will form the augmentation to the existing
                   specialised_args of the set of closures.  We must include
                   not only the new arguments requested by [T] but also
                   the parameters of the wrapper corresponding to the
                   existing specialised args (irrespective of whether any
                   particular specialised arg is being augmented or not). *)
                let for_new_arguments =
                  Variable.Map.disjoint_union new_inner_to_new_outer_vars
                    new_inner_to_new_outer_vars'
                in
                let for_existing_arguments =
                  Variable.Map.fold (fun inner_var
                            (outer_var : Flambda.specialised_to)
                            for_existing ->
                      match Variable.Map.find inner_var params_renaming with
                      | exception Not_found ->
                        (* Not a parameter of this [function_decl]. *)
                        for_existing
                      | wrapper_param ->
                        assert (not (Variable.Map.mem wrapper_param
                            for_existing));
                        (* Any projection associated with the outer variable
                           must be rewritten in terms of the specialised
                           arguments of this function. *)
                        let outer_var : Flambda.specialised_to =
                          match outer_var.projectee with
                          | None -> outer_var
                          | Some (projection, projectee) ->
                            let projection =
                              match
                                Variable.Map.find projection params_renaming
                              with
                              | exception Not_found ->
                                (* Must always be a parameter of this
                                   [function_decl]. *)
                                assert false
                              | wrapper_param -> wrapper_param
                            in
                            { outer_var with
                              projectee = Some (projection, projectee);
                            }
                        in
                        Variable.Map.add wrapper_param outer_var
                          for_existing)
                    set_of_closures.specialised_args
                    Variable.Map.empty
                in
                Variable.Map.disjoint_union for_new_arguments
                  for_existing_arguments
              in
              funs, new_specialised_args_indexed_by_new_outer_vars,
                new_inner_to_new_outer_vars, true)
          set_of_closures.function_decls.funs
          (Variable.Map.empty,
            Variable.Map.empty,
            set_of_closures.specialised_args,
            false)
      in
      if not done_something then
        None
      else
        let existing_specialised_args = set_of_closures.specialised_args in
        let function_decls =
          Flambda.update_function_declarations set_of_closures.function_decls
            ~funs
        in
        assert (Variable.Map.cardinal specialised_args
          >= Variable.Map.cardinal set_of_closures.specialised_args);
        let set_of_closures =
          Flambda.create_set_of_closures
            ~function_decls
            ~free_vars:set_of_closures.free_vars
            ~specialised_args
        in
        check_invariants ~set_of_closures ~original_set_of_closures;
        let expr =
          Variable.Map.fold (fun new_outer_var (definition : Definition.t)
                    expr ->
              let named : Flambda.named =
                match definition with
                | Existing_outer_var existing_outer_var ->
                  Expr (Var existing_outer_var)
                | Projection_from_existing_specialised_arg
                    (spec_arg, projectee) ->
                  (* The lifted definition must be in terms of outer variables,
                     not inner variables. *)
                  match
                    Variable.Map.find spec_args existing_specialised_args
                  with
                  | exception Not_found -> assert false
                  | outer_var ->
                    match projectee with
                    | Project_var var_within_closure ->
                      Project_var {
                        closure = outer_var;
                        closure_id = ...;
                        var = var_within_closure;
                      }
                    | Closure closure_id ->
                      (* We leave [Inline_and_simplify] to turn this into a
                         [Project_closure], if such can be done. *)
                      Move_within_set_of_closures {
                        closure = outer_var;
                        start_from = ...;
                        move_to = closure_id;
                      }
                    | Field field_index ->
                      Prim (Pfield field_index, [outer_var], Debuginfo.none)
              in
              Flambda.create_let new_outer_var named expr)
            what.new_lifted_projection_defining_exprs_indexed_by_new_outer_vars
            (Flambda_utils.name_expr (Set_of_closures set_of_closures)
              ~name:T.pass_name)
        in
        Some expr

  let rewrite_set_of_closures ~backend ~env ~set_of_closures =
    Pass_wrapper.with_dump ~pass_name:T.pass_name ~input:set_of_closures
      ~print_input:Flambda.print_set_of_closures
      ~print_output:Flambda.print
      ~f:(fun () ->
        rewrite_set_of_closures_core ~backend ~env ~set_of_closures)
end
