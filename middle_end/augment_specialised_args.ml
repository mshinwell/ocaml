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
    | Existing_inner_var of Variable.t
    | Projection_from_existing_specialised_arg of Projection.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Existing_inner_var var1, Existing_inner_var var2 ->
        Variable.compare var1 var2
      | Projection_from_existing_specialised_arg proj1,
          Projection_from_existing_specialised_arg proj2 ->
        Projection.compare proj1 proj2
      | Existing_inner_var, _ -> -1
      | _, Existing_inner_var -> 1

    let equal t1 t2 =
      (compare t1 t2) = 0

    let hash = Hashtbl.hash

    let print _ _ = failwith "Definition.print not yet implemented"

    let output _ _ = failwith "Definition.output not yet implemented"
  end)
end

module What_to_specialise = struct
  type t = {
    (* [definitions] is indexed by (fun_var, group) *)
    definitions : Definition.t list Variable.Pair.Map.t;
    set_of_closures : Flambda.set_of_closures;
    existing_inner_to_outer_vars : Variable.t Variable.Map.t;
  }

  let create ~set_of_closures ~existing_inner_to_outer_vars =
    let existing_inner_to_outer_vars =
      Variable.Map.map (fun (spec_to : Flambda.specialised_to) -> spec_to.var)
        existing_inner_to_outer_vars;
    in
    { definitions = [];
      set_of_closures;
      existing_inner_to_outer_vars;
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
    new_inner_to_new_outer_vars : Variable.Map.t Variable.Map.t;
    total_number_of_args : int;
  }

  type fun_var_and_group = {
    fun_var : Variable.t;
    group : Variable.t;
  }

  type t = {
    set_of_closures : Flambda.set_of_closures;
    existing_inner_to_outer_vars : Variable.Map.t Variable.Map.t;
    existing_definitions_via_spec_args_indexed_by_fun_var
      : Projection.Set.t Variable.Map.t;
    new_definitions_indexed_by_new_outer_vars : Projection.t Variable.Map.t;
    new_outer_vars_indexed_by_new_definitions : Variable.t Projection.Map.t;
    functions : for_one_function Variable.Map.t;
  }

  let really_add_new_specialised_arg t ~fun_var ~definition
        ~for_one_function =
    (* We know here that a new specialised argument must be added.  This
       needs a "new inner var" and a "new outer var".  However if there
       is already a lifted projection being introduced around the set
       of closures (corresponding to another new specialised argument),
       we should re-use its "new outer var" to avoid duplication of
       projection definitions. *)
    let new_outer_var, (t : t) =
      match
        Definition.Map.find definition
          t.new_outer_vars_indexed_by_new_definitions
      with
      | new_outer_var -> new_outer_var, t
      | exception Not_found ->
        (* We are adding a new lifted definition: generate a fresh
           "new outer var". *)
        let new_outer_var =
          Variable.rename group ~suffix:(T.pass_name ^ "_new_outer")
        in
        { t with
          new_outer_vars_indexed_by_new_definitions =
            Variable.Map.add
              definition new_outer_var
              t.new_outer_vars_indexed_by_new_definitions;
          new_definitions_indexed_by_new_outer_vars =
            Variable.Map.add
              new_outer_var defining_expr
              t.new_definitions_indexed_by_new_outer_vars;
        }
    in
    let new_inner_var =
      Variable.rename group ~suffix:(T.pass_name ^ "_new_inner")
    in
    let new_inner_to_new_outer_vars =
      Variable.Map.add new_inner_var new_outer_var
        t.new_inner_to_new_outer_vars
    in
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
            t.existing_definitions_of_specialised_args_indexed_by_fun_var
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
    let existing_definitions_via_spec_args_indexed_by_fun_var =
      Variable.Map.map (fun fun_var
                (function_decl : Flambda.function_declaration) ->
          let params = Variable.Set.of_list function_decl.params in
          Variable.Map.fold (fun inner_var
                    (spec_to : Flambda.specialised_to) definitions ->
              if not (Variable.Set.mem inner_var params) then
                definitions
              else
                let definition : Definition.t =
                  match spec_to.projection with
                  | None -> Existing_inner_var inner_var
                  | Some projection ->
                    Projection_from_existing_specialised_arg projection
                in
                Variable.Map.add definition definitions)
            what_to_specialise.set_of_closures.specialised_args
            Definition.Set.empty)
        what_to_specialise.set_of_closures.function_decl.funs
    in
    let t : t =
      { set_of_closures = what_to_specialise.set_of_closures;
        existing_inner_to_outer_vars =
          what_to_specialise.existing_inner_to_outer_vars;
        existing_definitions_via_spec_args_indexed_by_fun_var;
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
          (* Try adding all specialised args in the current group. *)
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

  let rename_function_parameters ~fun_var
        ~(function_decl : Flambda.function_declaration) =
    let new_fun_var = Variable.rename fun_var ~append:T.variable_suffix in
    let params_renaming =
      Variable.Map.of_list
        (List.map (fun param ->
            let new_param = Variable.rename param ~append:T.variable_suffix in
            param, new_param)
          function_decl.params)
    in
    let renamed_params =
      List.map (fun param -> Variable.Map.find param params_renaming)
        function_decl.params
    in
    new_fun_var, params_renaming, renamed_params

  let invert_specialised_args_map ~(set_of_closures : Flambda.set_of_closures)
        ~(function_decl : Flambda.function_declaration) =
    (* Filter the specialised args map from the given set of closures, so
       it only contains arguments of the given function, and then invert it. *)
    let specialised_args =
      Variable.Map.filter_map set_of_closures.specialised_args
        ~f:(fun inner_var (spec_to : Flambda.specialised_to) ->
          if Variable.Set.mem inner_var function_decl.params then
            Some spec_to.var
          else
            None)
    in
    Variable.Map.transpose_keys_and_data specialised_args

  let create_wrapper ~fun_var ~(set_of_closures : Flambda.set_of_closures)
      ~(function_decl : Flambda.function_declaration)
      ~definitions_indexed_by_new_inner_vars
      ~new_inner_to_new_outer_vars =
    (* To avoid increasing the free variables of the wrapper, for
       general cleanliness, we restate the definitions of the
       newly-specialised arguments in the wrapper itself in terms of the
       original specialised arguments.  The variables bound to these
       definitions are called the "specialised args bound in the wrapper".
       Note that the domain of [params_renaming] is a (non-strict) superset
       of the "inner vars" of the original specialised args. *)
    let params = Variable.Set.of_list function_decl.params in
    let new_fun_var, params_renaming, wrapper_params =
      rename_function_and_parameters ~fun_var ~function_decl
    in
    let new_inner_vars_to_spec_args_bound_in_the_wrapper_renaming =
      Variable.Map.mapi (fun new_inner_var _ ->
          Variable.rename new_inner_var ~suffix:T.pass_name)
        ...
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
      Variable.Map.fold (fun new_inner_var definition wrapper_body ->
          (* Definitions that project from existing specialised args need
             to be rewritten to use the corresponding specialised args of
             the wrapper.  Definitions that are just equality to existing
             inner free variables do not need to be changed. *)
          let definition : Definition.t =
            let find_wrapper_param param =
              assert (Variable.Set.mem param params);
              match Variable.Map.find inner_var params_renaming with
              | wrapper_param -> wrapper_param
              | exception Not_found ->
                Misc.fatal_errorf "find_wrapper_param: expected %a \
                    to be in [params_renaming], but it is not."
                  Variable.print param
            in
            match definition with
            | Existing_inner_free_var _ -> definition
            | Projection_from_existing_specialised_arg projection ->
              Projection_from_existing_specialised_arg
                (Projection.map_projecting_from ~f:find_wrapper_param)
          in
          match
            Variable.Map.find new_inner_var
              new_inner_vars_to_spec_args_bound_in_the_wrapper_renaming
          with
          | exception Not_found -> assert false
          | new_inner_var_of_wrapper ->
            let named : Flambda.named =
              match definition with
              | Existing_inner_var existing_inner_var ->
                Expr (Var existing_inner_var)
              | Projection_from_existing_specialised_arg projection ->
                Flambda_utils.projection_to_named projection
            in
            Flambda.create_let new_inner_var_of_wrapper named wrapper_body)
      definitions_indexed_by_new_inner_vars
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
          (* Add the lifted definitions around the rewritten set of
             closures. *)
          Variable.Map.fold (fun new_outer_var (definition : Definition.t)
                    expr ->
              let named : Flambda.named =
                (* The lifted definition must be in terms of outer variables,
                   not inner variables. *)
                let find_outer_var inner_var =
                  match
                    Variable.Map.find inner_var existing_inner_to_outer_vars
                  with
                  | outer_var -> outer_var
                  | exception Not_found ->
                    Misc.fatal_errorf "find_outer_var: expected %a \
                        to be in [existing_inner_to_outer_vars], but it is \
                        not.  (The projection was: %a)"
                      Variable.print inner_var
                      Projection.print projection
                in
                match definition with
                | Existing_inner_var existing_inner_var ->
                  Expr (Var find_outer_var existing_inner_var)
                | Projection_from_existing_specialised_arg projection ->
                  let projection =
                    Projection.map_projecting_from projection ~f:find_outer_var
                  in
                  Flambda_utils.projection_to_named projection
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
