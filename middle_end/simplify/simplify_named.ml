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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module B = Inlining_cost.Benefit
module E = Simplify_env_and_result.Env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module T = Flambda_type

module Named = Flambda.Named

let freshen_continuation env cont =
  Freshening.add_continuation (E.freshening env) cont

let simplify_set_of_closures original_env r
      (set_of_closures : Flambda.Set_of_closures.t)
      : Flambda.Set_of_closures.t * T.t * R.t =
(* CR mshinwell for pchambart: This can be removed now, right?
  let function_decls =
    let module Backend = (val (E.backend original_env) : Backend_intf.S) in
    (* CR-soon mshinwell: Does this affect
       [reference_recursive_function_directly]?
       mshinwell: This should be thought about as part of the wider issue of
       references to functions via symbols or variables. *)
    Freshening.rewrite_recursive_calls_with_symbols (E.freshening original_env)
      set_of_closures.function_decls
      ~make_closure_symbol:Backend.closure_symbol
  in
*)
  let env = E.increase_closure_depth original_env in
  let function_decls, _set_of_closures_ty, set_of_closures_env =
    Simplify_aux.prepare_to_simplify_set_of_closures ~env
      ~set_of_closures ~function_decls:set_of_closures.function_decls
  in
  let continuation_param_uses = Continuation.Tbl.create 42 in
  let simplify_function closure_id
        (function_decl : Flambda.Function_declaration.t)
        (funs, r) =
    let closure_env =
      Simplify_aux.prepare_to_simplify_closure ~function_decl
        ~set_of_closures_env
    in
    let continuation_param, closure_env =
      let continuation_param, freshening =
        freshen_continuation closure_env function_decl.continuation_param
      in
      let cont_type =
        Continuation_approx.create_unknown ~name:continuation_param
          ~arity:function_decl.return_arity
      in
      let closure_env =
        E.add_continuation (E.set_freshening closure_env freshening)
          continuation_param cont_type
      in
      continuation_param, closure_env
    in
    let exn_continuation_param, closure_env =
      let exn_continuation_param, freshening =
        freshen_continuation closure_env function_decl.exn_continuation_param
      in
      let cont_type =
        Continuation_approx.create_unknown ~name:exn_continuation_param
          ~arity:[K.value Unknown]
      in
      let closure_env =
        E.add_continuation (E.set_freshening closure_env freshening)
          exn_continuation_param cont_type
      in
      exn_continuation_param, closure_env
    in
    let my_closure, freshening =
      Freshening.add_variable (E.freshening env) function_decl.my_closure
    in
    let closure_env = E.set_freshening closure_env freshening in
    let body, r, return_continuation_uses =
      E.enter_closure closure_env ~closure_id
        ~inline_inside:
          (Inlining_decision.should_inline_inside_declaration function_decl)
        ~dbg:function_decl.dbg
        ~f:(fun body_env ->
          assert (E.inside_set_of_closures_declaration
            function_decls.set_of_closures_origin body_env);
          (* We don't need to collect the lifted constants separately; they
             will remain in [r]. *)
          let body, r, uses, _lifted_constants =
            let descr =
              Format.asprintf "the body of %a" Closure_id.print closure_id
            in
            (E.simplify_toplevel body_env) body_env r function_decl.body
              ~continuation:continuation_param
              ~exn_continuation:exn_continuation_param
              ~descr
          in
          Continuation.Tbl.add continuation_param_uses continuation_param uses;
          body, r, uses)
    in
    let result, _typing_env =
      R.Continuation_uses.join_of_arg_types return_continuation_uses
        ~arity:function_decl.return_arity
        ~default_env:(E.get_typing_environment closure_env)  (* XXX *)
    in
    (* CR mshinwell: Anything to do with the returned environment? *)
    let inline : Flambda.inline_attribute =
      match function_decl.inline with
      | Default_inline ->
        if !Clflags.classic_inlining && not function_decl.stub then begin
          (* In classic-inlining mode, the inlining decision is taken at
             definition site (here).  If the function is small enough
             (below the -inline threshold) it will always be inlined. *)
          let inlining_threshold =
            Simplify_aux.initial_inlining_threshold ~round:(E.round env)
          in
          if Inlining_cost.can_inline body inlining_threshold ~bonus:0
          then Always_inline
          else Default_inline
        end else begin
          Default_inline
        end
      | inline -> inline
    in
    let function_decl =
      Flambda.Function_declaration.create ~params:function_decl.params
        ~continuation_param ~exn_continuation_param
        ~return_arity:function_decl.return_arity
        ~body ~stub:function_decl.stub ~dbg:function_decl.dbg
        ~inline ~specialise:function_decl.specialise
        ~is_a_functor:function_decl.is_a_functor
        ~closure_origin:function_decl.closure_origin
        ~my_closure
    in
    let ty =
      let params =
        List.map (fun param ->
            Flambda.Typed_parameter.param param,
              Flambda.Typed_parameter.ty param)
          function_decl.params
      in
      (* CR mshinwell: Put this conversion in a function somewhere *)
      let inline : T.inline_attribute =
        match function_decl.inline with
        | Always_inline -> Always_inline
        | Never_inline -> Never_inline
        | Unroll n -> Unroll n
        | Default_inline -> Default_inline
      in
      let specialise : T.specialise_attribute =
        match function_decl.specialise with
        | Always_specialise -> Always_specialise
        | Never_specialise -> Never_specialise
        | Default_specialise -> Default_specialise
      in
      T.create_inlinable_function_declaration
        ~is_classic_mode:!Clflags.classic_inlining
        ~closure_origin:function_decl.closure_origin
        ~continuation_param
        ~exn_continuation_param
        ~params
        ~body
        ~result
        ~stub:function_decl.stub
        ~dbg:function_decl.dbg
        ~inline
        ~specialise
        ~is_a_functor:function_decl.is_a_functor
        ~invariant_params:(lazy Variable.Set.empty)  (* CR mshinwell: fix *)
        ~size:(lazy None)  (* CR mshinwell: to fix *)
        ~direct_call_surrogate:None  (* CR mshinwell: to fix *)
        ~my_closure
    in
(* CR mshinwell: temporarily disabled
    let function_decl =
      match Unrecursify.unrecursify_function ~closure_id ~function_decl with
      | None -> function_decl
      | Some function_decl -> function_decl
    in
*)
    Closure_id.Map.add closure_id (function_decl, ty) funs, r
  in
  let funs_with_types, r =
    Closure_id.Map.fold simplify_function function_decls.funs
      (Closure_id.Map.empty, r)
  in
  let function_decls =
    let funs =
      Closure_id.Map.map (fun (func_decl, _ty) -> func_decl) funs_with_types
    in
    Flambda.Function_declarations.update function_decls ~funs
  in
(*
  let function_decls =
    (* CR mshinwell: I'm not sure about this "round" condition.  It seems
       though that doing [Unbox_returns] too early may be
       detrimental, as it prevents small functions being inlined *)
    if E.never_inline env
      || E.round env < 2
      || E.never_unbox_continuations env
    then
      function_decls, Variable.Map.empty
    else
      let continuation_param_uses =
        Continuation.Tbl.to_map continuation_param_uses
      in
      Unbox_returns.run ~continuation_uses:continuation_param_uses
        ~function_decls ~backend:(E.backend env)
  in
*)
  let _invariant_params =
    Variable.Map.empty
(* CR mshinwell for pchambart: Need to fix Invariant_params
    lazy (Invariant_params.Functions.invariant_params_in_recursion
      function_decls ~backend:(E.backend env))
*)
  in
(* XXX
  let direct_call_surrogates =
    Closure_id.Map.fold (fun existing surrogate surrogates ->
        Variable.Map.add (Closure_id.unwrap existing)
          (Closure_id.unwrap surrogate) surrogates)
      internal_value_set_of_closures.direct_call_surrogates
      Variable.Map.empty
  in
*)
  let in_closure =
    Flambda.Free_vars.map_vars set_of_closures.free_vars ~f:(fun var ->
      (* XXX This should use a variant of [simplify_named] called
         [simplify_var] which always returns [Variable.t], otherwise
         we're not following aliases via types.  (And below) *)
      Freshening.apply_variable (E.freshening env) var)
  in
  let set_of_closures =
    Flambda.Set_of_closures.create ~function_decls ~in_closure
      ~direct_call_surrogates:Closure_id.Map.empty
  in
  let closures =
    Closure_id.Map.map (fun (_func_decl, ty) -> T.closure ty) funs_with_types
  in
  let closure_elements =
    Var_within_closure.Map.map (fun (free_var : Flambda.Free_var.t) ->
        let var = Freshening.apply_variable (E.freshening env) free_var.var in
        T.alias_type_of_as_ty_value (Name.var var))
      set_of_closures.free_vars
  in
  let ty =
    T.set_of_closures ~closures:(Exactly closures)
      ~closure_elements:(Exactly closure_elements)
  in
  set_of_closures, ty, r

let try_to_reify env r ty ~(term : Flambda.Reachable.t) ~result_var
      ~remove_term ~can_lift =
  match term with
  | Invalid _ -> 
    let ty = (E.type_accessor env T.bottom_like) ty in
    [], term, ty, remove_term ()
  | Reachable _ ->
    match (E.type_accessor env T.reify) ty ~allow_free_variables:true with
    | Term (simple, ty) ->
      let term : Named.t = Simple simple in
      [], Flambda.Reachable.reachable term, ty, remove_term ()
    | Lift static_part ->
      if not can_lift then [], term, ty, r
      else
        let symbol, r =
          let name = Variable.unique_name result_var in
          R.new_lifted_constant env r ~name ty static_part
        in
        let name = Name.symbol symbol in
        let kind = (E.type_accessor env T.kind) ty in
        let ty = T.alias_type_of kind name in
        let term : Named.t = Simple (Simple.name name) in
        [], Flambda.Reachable.reachable term, ty, r
    | Cannot_reify -> [], term, ty, r
    | Invalid ->
      let ty = (E.type_accessor env T.bottom_like) ty in
      [], Flambda.Reachable.invalid (), ty, remove_term ()

(** [simplify_named] returns:
    - extra [Let]-bindings to be inserted prior to the one being simplified;
    - the simplified [named];
    - the new result structure. *)
let simplify_named env r (tree : Named.t) ~result_var =
  match tree with
  | Simple simple ->
    let simple, ty = Simplify_simple.simplify_simple env simple in
    [], Flambda.Reachable.reachable (Simple simple), ty, r
  | Read_mutable mut_var ->
    (* See comment on the [Assign] case. *)
    let mut_var =
      Freshening.apply_mutable_variable (E.freshening env) mut_var
    in
    let ty = E.find_mutable_exn env mut_var in
    [], Flambda.Reachable.reachable (Read_mutable mut_var), ty, r
  | Set_of_closures set_of_closures ->
(*
    let backend = E.backend env in
    let cont_usage_snapshot = R.snapshot_continuation_uses r in
*)
    let set_of_closures, ty, r =
      simplify_set_of_closures env r set_of_closures
    in
    let term : Named.t = Set_of_closures set_of_closures in
    let remove_primitive () =
      R.map_benefit r (B.remove_code_named term)
    in
    let can_lift =
      Var_within_closure.Map.is_empty set_of_closures.free_vars
    in
    try_to_reify env r ty ~term:(Flambda.Reachable.reachable term)
      ~result_var ~remove_term:remove_primitive ~can_lift
(* XXX Disabled just for the moment -- mshinwell
    let simplify env r ~bindings ~set_of_closures ~pass_name =
      (* If simplifying a set of closures more than once during any given round
         of simplification, the [Freshening.Project_var] substitutions arising
         from each call to [simplify_set_of_closures] must be composed.
         Note that this function only composes with [first_freshening] owing
         to the structure of the code below (this new [simplify] is always
         in tail position).
         We also need to be careful not to double-count (or worse) uses of
         continuations. *)
      let r = R.roll_back_continuation_uses r cont_usage_snapshot in
      let bindings, set_of_closures, r =
        let env = E.set_never_inline env in
        simplify_newly_introduced_let_bindings env r ~bindings
          ~around:((Set_of_closures set_of_closures) : Named.t)
      in
      let ty = R.inferred_type r in
      let value_set_of_closures =
        match T.strict_check_type_for_set_of_closures ty with
        | Wrong ->
          Misc.fatal_errorf "Unexpected Flambda type returned from \
              simplification of [%s] result: %a"
            pass_name T.print ty
        | Ok (_var, value_set_of_closures) ->
          let freshening =
            Freshening.Project_var.compose ~earlier:first_freshening
              ~later:value_set_of_closures.freshening
          in
          T.update_freshening_of_value_set_of_closures value_set_of_closures
            ~freshening
      in
      bindings, set_of_closures,
        (ret r (T.set_of_closures value_set_of_closures))
    in
    (* This does the actual substitutions of specialised args introduced
       by [Unbox_closures] for free variables.  (Apart from simplifying
       the [Unbox_closures] output, this also prevents applying
       [Unbox_closures] over and over.) *)
    let set_of_closures =
      match Remove_free_vars_equal_to_args.run set_of_closures with
      | None -> set_of_closures
      | Some set_of_closures -> set_of_closures
    in
    (* Do [Unbox_closures] next to try to decide which things are
       free variables and which things are specialised arguments before
       unboxing them. *)
    match
      Unbox_closures.rewrite_set_of_closures ~env
        ~duplicate_function ~set_of_closures
    with
    | Some (bindings, set_of_closures, benefit) ->
      let r = R.add_benefit r benefit in
      simplify env r ~bindings ~set_of_closures ~pass_name:"Unbox_closures"
    | None ->
      match Unbox_free_vars_of_closures.run ~env ~set_of_closures with
      | Some (bindings, set_of_closures, benefit) ->
        let r = R.add_benefit r benefit in
        simplify env r ~bindings ~set_of_closures
          ~pass_name:"Unbox_free_vars_of_closures"
      | None ->
        (* CR-soon mshinwell: should maybe add one allocation for the
           stub *)
        match
          Unbox_specialised_args.rewrite_set_of_closures ~env
            ~duplicate_function ~set_of_closures
        with
        | Some (bindings, set_of_closures, benefit) ->
          let r = R.add_benefit r benefit in
          simplify env r ~bindings ~set_of_closures
            ~pass_name:"Unbox_specialised_args"
        | None ->
          match
            Remove_unused_arguments.
                separate_unused_arguments_in_set_of_closures
              set_of_closures ~backend
          with
          | Some set_of_closures ->
            simplify env r ~bindings:[] ~set_of_closures
              ~pass_name:"Remove_unused_arguments"
          | None -> [], Reachable (Set_of_closures set_of_closures), r
    end *)
  | Prim (prim, dbg) ->
    let term, ty, r =
      Simplify_primitive.simplify_primitive env r prim dbg ~result_var
    in
(*
Format.eprintf "Prim %a: type %a\n%!" Variable.print result_var T.print ty;
*)
    let remove_primitive () =
      R.map_benefit r (B.remove_primitive_application prim)
    in
    let effects_and_coeffects_ok =
      Flambda_primitive.With_fixed_value.eligible prim
    in
    try_to_reify env r ty ~term ~result_var ~remove_term:remove_primitive
      ~can_lift:effects_and_coeffects_ok
  | Assign { being_assigned; new_value; } ->
    let being_assigned =
      Freshening.apply_mutable_variable (E.freshening env) being_assigned
    in
    (* CR mshinwell: This needs a kind check, but we're planning to remove
       mutable variables soon anyway, so we won't bother *)
    let new_value, _ty = Simplify_simple.simplify_simple env new_value in
    [], Flambda.Reachable.reachable (Assign { being_assigned; new_value; }),
      T.unit (), r
  | Coerce (Kind (simple, desired_kind)) ->
    let simple, ty = Simplify_simple.simplify_simple env simple in
    let actual_kind = (E.type_accessor env T.kind) ty in
    match Flambda_kind.coerce ~actual_kind ~desired_kind with
    | Always_ok ->
      [], Flambda.Reachable.reachable (Simple simple), ty, r
    | Needs_runtime_check ->
      [], Flambda.Reachable.reachable (Coerce (Kind (simple, desired_kind))),
        T.unknown desired_kind, r
    | Always_wrong ->
      [], Flambda.Reachable.invalid (), T.bottom desired_kind, r
