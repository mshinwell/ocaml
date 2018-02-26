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

module E = Simplify_env_and_result.Env
module R = Simplify_env_and_result.Result

(** Values of two types hold the information propagated during simplification:
    - [E.t] "environments", top-down, almost always called "env";
    - [R.t] "results", bottom-up approximately following the evaluation order,
      almost always called "r".  These results come along with rewritten
      Flambda terms.
    The environments map variables to Flambda types, which enable various
    simplifications to be performed; for example, some variable may be known
    to always hold a particular constant.
*)

let check_toplevel_simplification_result r expr ~continuation
      ~exn_continuation ~descr =
  if !Clflags.flambda_invariant_checks then begin
    let without_definitions = R.used_continuations r in
    let bad_without_definitions =
      Continuation.Set.remove continuation without_definitions
    in
    let bad_without_definitions =
      Continuation.Set.remove exn_continuation bad_without_definitions
    in
    if not (Continuation.Set.is_empty bad_without_definitions) then begin
      Misc.fatal_errorf "The following continuations in %s \
          had uses but no definitions recorded for them in [r]: %a.  \
          Term:\n@ %a"
        descr
        Continuation.Set.print bad_without_definitions
        Flambda.Expr.print expr
    end;
    let continuation_definitions_with_uses =
      R.continuation_definitions_with_uses r
    in
    let defined_continuations_in_r =
      Continuation.Map.keys continuation_definitions_with_uses
    in
    let defined_continuations =
      Flambda.Expr.all_defined_continuations_toplevel expr
    in
    (* This is deliberately a strong condition. *)
    if not (Continuation.Set.equal defined_continuations_in_r
      defined_continuations)
    then begin
      Misc.fatal_errorf "Defined continuations in [r] (%a) do not match those \
          defined in %s (%a) (in [r] but not in the term: %a; \
          in the term but not in [r]: %a):@ \n%a"
        Continuation.Set.print defined_continuations_in_r
        descr
        Continuation.Set.print defined_continuations
        Continuation.Set.print
        (Continuation.Set.diff defined_continuations_in_r
          defined_continuations)
        Continuation.Set.print
        (Continuation.Set.diff defined_continuations
          defined_continuations_in_r)
        Flambda.Expr.print expr
    end;
    (* CR mshinwell: The following could check the actual code in the
       continuation approximations matches the code in the term. *)
    let all_handlers_in_continuation_approxs =
      Continuation.Map.fold (fun _cont (_, approx, _, _) all_handlers ->
          match Continuation_approx.handlers approx with
          | None -> all_handlers
          | Some (Non_recursive _) ->
            Continuation.Set.add (Continuation_approx.name approx) all_handlers
          | Some (Recursive handlers) ->
            Continuation.Set.union all_handlers
              (Continuation.Map.keys handlers))
        (R.continuation_definitions_with_uses r)
        Continuation.Set.empty
    in
    if not (Continuation.Set.equal defined_continuations
      all_handlers_in_continuation_approxs)
    then begin
      Misc.fatal_errorf "Continuations don't match up between the \
          continuation approximations in [r] (%a) and the term \
          (%a):@ \n%a\n"
        Continuation.Set.print all_handlers_in_continuation_approxs
        Continuation.Set.print defined_continuations
        Flambda.Expr.print expr
    end;
    (* Checking the number of uses recorded in [r] is correct helps to catch
       bugs where, for example, some [Value_unknown] approximation for some
       argument of some continuation fails to be removed by a transformation
       that produces a more precise approximation (which can cause the
       join of the argument's approximations to remain at [Value_unknown]). *)
    let counts = Flambda.Expr.count_continuation_uses_toplevel expr in
    Continuation.Map.iter (fun cont (uses, _, _, _) ->
        let num_in_term =
          match Continuation.Map.find cont counts with
          | exception Not_found -> 0
          | count -> count
        in
        let num_in_r =
          Simplify_env_and_result.Result.Continuation_uses.num_uses uses
        in
        if num_in_term <> num_in_r then begin
          Misc.fatal_errorf "Continuation count mismatch for %a between the \
              term (%d) and [r] (%d):@ Continuation uses:@ %a@ Term:@ %a"
            Continuation.print cont
            num_in_term
            num_in_r
            Simplify_env_and_result.Result.Continuation_uses.print uses
            Flambda.Expr.print expr
        end)
      continuation_definitions_with_uses
  end

(* CR mshinwell: We should add a structure to record the types of bound names
   and any freshenings applied.  This can go in "r" along with continuation
   usage.  Then we should be able to use this to check that types are not
   regressing in preciseness, even if we are now robust against that
   possibility. *)

let simplify_toplevel env r expr ~continuation ~exn_continuation ~descr =
  if not (Continuation.Map.mem continuation (E.continuations_in_scope env))
  then begin
    Misc.fatal_errorf "The continuation parameter (%a) must be in the \
        environment before calling [simplify_toplevel]"
      Continuation.print continuation
  end;
  if not (Continuation.Map.mem exn_continuation (E.continuations_in_scope env))
  then begin
    Misc.fatal_errorf "The exception continuation parameter (%a) must be in \
        the environment before calling [simplify_toplevel]"
      Continuation.print exn_continuation
  end;
  (* Use-def pairs of continuations cannot cross function boundaries.
     We localise the uses and definitions of continuations within each
     toplevel expression / function body by using the snapshot/restore
     functions in [R].  This ensures in particular that passes such as
     [Continuation_specialisation] which look at the defined
     continuations information in [r] won't see definitions that don't
     actually exist at toplevel in the expression they are analysing.
  *)
  let continuation_uses_snapshot, r =
    R.snapshot_and_forget_continuation_uses r
  in
  let expr, r = Simplify_expr.simplify_expr env r expr in
  check_toplevel_simplification_result r expr ~continuation ~exn_continuation
    ~descr;
  let expr, r =
    let expr, r =
      if E.never_inline_continuations env then begin
        expr, r
      end else begin
        (* Continuation inlining and specialisation is done now, rather than
           during [simplify]'s traversal itself, to reduce quadratic behaviour
           to linear.
           Since we only inline linearly-used non-recursive continuations, the
           changes to [r] that need to be made by the inlining pass are
           straightforward. *)
        let expr, r =
          Continuation_inlining.for_toplevel_expression expr r
        in
        check_toplevel_simplification_result r expr ~continuation
          ~exn_continuation ~descr;
        expr, r
      end
    in
    if E.never_specialise_continuations env then begin
      expr, r
    end else begin
      expr, r
(* CR mshinwell: To be re-enabled
      let vars_in_scope = E.vars_in_scope env in
      let new_expr =
        (* CR mshinwell: Should the specialisation pass return some
           benefit value? *)
        Continuation_specialisation.for_toplevel_expression expr
          ~vars_in_scope r
          ~simplify_let_cont_handlers:Simplify_expr.simplify_let_cont_handlers
          ~backend:(E.backend env)
      in
      match new_expr with
      | None -> expr, r
      | Some new_expr -> new_expr, r
*)
    end
  in
  (* Continuation specialisation could theoretically improve the precision
     of the Flambda type for [continuation].  However tracking changes to 
     continuation usage during specialisation is complicated and error-prone,
     so instead, we accept an Flambda type for [continuation] that may be
     slightly less precise.  Any subsequent round of simplification will
     calculate the improved Flambda type anyway. *)
  (* CR mshinwell: try to fix the above *)
  let r, uses = R.exit_scope_of_let_cont r env continuation in
  let r, _uses = R.exit_scope_of_let_cont r env exn_continuation in
  let lifted_constants = R.get_lifted_constants r in
  let r = R.roll_back_continuation_uses r continuation_uses_snapshot in
  (* At this stage:
     - no continuation defined in [expr] should be mentioned in [r];
     - the free continuations of [expr] must be at most the [continuation]
       and [exn_continuation] parameters. *)
  if !Clflags.flambda_invariant_checks then begin
    let defined_conts = Flambda.Expr.all_defined_continuations_toplevel expr in
    let r_used = R.used_continuations r in
    let r_defined =
      Continuation.Map.keys (R.continuation_definitions_with_uses r)
    in
    let check_defined from_r descr' =
      let bad = Continuation.Set.inter defined_conts from_r in
      if not (Continuation.Set.is_empty bad) then begin
        Misc.fatal_errorf "Continuations (%a) defined locally to %s are still \
            mentioned in %s [r] upon leaving [simplify_toplevel]:@ \n%a"
          Continuation.Set.print bad
          descr
          descr'
          Flambda.Expr.print expr
      end
    in
    check_defined r_used "the use information inside";
    check_defined r_defined "the defined-continuations information inside";
    let free_conts = Flambda.Expr.free_continuations expr in
    let bad_free_conts =
      Continuation.Set.diff free_conts
        (Continuation.Set.union
          (Continuation.Set.singleton continuation)
          (Continuation.Set.singleton exn_continuation))
    in
    if not (Continuation.Set.is_empty bad_free_conts) then begin
      Misc.fatal_errorf "The free continuations of %s \
          must be at most {%a %a} (but are instead {%a}):@ \n%a"
        descr
        Continuation.print continuation
        Continuation.print exn_continuation
        Continuation.Set.print free_conts
        Flambda.Expr.print expr
    end
  end;
  expr, r, uses, lifted_constants

let duplicate_function ~env:_ ~(set_of_closures : Flambda.Set_of_closures.t)
      ~closure_id:_ ~new_closure_id:_ =
  ignore set_of_closures;
  assert false
(* CR mshinwell: To do later (crib from Simplify_named)
  let function_decl =
    match Closure_id.Map.find fun_var set_of_closures.function_decls.funs with
    | exception Not_found ->
      Misc.fatal_errorf "duplicate_function: cannot find function %a"
        Closure_id.print fun_var
    | function_decl -> function_decl
  in
  let env = E.activate_freshening (E.set_never_inline env) in
  let free_vars, specialised_args, function_decls, parameter_types,
      _internal_value_set_of_closures, set_of_closures_env =
    Simplify_aux.prepare_to_simplify_set_of_closures ~env
      ~set_of_closures ~function_decls:set_of_closures.function_decls
      ~freshen:false ~only_for_function_decl:(Some function_decl)
  in
  let function_decl =
    match Variable.Map.find fun_var function_decls.funs with
    | exception Not_found ->
      Misc.fatal_errorf "duplicate_function: cannot find function %a (2)"
        Variable.print fun_var
    | function_decl -> function_decl
  in
  let closure_env =
    Simplify_aux.prepare_to_simplify_closure ~function_decl
      ~free_vars ~specialised_args ~parameter_types
      ~set_of_closures_env
  in
  let cont_type =
    Continuation_approx.create_unknown ~name:function_decl.continuation_param
      ~num_params:1
  in
  let closure_env =
    E.add_continuation closure_env function_decl.continuation_param
      cont_type
  in
  let r = R.create () in
  let body, r =
    E.enter_closure closure_env
      ~closure_id:(Closure_id.wrap fun_var)
      ~inline_inside:false
      ~dbg:function_decl.dbg
      ~f:(fun body_env ->
        assert (E.inside_set_of_closures_declaration
          function_decls.set_of_closures_origin body_env);
        simplify body_env r function_decl.body)
  in
  let _r, _uses =
    R.exit_scope_of_let_cont r env function_decl.continuation_param
  in
  let function_decl =
    Flambda.Function_declaration.create ~params:function_decl.params
      ~continuation_param:function_decl.continuation_param
      ~return_arity:function_decl.return_arity
      ~body ~stub:function_decl.stub ~dbg:function_decl.dbg
      ~inline:function_decl.inline ~specialise:function_decl.specialise
      ~is_a_functor:function_decl.is_a_functor
      ~closure_origin:(Closure_origin.create (Closure_id.wrap new_fun_var))
  in
  function_decl, specialised_args
*)

let run ~never_inline ~allow_continuation_inlining
      ~allow_continuation_specialisation ~backend ~prefixname ~round program =
  let report = !Clflags.inlining_report in
  if never_inline then Clflags.inlining_report := false;
  let initial_env =
    E.create ~never_inline ~allow_continuation_inlining
      ~allow_continuation_specialisation ~backend ~round
      ~simplify_toplevel
      ~simplify_expr:Simplify_expr.simplify_expr
      ~simplify_continuation_use_cannot_inline:
        Simplify_expr.simplify_continuation_use_cannot_inline
  in
  let program, newly_imported_symbols =
    Simplify_program.simplify_program initial_env program
  in
  let imported_symbols =
    (* CR mshinwell: Here and elsewhere, these [disjoint_union] calls should
       raise proper messages *)
    Symbol.Map.disjoint_union program.imported_symbols
      newly_imported_symbols
  in
  let program : Flambda_static.Program.t =
    { program with
      imported_symbols;
    }
  in
  if !Clflags.inlining_report then begin
    let output_prefix = Printf.sprintf "%s.%d" prefixname round in
    Inlining_stats.save_then_forget_decisions ~output_prefix
  end;
  Clflags.inlining_report := report;
  program
