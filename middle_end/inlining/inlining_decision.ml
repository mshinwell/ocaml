(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*     Mark Shinwell, Fu Yong Quah and Leo White, Jane Street Europe      *)
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

module D = Inlining_stats_types.Decision
module E = Simplify_env_and_result.Env
module IT = Inlining_cost.Threshold
module R = Simplify_env_and_result.Result
module S = Inlining_stats_types
module T = Flambda_type
module W = Inlining_cost.Whether_sufficient_benefit

type ('a, 'b) inlining_result =
  | Changed of (Flambda.Expr.t * R.t) * 'a
  | Original of 'b

type 'b good_idea =
  | Try_it
  | Don't_try_it of 'b

(* CR mshinwell: We should take the meet of the types on the function's
   parameters and the arguments when inlining *)

let all_types_not_useful env simples =
  List.for_all (fun (simple : Simple.t) ->
      match simple with
      | Name name ->
        not (T.is_useful (E.get_typing_environment env) (E.find_name env name))
      | Const _ | Tag _ -> false)
    simples

let inline env r ~callee
    ~callee's_closure_id
    ~(function_decl : T.inlinable_function_declaration)
    ~(set_of_closures : T.set_of_closures)
    ~only_use_of_function ~original ~recursive:_
    ~(args : Simple.t list) ~continuation ~size_from_approximation ~dbg
    ~(inline_requested : Flambda.inline_attribute)
    ~(specialise_requested : Flambda.specialise_attribute)
    ~self_call ~fun_cost ~inlining_threshold =
  let toplevel = E.at_toplevel env in
  let branch_depth = E.branch_depth env in
  let unrolling, always_inline, never_inline, env =
    let unrolling = None in
(* CR mshinwell for pchambart: What do we do here?
      E.actively_unrolling env set_of_closures.set_of_closures_origin
    in
*)
    match unrolling with
    | Some count ->
      if count > 0 then
(*
        let env =
          E.continue_actively_unrolling env
            set_of_closures.set_of_closures_origin
        in
*)
        true, true, false, env
      else false, false, true, env
    | None -> begin
        let inline_annotation : Flambda.inline_attribute =
          (* Merge call site annotation and function annotation.
             The call site annotation takes precedence *)
          match inline_requested with
          | Always_inline | Never_inline | Unroll _ -> inline_requested
          | Default_inline ->
            match function_decl.inline with
            | Always_inline -> Always_inline
            | Never_inline -> Never_inline
            | Unroll n -> Unroll n
            | Default_inline -> Default_inline
        in
        match inline_annotation with
        | Always_inline -> false, true, false, env
        | Never_inline -> false, false, true, env
        | Default_inline -> false, false, false, env
        | Unroll count ->
          if count > 0 then
(*
            let env =
              E.start_actively_unrolling env
                set_of_closures.set_of_closures_origin (count - 1)
            in
*)
            true, true, false, env
          else false, false, true, env
      end
  in
  let remaining_inlining_threshold : IT.t =
    if always_inline then inlining_threshold
    else Lazy.force fun_cost
  in
  let try_inlining =
    if function_decl.is_classic_mode then
      Don't_try_it S.Not_inlined.Classic_mode
    else if unrolling then
      Try_it
    else if self_call then
      Don't_try_it S.Not_inlined.Self_call
    else if not (E.inlining_allowed env function_decl.closure_origin) then
      Don't_try_it S.Not_inlined.Unrolling_depth_exceeded
    else if only_use_of_function || always_inline then
      Try_it
    else if never_inline then
      Don't_try_it S.Not_inlined.Annotation
(*
    else if not (E.unrolling_allowed env set_of_closures.set_of_closures_origin)
         && (Lazy.force recursive) then
      Don't_try_it S.Not_inlined.Unrolling_depth_exceeded
*)
    else if remaining_inlining_threshold = IT.Never_inline then
      let threshold =
        match inlining_threshold with
        | IT.Never_inline -> assert false
        | IT.Can_inline_if_no_larger_than threshold -> threshold
      in
      Don't_try_it (S.Not_inlined.Above_threshold threshold)
    else if not (toplevel && branch_depth = 0)
         && all_types_not_useful env args
    then
      (* When all of the arguments to the function being inlined are unknown,
         then we cannot materially simplify the function.  As such, we know
         what the benefit of inlining it would be: just removing the call.
         In this case we may be able to prove the function cannot be inlined
         without traversing its body.
         Note that if the function is sufficiently small, we still have to call
         [simplify], because the body needs freshening before substitution.
      *)
      (* CR-someday mshinwell: (from GPR#8): pchambart writes:

          We may need to think a bit about that. I can't see a lot of
          meaningful examples right now, but there are some cases where some
          optimization can happen even if we don't know anything about the
          shape of the arguments.

          For instance

          let f x y = x

          let g x =
            let y = (x,x) in
            f x y
          let f x y =
            if x = y then ... else ...

          let g x = f x x
      *)
      match size_from_approximation with
      | Some body_size ->
        let wsb =
          let benefit =
            Inlining_cost.Benefit.remove_call Inlining_cost.Benefit.zero
          in
          W.create_estimate
            ~original_size:Inlining_cost.direct_call_size
            ~new_size:body_size
            ~toplevel:(E.at_toplevel env)
            ~branch_depth:(E.branch_depth env)
            ~lifting:function_decl.is_a_functor
            ~round:(E.round env)
            ~benefit
        in
        if (not (W.evaluate wsb)) then begin
          Don't_try_it
            (S.Not_inlined.Without_subfunctions wsb)
        end else Try_it
      | None ->
        (* The function is definitely too large to inline given that we don't
           have any approximations for its arguments.  Further, the body
           should already have been simplified (inside its declaration), so
           we also expect no gain from the code below that permits inlining
           inside the body. *)
        Don't_try_it S.Not_inlined.No_useful_approximations
    else begin
      (* There are useful approximations, so we should simplify. *)
      Try_it
    end
  in
  match try_inlining with
  | Don't_try_it decision -> Original decision
  | Try_it ->
    let r =
      R.set_inlining_threshold r (Some remaining_inlining_threshold)
    in
    let cont_usage_snapshot = R.snapshot_continuation_uses r in
    let body, r_inlined =
      (* First we construct the code that would result from copying the body of
         the function, without doing any further inlining upon it, to the call
         site. *)
(*
Format.eprintf "Inlining application of %a whose body is:@ \n%a\n%!"
  Variable.print callee Flambda.Expr.print function_decl.body;
*)
      Inlining_transforms.inline_by_copying_function_body ~env
        ~r:(R.reset_benefit r)
        ~set_of_closures
        ~callee
        ~inline_requested
        ~specialise_requested
        ~callee's_closure_id
        ~function_decl
        ~args
        ~continuation
        ~dbg
    in
    let num_direct_applications_seen =
      (R.num_direct_applications r_inlined) - (R.num_direct_applications r)
    in
    assert (num_direct_applications_seen >= 0);
    let keep_inlined_version decision =
      (* Inlining the body of the function was sufficiently beneficial that we
         will keep it, replacing the call site.  We continue by allowing
         further inlining within the inlined copy of the body. *)
      let r_inlined =
        (* The meaning of requesting inlining is that the user ensure
           that the function has a benefit of at least its size. It is not
           added to the benefit exposed by the inlining because the user should
           have taken that into account before annotating the function. *)
        if always_inline then
          R.map_benefit r_inlined
            (Inlining_cost.Benefit.max ~round:(E.round env)
               Inlining_cost.Benefit.(requested_inline ~size_of:body zero))
        else r_inlined
      in
      let r_inlined =
        R.roll_back_continuation_uses r_inlined cont_usage_snapshot
      in
      let r =
        R.map_benefit r_inlined (Inlining_cost.Benefit.(+) (R.benefit r))
      in
      let env = E.note_entering_inlined env in
(*
      let env =
        (* We decrement the unrolling count even if the function is not
           recursive to avoid having to check whether or not it is recursive *)
        E.inside_unrolled_function env set_of_closures.set_of_closures_origin
      in
*)
      let env = E.inside_inlined_function env function_decl.closure_origin in
      let env =
        if E.inlining_level env = 0
           (* If the function was considered for inlining without considering
              its sub-functions, and it is not below another inlining choice,
              then we are certain that this code will be kept. *)
        then env
        else E.inlining_level_up env
      in
      Changed (((E.simplify_expr env) env r body), decision)
    in
    if always_inline then
      keep_inlined_version S.Inlined.Annotation
    else if only_use_of_function then
      keep_inlined_version S.Inlined.Decl_local_to_application
    else begin
      let wsb =
        W.create ~original body
          ~toplevel:(E.at_toplevel env)
          ~branch_depth:(E.branch_depth env)
          ~lifting:function_decl.is_a_functor
          ~round:(E.round env)
          ~benefit:(R.benefit r_inlined)
      in
      if W.evaluate wsb then
        keep_inlined_version (S.Inlined.Without_subfunctions wsb)
      else if num_direct_applications_seen < 1 then begin
      (* Inlining the body of the function did not appear sufficiently
         beneficial; however, it may become so if we inline within the body
         first.  We try that next, unless it is known that there were
         no direct applications in the simplified body computed above, meaning
         no opportunities for inlining. *)
        Original (S.Not_inlined.Without_subfunctions wsb)
      end else begin
        let env = E.inlining_level_up env in
        let env = E.note_entering_inlined env in
(*
        let env =
          (* We decrement the unrolling count even if the function is recursive
             to avoid having to check whether or not it is recursive *)
          E.inside_unrolled_function env set_of_closures.set_of_closures_origin
        in
*)
        (* CR mshinwell: note: this next line was missing in the old Flambda *)
        let env = E.inside_inlined_function env function_decl.closure_origin in
        let r_inlined =
          R.roll_back_continuation_uses r_inlined cont_usage_snapshot
        in
        let body, r_inlined = (E.simplify_expr env) env r_inlined body in
        let wsb_with_subfunctions =
          W.create ~original body
            ~toplevel:(E.at_toplevel env)
            ~branch_depth:(E.branch_depth env)
            ~lifting:function_decl.is_a_functor
            ~round:(E.round env)
            ~benefit:(R.benefit r_inlined)
        in
        if W.evaluate wsb_with_subfunctions then begin
          let res =
            (body, R.map_benefit r_inlined
                     (Inlining_cost.Benefit.(+) (R.benefit r)))
          in
          let decision =
            S.Inlined.With_subfunctions (wsb, wsb_with_subfunctions)
          in
          Changed (res, decision)
        end
        else begin
          (* r_inlined contains an approximation that may be invalid for the
             untransformed expression: it may reference functions that only
             exists if the body of the function is in fact inlined.
             If the function approximation contained an approximation that
             does not depend on the actual values of its arguments, it
             could be returned instead of [T.value_unknown]. *)
          let decision =
            S.Not_inlined.With_subfunctions (wsb, wsb_with_subfunctions)
          in
          Original decision
        end
      end
    end

(*
(* CR mshinwell: Stop specialising functions when there is no increase in
   precision of the argument approximations.  (Will act as a backstop to
   prevent unbounded recursion during specialisation.) *)
let specialise env r ~callee
      ~(function_decls : Flambda.Function_declarations.t)
      ~(function_decl : Flambda.Function_declaration.t)
      ~callee's_closure_id
      ~(value_set_of_closures : Flambda_type.value_set_of_closures)
      ~args ~args_approxs ~continuation ~dbg ~simplify ~original ~recursive
      ~self_call ~inlining_threshold ~fun_cost
      ~inline_requested ~specialise_requested =
  let bound_vars =
    lazy
      (let closures_required =
         Flambda_utils.closures_required_by_entry_point
           ~entry_point:callee's_closure_id
           ~backend:(E.backend env)
           function_decls
       in
       let bound_vars_required =
         Variable.Set.fold (fun fun_var bound_vars_required ->
               let bound_vars =
                 Flambda_utils.variables_bound_by_the_closure
                   (Closure_id.wrap fun_var)
                   function_decls
               in
               Variable.Set.union bound_vars bound_vars_required)
           closures_required
           Variable.Set.empty
       in
       Var_within_closure.Map.filter (fun var _approx ->
           Variable.Set.mem (Var_within_closure.unwrap var) bound_vars_required)
         value_set_of_closures.bound_vars)
  in
  let invariant_params =
    if Lazy.force recursive then
      value_set_of_closures.invariant_params
    else
      lazy (List.fold_left (fun invariant_params param ->
          Variable.Map.add param (Variable.Set.singleton param)
            invariant_params)
        Variable.Map.empty
        (Parameter.List.vars function_decl.params))
  in
  let has_no_useful_approxes =
    lazy
      (List.for_all2
         (fun id approx ->
            not ((T.useful approx)
                 && Variable.Map.mem id (Lazy.force invariant_params)))
         (Parameter.List.vars function_decl.params) args_approxs)
  in
  let always_specialise, never_specialise =
    (* Merge call site annotation and function annotation.
       The call site annotation takes precedence *)
    match (specialise_requested : Flambda.specialise_attribute) with
    | Always_specialise -> true, false
    | Never_specialise -> false, true
    | Default_specialise -> begin
        match (function_decl.specialise : Flambda.specialise_attribute) with
        | Always_specialise -> true, false
        | Never_specialise -> false, true
        | Default_specialise -> false, false
      end
  in
  let remaining_inlining_threshold : Inlining_cost.Threshold.t =
    if always_specialise then inlining_threshold
    else Lazy.force fun_cost
  in
  let try_specialising =
    (* Try specialising if the function:
       - is closed (it and all other members of the set of closures on which
         it depends); and
       - has useful approximations for some invariant parameters. *)
    if set_of_closures.is_classic_mode then
      Don't_try_it S.Not_specialised.Classic_mode
    else if self_call then
      Don't_try_it S.Not_specialised.Self_call
    else if always_specialise && not (Lazy.force has_no_useful_approxes) then
      Try_it
    else if never_specialise then
      Don't_try_it S.Not_specialised.Annotation
    else if remaining_inlining_threshold = T.Never_inline then
      let threshold =
        match inlining_threshold with
        | T.Never_inline -> assert false
        | T.Can_inline_if_no_larger_than threshold -> threshold
      in
      Don't_try_it (S.Not_specialised.Above_threshold threshold)
    else if not (Var_within_closure.Map.is_empty (Lazy.force bound_vars)) then
      Don't_try_it S.Not_specialised.Not_closed
    else if not (Lazy.force recursive) then
      Don't_try_it S.Not_specialised.Not_recursive
    else if Variable.Map.is_empty (Lazy.force invariant_params) then
      Don't_try_it S.Not_specialised.No_invariant_parameters
    else if Lazy.force has_no_useful_approxes then
      Don't_try_it S.Not_specialised.No_useful_approximations
    else Try_it
  in
  match try_specialising with
  | Don't_try_it decision ->
      Original decision
  | Try_it -> begin
      let cont_usage_snapshot = R.snapshot_continuation_uses r in
      let r =
        R.set_inlining_threshold r (Some remaining_inlining_threshold)
      in
      let copied_function_declaration =
        Inlining_transforms.inline_by_copying_function_declaration ~env
          ~r:(R.reset_benefit r) ~callee
          ~function_decls ~callee's_closure_id ~function_decl
          ~args ~args_approxs ~continuation
          ~invariant_params:value_set_of_closures.invariant_params
          ~specialised_args:value_set_of_closures.specialised_args
          ~direct_call_surrogates:value_set_of_closures.direct_call_surrogates
          ~dbg ~simplify ~inline_requested
      in
      match copied_function_declaration with
      | Some (expr, r_inlined) ->
        let wsb =
          W.create ~original expr
            ~toplevel:false
            ~branch_depth:(E.branch_depth env)
            ~lifting:false
            ~round:(E.round env)
            ~benefit:(R.benefit r_inlined)
        in
        let env =
          (* CR-someday lwhite: could avoid calculating this if stats is turned
             off *)
          let closure_ids =
            Closure_id.Set.of_list (
              List.map Closure_id.wrap
                (Variable.Set.elements (Variable.Map.keys function_decls.funs)))
          in
          E.note_entering_specialised env ~closure_ids
        in
        if always_specialise || W.evaluate wsb then begin
          let r_inlined =
            if always_specialise then
              R.map_benefit r_inlined
                (Inlining_cost.Benefit.max ~round:(E.round env)
                   Inlining_cost.Benefit.(requested_inline ~size_of:expr zero))
            else r_inlined
          in
          let r_inlined =
            R.roll_back_continuation_uses r_inlined cont_usage_snapshot
          in
          let r =
            R.map_benefit r_inlined (Inlining_cost.Benefit.(+) (R.benefit r))
          in
          let closure_env =
            let env =
              if E.inlining_level env = 0
               (* If the function was considered for specialising without
                  considering its sub-functions, and it is not below another
                  inlining choice, then we are certain that this code will
                  be kept. *)
              then env
              else E.inlining_level_up env
            in
              E.set_never_inline_outside_closures env
          in
          let application_env = E.set_never_inline_inside_closures env in
          let expr, r = simplify closure_env r expr in
          let r = R.roll_back_continuation_uses r cont_usage_snapshot in
          let res = simplify application_env r expr in
          let decision =
            if always_specialise then S.Specialised.Annotation
            else S.Specialised.Without_subfunctions wsb
          in
          Changed (res, decision)
        end else begin
          let closure_env =
            let env = E.inlining_level_up env in
            E.set_never_inline_outside_closures env
          in
          let r_inlined =
            R.roll_back_continuation_uses r_inlined cont_usage_snapshot
          in
          let expr, r_inlined = simplify closure_env r_inlined expr in
          let wsb_with_subfunctions =
            W.create ~original expr
              ~toplevel:false
              ~branch_depth:(E.branch_depth env)
              ~lifting:false
              ~round:(E.round env)
              ~benefit:(R.benefit r_inlined)
          in
          if W.evaluate wsb_with_subfunctions then begin
            let r_inlined =
              R.roll_back_continuation_uses r_inlined cont_usage_snapshot
            in
            let r =
              R.map_benefit r_inlined
                       (Inlining_cost.Benefit.(+) (R.benefit r))
            in
            let application_env = E.set_never_inline_inside_closures env in
(*
Format.eprintf "Application env: %a@ \nExpression:\n%a%!"
  E.print application_env Flambda.Expr.print expr;
*)
            let res = simplify application_env r expr in
            let decision =
              S.Specialised.With_subfunctions (wsb, wsb_with_subfunctions)
            in
            Changed (res, decision)
          end else begin
            let decision =
              S.Not_specialised.Not_beneficial (wsb, wsb_with_subfunctions)
            in
            Original decision
          end
        end
      | None ->
        let decision = S.Not_specialised.No_useful_approximations in
        Original decision
    end
*)

let for_call_site ~env ~r
      ~callee ~callee's_closure_id
      ~(function_decl : Flambda_type.inlinable_function_declaration)
      ~(set_of_closures : Flambda_type.set_of_closures)
      ~args ~arg_tys ~continuation ~exn_continuation ~dbg ~inline_requested
      ~specialise_requested =
  if List.length args <> List.length arg_tys then begin
    Misc.fatal_error "Inlining_decision.for_call_site: inconsistent lengths \
        of [args] and [arg_tys]"
  end;
  (* Remove unroll attributes from functions we are already actively
     unrolling, otherwise they'll be unrolled again next round. *)
  let inline_requested : Flambda.inline_attribute =
    match (inline_requested : Flambda.inline_attribute) with
    | Unroll _ ->
      let unrolling = None in
(*
      let unrolling =
        E.actively_unrolling env set_of_closures.set_of_closures_origin
      in
*)
      begin match unrolling with
      | Some _ -> Default_inline
      | None -> inline_requested
      end
    | Always_inline | Default_inline | Never_inline -> inline_requested
  in
  let return_arity = List.map (fun ty -> T.kind ty) function_decl.result in
  let return_cont_params =
    (* CR mshinwell: Move into utility function *)
    List.mapi (fun index ty ->
        let name = Format.sprintf "return_cont_param%d" index in
        let var = Variable.create name in
        let param = Parameter.wrap var in
        Flambda.Typed_parameter.create param ty)
      function_decl.result
  in
  let original_apply : Flambda.Apply.t =
    { continuation;
      exn_continuation;
      func = callee;
      args;
      call_kind = Function (Direct {
        closure_id = callee's_closure_id;
        return_arity;
      });
      dbg;
      inline = inline_requested;
      specialise = specialise_requested;
    }
  in
  let original_expr : Flambda.Expr.t = Apply original_apply in
  let original r =
    let continuation, r =
      (E.simplify_continuation_use_cannot_inline env) env r
        continuation ~params:return_cont_params
    in
    let original_expr : Flambda.Expr.t =
      Apply {
        original_apply with
        continuation;
      }
    in
    original_expr, r
  in
(*
Format.eprintf "Application of %a (%a).\n%!"
  Closure_id.print callee's_closure_id
  Variable.print_list args;
*)
(*
Format.eprintf "Application of %a (%a): inline_requested=%a self_call=%b\n%!"
  Closure_id.print callee's_closure_id
  Variable.print_list args
  Printlambda.apply_inlined_attribute inline_requested self_call;
*)
  let original_r = R.seen_direct_application r in
  if function_decl.stub then
    Inlining_transforms.inline_by_copying_function_body ~env ~r
      ~set_of_closures ~callee ~callee's_closure_id
      ~specialise_requested ~inline_requested ~function_decl ~args
      ~continuation ~dbg
  else if E.never_inline env then
    (* This case only occurs when examining the body of a stub function
       but not in the context of inlining said function.  As such, there
       is nothing to do here (and no decision to report). *)
    original original_r
  else if function_decl.is_classic_mode then begin
    let env =
      E.note_entering_call env
        ~closure_id:callee's_closure_id ~dbg:dbg
    in
    let simpl =
      let self_call = false in (* XXX *)
(*
      let self_call =
        E.inside_set_of_closures_declaration
          set_of_closures.set_of_closures_origin env
      in
*)
      let try_inlining =
        if self_call then
          Don't_try_it S.Not_inlined.Self_call
        else if not (E.inlining_allowed env function_decl.closure_origin) then
          Don't_try_it S.Not_inlined.Unrolling_depth_exceeded
        else begin
          (* There are useful approximations, so we should simplify. *)
          Try_it
        end
      in
      match try_inlining with
      | Don't_try_it decision -> Original decision
      | Try_it ->
        let body, r =
          Inlining_transforms.inline_by_copying_function_body ~env
            ~r ~set_of_closures ~callee
            ~callee's_closure_id ~specialise_requested ~inline_requested
            ~function_decl ~args ~continuation ~dbg
        in
        let env = E.note_entering_inlined env in
(*
        let env =
          (* We decrement the unrolling count, even if the function is not
             recursive, to avoid having to check whether or not it is
             recursive. *)
          E.inside_unrolled_function env set_of_closures.set_of_closures_origin
        in
*)
        let env =
          E.inside_inlined_function env function_decl.closure_origin
        in
        Changed (((E.simplify_expr env) env r body), S.Inlined.Classic_mode)
    in
    let res, decision =
      match simpl with
      | Original decision ->
        let decision =
          S.Decision.Unchanged (S.Not_specialised.Classic_mode, decision)
        in
        original original_r, decision
      | Changed ((expr, r), decision) ->
        let max_inlining_threshold =
          if E.at_toplevel env then
            Simplify_aux.initial_inlining_toplevel_threshold
              ~round:(E.round env)
          else
            Simplify_aux.initial_inlining_threshold
              ~round:(E.round env)
        in
        let raw_inlining_threshold = R.inlining_threshold r in
        let unthrottled_inlining_threshold =
          match raw_inlining_threshold with
          | None -> max_inlining_threshold
          | Some inlining_threshold -> inlining_threshold
        in
        let inlining_threshold =
          IT.min unthrottled_inlining_threshold max_inlining_threshold
        in
        let inlining_threshold_diff =
          IT.sub unthrottled_inlining_threshold inlining_threshold
        in
        let res =
          if E.inlining_level env = 0
          then expr, R.set_inlining_threshold r raw_inlining_threshold
          else expr, R.add_inlining_threshold r inlining_threshold_diff
        in
        res, S.Decision.Inlined (S.Not_specialised.Classic_mode, decision)
    in
    E.record_decision env decision;
    res
  end else begin
    let env = E.unset_never_inline_inside_closures env in
    let env =
      E.note_entering_call env
        ~closure_id:callee's_closure_id ~dbg:dbg
    in
    let max_level =
      Clflags.Int_arg_helper.get ~key:(E.round env) !Clflags.inline_max_depth
    in
    let raw_inlining_threshold = R.inlining_threshold r in
    let max_inlining_threshold =
      if E.at_toplevel env then
        Simplify_aux.initial_inlining_toplevel_threshold
          ~round:(E.round env)
      else
        Simplify_aux.initial_inlining_threshold ~round:(E.round env)
    in
    let unthrottled_inlining_threshold =
      match raw_inlining_threshold with
      | None -> max_inlining_threshold
      | Some inlining_threshold -> inlining_threshold
    in
    let inlining_threshold =
      IT.min unthrottled_inlining_threshold max_inlining_threshold
    in
    let inlining_threshold_diff =
      IT.sub unthrottled_inlining_threshold inlining_threshold
    in
    let inlining_prevented =
      match inlining_threshold with
      | Never_inline -> true
      | Can_inline_if_no_larger_than _ -> false
    in
    let simpl =
      if inlining_prevented then
        Original (D.Prevented Function_prevented_from_inlining)
      else if E.inlining_level env >= max_level then
        Original (D.Prevented Level_exceeded)
      else begin
        let self_call = false in (* XXX *)
(*
          E.inside_set_of_closures_declaration
            set_of_closures.set_of_closures_origin env
        in
*)
        let fun_cost =
          lazy
            (Inlining_cost.can_try_inlining function_decl.body
                inlining_threshold
                ~number_of_arguments:(List.length function_decl.params)
                (* CR-someday mshinwell: for the moment, this is None, since
                   the Inlining_cost code isn't checking sizes up to the max
                   inlining threshold---this seems to take too long. *)
                ~size_from_approximation:None)
        in
        (* CR mshinwell for pchambart and lwhite: Think about how to do
           specialisation in the new world *)
(* Note: fquah's diff for specialising has not been applied here yet. *)
        let recursive = lazy false in  (* XXX *)
(*
        let fun_var =
          U.find_declaration_variable callee's_closure_id function_decls
        in
          lazy
            (Variable.Set.mem fun_var
               ((Find_recursive_functions.in_function_declarations
                  function_decls
                  ~backend:(E.backend env))))
        in
*)
        let cont_usage_snapshot = R.snapshot_continuation_uses r in
        let specialise_result =
          Original (S.Not_specialised.No_useful_approximations)  (* XXX *)
(*
          specialise env r ~callee ~function_decls ~recursive
            ~callee's_closure_id ~function_decl ~value_set_of_closures
            ~args ~arg_tys ~continuation ~dbg ~simplify
            ~original:original_expr
            ~inline_requested ~specialise_requested ~fun_cost ~self_call
            ~inlining_threshold
*)
        in
        match specialise_result with
        | Changed (res, spec_reason) ->
          Changed (res, D.Specialised spec_reason)
        | Original spec_reason ->
          let only_use_of_function = false in
          (* If we didn't specialise then try inlining *)
          let size_from_approximation = Some 42 in
(* XXX
            match
              Variable.Map.find fun_var (Lazy.force value_set_of_closures.size)
            with
            | size -> size
            | exception Not_found ->
                Misc.fatal_errorf "Approximation does not give a size for the \
                  function having fun_var %a.  value_set_of_closures: %a"
                  Variable.print fun_var
                  T.print_value_set_of_closures value_set_of_closures
          in
*)
          let r = R.roll_back_continuation_uses r cont_usage_snapshot in
          let inline_result =
            inline env r ~callee
              ~callee's_closure_id ~function_decl ~set_of_closures
              ~only_use_of_function ~original:original_expr ~recursive
              ~inline_requested ~specialise_requested ~args ~continuation
              ~size_from_approximation ~dbg ~fun_cost ~self_call
              ~inlining_threshold
          in
          match inline_result with
          | Changed (res, inl_reason) ->
            Changed (res, D.Inlined (spec_reason, inl_reason))
          | Original inl_reason ->
            Original (D.Unchanged (spec_reason, inl_reason))
      end
    in
    let res, decision =
      match simpl with
      | Original decision ->
        original original_r, decision
      | Changed ((expr, r), decision) ->
        let res =
          if E.inlining_level env = 0
          then expr, R.set_inlining_threshold r raw_inlining_threshold
          else expr, R.add_inlining_threshold r inlining_threshold_diff
        in
        res, decision
    in
    E.record_decision env decision;
    res
  end

(* We do not inline inside stubs, which are always inlined at their call site.
   Inlining inside the declaration of a stub could result in more code than
   expected being inlined (e.g. the body of a function that was transformed
   by adding the stub). *)
let should_inline_inside_declaration (decl : Flambda.Function_declaration.t) =
  not decl.stub
