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

let rebuild_switch dacc ~arms ~scrutinee ~scrutinee_ty uacc
      ~after_rebuild =
  let new_let_conts, arms, identity_arms, not_arms =
    Target_imm.Map.fold
      (fun arm (action, use_id, arity)
           (new_let_conts, arms, identity_arms, not_arms) ->
        match
          Simplify_common.add_wrapper_for_switch_arm uacc action
            ~use_id (Flambda_arity.With_subkinds.of_arity arity)
        with
        | Apply_cont action ->
          let action =
            (* First try to absorb any [Apply_cont] expression that forms the
               entirety of the arm's action (via an intermediate zero-arity
               continuation without trap action) into the [Switch] expression
               itself. *)
            if not (Apply_cont.is_goto action) then Some action
            else
              let cont = Apply_cont.continuation action in
              match UE.find_continuation (UA.uenv uacc) cont with
              | Inline { arity = _; handler; }
              | Unknown { arity = _; handler = Some handler; } ->
                Continuation_params_and_handler.pattern_match
                  (Continuation_handler.params_and_handler handler)
                  ~f:(fun params ~handler ->
                    assert (List.length params = 0);
                    match Expr.descr handler with
                    | Apply_cont action -> Some action
                    | Let _ | Let_cont _ | Apply _
                    | Switch _ | Invalid _ -> Some action)
              | Unknown _ -> Some action
              | Unreachable _ -> None
          in
          begin match action with
          | None ->
            (* The destination is unreachable; delete the [Switch] arm. *)
            new_let_conts, arms, identity_arms, not_arms
          | Some action ->
            let normal_case ~identity_arms ~not_arms =
              let arms = Target_imm.Map.add arm action arms in
              new_let_conts, arms, identity_arms, not_arms
            in
            (* Now check to see if the arm is of a form that might mean the
               whole [Switch] is either the identity or a boolean NOT. *)
            match Apply_cont.to_one_arg_without_trap_action action with
            | None -> normal_case ~identity_arms ~not_arms
            | Some arg ->
              (* CR-someday mshinwell: Maybe this check should be generalised
                 e.g. to detect
                   | 0 -> apply_cont k x y 1
                   | 1 -> apply_cont k x y 0
              *)
              let [@inline always] const arg =
                match Reg_width_const.descr arg with
                | Tagged_immediate arg ->
                  if Target_imm.equal arm arg then
                    let identity_arms =
                      Target_imm.Map.add arm action identity_arms
                    in
                    normal_case ~identity_arms ~not_arms
                  else if
                    (Target_imm.equal arm Target_imm.bool_true
                      && Target_imm.equal arg Target_imm.bool_false)
                    ||
                      (Target_imm.equal arm Target_imm.bool_false
                        && Target_imm.equal arg Target_imm.bool_true)
                  then
                    let not_arms = Target_imm.Map.add arm action not_arms in
                    normal_case ~identity_arms ~not_arms
                  else
                    normal_case ~identity_arms ~not_arms
                | Naked_immediate _ | Naked_float _ | Naked_int32 _
                | Naked_int64 _ | Naked_nativeint _ ->
                  normal_case ~identity_arms ~not_arms
              in
              Simple.pattern_match arg ~const
                ~name:(fun _ -> normal_case ~identity_arms ~not_arms)
          end
        | New_wrapper (new_cont, new_handler) ->
          let new_let_cont = new_cont, new_handler in
          let new_let_conts = new_let_cont :: new_let_conts in
          let action = Apply_cont.goto new_cont in
          let arms = Target_imm.Map.add arm action arms in
          new_let_conts, arms, identity_arms, not_arms)
      arms
      ([], Target_imm.Map.empty, Target_imm.Map.empty, Target_imm.Map.empty)
  in
  let switch_is_identity =
    let arm_discrs = Target_imm.Map.keys arms in
    let identity_arms_discrs = Target_imm.Map.keys identity_arms in
    if not (Target_imm.Set.equal arm_discrs identity_arms_discrs) then
      None
    else
      Target_imm.Map.data identity_arms
      |> List.map Apply_cont.continuation
      |> Continuation.Set.of_list
      |> Continuation.Set.get_singleton
  in
  let switch_is_boolean_not =
    let arm_discrs = Target_imm.Map.keys arms in
    let not_arms_discrs = Target_imm.Map.keys not_arms in
    if (not (Target_imm.Set.equal arm_discrs Target_imm.all_bools))
      || (not (Target_imm.Set.equal arm_discrs not_arms_discrs))
    then
      None
    else
      Target_imm.Map.data not_arms
      |> List.map Apply_cont.continuation
      |> Continuation.Set.of_list
      |> Continuation.Set.get_singleton
  in
  let create_tagged_scrutinee k =
    let bound_to = Variable.create "tagged_scrutinee" in
    let bound_vars =
      Bindable_let_bound.singleton (VB.create bound_to NM.normal)
    in
    let named =
      Named.create_prim (Unary (Box_number Untagged_immediate, scrutinee))
        Debuginfo.none
    in
    let dacc =
      (* Disable inconstant lifting *)
      DA.map_denv dacc ~f:DE.set_not_at_unit_toplevel
    in
    let { Simplify_named. bindings_outermost_first = bindings; dacc = _; } =
      Simplify_named.simplify_named dacc bound_vars named
    in
    let body = k ~tagged_scrutinee:(Simple.var bound_to) in
    Simplify_common.bind_let_bound ~bindings ~body, uacc
  in
  let body, uacc =
    match switch_is_identity with
    | Some dest ->
      create_tagged_scrutinee (fun ~tagged_scrutinee ->
        let apply_cont =
          Apply_cont.create dest ~args:[tagged_scrutinee] ~dbg:Debuginfo.none
        in
        Expr.create_apply_cont apply_cont)
    | None ->
      match switch_is_boolean_not with
      | Some dest ->
        create_tagged_scrutinee (fun ~tagged_scrutinee ->
          let not_scrutinee = Variable.create "not_scrutinee" in
          let apply_cont =
            Apply_cont.create dest ~args:[Simple.var not_scrutinee]
              ~dbg:Debuginfo.none
          in
          Expr.create_let (VB.create not_scrutinee NM.normal)
            (Named.create_prim (P.Unary (Boolean_not, tagged_scrutinee))
              Debuginfo.none)
            (Expr.create_apply_cont apply_cont))
      | None ->
        let expr = Expr.create_switch ~scrutinee ~arms in
        if !Clflags.flambda_invariant_checks
          && Simple.is_const scrutinee
          && Target_imm.Map.cardinal arms > 1
        then begin
          Misc.fatal_errorf "[Switch] with constant scrutinee (type: %a) \
              should have been simplified away:@ %a"
            T.print scrutinee_ty
            Expr.print expr
        end;
        expr, uacc
  in
  let expr =
    List.fold_left (fun body (new_cont, new_handler) ->
        Let_cont.create_non_recursive new_cont new_handler ~body)
      body
      new_let_conts
  in
  after_rebuild expr uacc

let simplify_switch dacc switch ~down_to_up =
  let module AC = Apply_cont in
  let min_name_mode = Name_mode.normal in
  let scrutinee = Switch.scrutinee switch in
  match S.simplify_simple dacc scrutinee ~min_name_mode with
  | Bottom, _ty ->
    down_to_up dacc ~rebuild:Simplify_common.rebuild_invalid
  | Ok scrutinee, scrutinee_ty ->
    let arms, dacc =
      let typing_env_at_use = DA.typing_env dacc in
      Target_imm.Map.fold (fun arm action (arms, dacc) ->
          let shape =
            let imm = Target_imm.int (Target_imm.to_targetint arm) in
            T.this_naked_immediate imm
          in
          match T.meet typing_env_at_use scrutinee_ty shape with
          | Bottom -> arms, dacc
          | Ok (_meet_ty, env_extension) ->
            let env_at_use =
              TE.add_env_extension typing_env_at_use env_extension
              |> DE.with_typing_env (DA.denv dacc)
            in
            let args = AC.args action in
            match args with
            | [] ->
              let dacc, rewrite_id =
                DA.record_continuation_use dacc (AC.continuation action)
                  Non_inlinable ~env_at_use ~arg_types:[]
              in
              let arms = Target_imm.Map.add arm (action, rewrite_id, []) arms in
              arms, dacc
            | _::_ ->
              let min_name_mode = Name_mode.normal in
              match S.simplify_simples dacc args ~min_name_mode with
              | _, Bottom -> arms, dacc
              | _changed, Ok args_with_types ->
                let args, arg_types = List.split args_with_types in
                let dacc, rewrite_id =
                  DA.record_continuation_use dacc (AC.continuation action)
                    Non_inlinable ~env_at_use ~arg_types
                in
                let arity = List.map T.kind arg_types in
                let action = Apply_cont.update_args action ~args in
                let arms =
                  Target_imm.Map.add arm (action, rewrite_id, arity) arms
                in
                arms, dacc)
        (Switch.arms switch)
        (Target_imm.Map.empty, dacc)
    in
    down_to_up dacc
      ~rebuild:(rebuild_switch dacc ~arms ~scrutinee ~scrutinee_ty)
