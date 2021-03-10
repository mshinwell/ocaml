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

open! Flambda.Import

module BLB = Bindable_let_bound
module LC = Lifted_constant
module LCS = Lifted_constant_state
module P = Flambda_primitive
module UA = Upwards_acc
module VB = Var_in_binding_pos

(* The constructed values of this type aren't currently used, but will be
   needed when we import the Flambda 1 inliner. *)
(* CR mshinwell: This should turn into a full "benefit" type *)
type let_creation_result =
  | Have_deleted of Named.t
  | Nothing_deleted

let create_let uacc (bound_vars : BLB.t) defining_expr
      ~free_names_of_defining_expr ~body ~cost_metrics_of_defining_expr =
  (* The name occurrences component of [uacc] is expected to be in the state
     described in the comment at the top of [Simplify_let.rebuild_let]. *)
  let generate_phantom_lets =
    !Clflags.debug && !Clflags.Flambda.Expert.phantom_lets
  in
  let free_names_of_body = UA.name_occurrences uacc in
  let bound_vars, keep_binding, let_creation_result =
    let greatest_name_mode =
      match bound_vars with
      | Singleton bound_var ->
        (* We avoid the closure allocation (below) in this case. *)
        Name_occurrences.greatest_name_mode_var free_names_of_body
          (VB.var bound_var)
      | Set_of_closures _ | Symbols _ ->
        BLB.fold_all_bound_vars bound_vars ~init:Name_mode.Or_absent.absent
          ~f:(fun (greatest_name_mode : Name_mode.Or_absent.t) bound_var ->
            let name_mode =
              Name_occurrences.greatest_name_mode_var free_names_of_body
                (VB.var bound_var)
            in
            match name_mode, greatest_name_mode with
            | Absent, Absent -> Name_mode.Or_absent.absent
            | Absent, Present _ -> greatest_name_mode
            | Present _, Absent -> name_mode
            | Present name_mode, Present greatest_name_mode ->
              Name_mode.max_in_terms name_mode greatest_name_mode
              |> Name_mode.Or_absent.present)
    in
    let declared_name_mode = BLB.name_mode bound_vars in
    begin match
      Name_mode.Or_absent.compare_partial_order
         greatest_name_mode
         (Name_mode.Or_absent.present declared_name_mode)
    with
    | None -> ()
    | Some c ->
      if c <= 0 then ()
      else
        Misc.fatal_errorf "[Let]-binding declares variable(s) %a (mode %a) to \
            be bound to@ %a,@ but there exist occurrences for such variable(s) \
            at a higher mode@ (>= %a)@ in the body (free names %a):@ %a"
          BLB.print bound_vars
          Name_mode.print declared_name_mode
          Named.print defining_expr
          Name_mode.Or_absent.print greatest_name_mode
          Name_occurrences.print free_names_of_body
          Expr.print body
    end;
    if not (Named.at_most_generative_effects defining_expr) then begin
      if not (Name_mode.is_normal declared_name_mode)
      then begin
        Misc.fatal_errorf "Cannot [Let]-bind non-normal variable(s) to \
            a [Named] that has more than generative effects:@ %a@ =@ %a"
          BLB.print bound_vars
          Named.print defining_expr
      end;
      bound_vars, Some Name_mode.normal, Nothing_deleted
    end else begin
      let has_uses = Name_mode.Or_absent.is_present greatest_name_mode in
      let user_visible =
        BLB.exists_all_bound_vars bound_vars ~f:(fun bound_var ->
          Variable.user_visible (VB.var bound_var))
      in
      let will_delete_binding =
        (* CR mshinwell: This should detect whether there is any
           provenance info associated with the variable.  If there isn't, the
           [Let] can be deleted even if debugging information is being
           generated. *)
        not (has_uses || (generate_phantom_lets && user_visible))
      in
      if will_delete_binding then begin
        bound_vars, None, Have_deleted defining_expr
      end else
        let name_mode =
          match greatest_name_mode with
          | Absent -> Name_mode.phantom
          | Present name_mode -> name_mode
        in
        assert (Name_mode.can_be_in_terms name_mode);
        let bound_vars = BLB.with_name_mode bound_vars name_mode in
        if Name_mode.is_normal name_mode then
          bound_vars, Some name_mode, Nothing_deleted
        else
          bound_vars, Some name_mode, Have_deleted defining_expr
    end
  in
  (* CR mshinwell: When leaving behind phantom lets, maybe we should turn
     the defining expressions into simpler ones by using the type, if possible.
     For example an Unbox_naked_int64 or something could potentially turn
     into a variable.  This defining expression usually never exists as
     the types propagate the information forward.
     mshinwell: this might be done now in Simplify_named, check. *)
  match keep_binding with
  | None -> body, uacc, let_creation_result
  | Some name_mode ->
    let free_names_of_body = UA.name_occurrences uacc in
    let free_names_of_defining_expr =
      if not generate_phantom_lets then (* CR mshinwell: refine condition *)
        free_names_of_defining_expr
      else
        Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
          free_names_of_defining_expr name_mode
    in
    let free_names_of_let =
      let without_bound_vars =
        BLB.fold_all_bound_vars bound_vars ~init:free_names_of_body
          ~f:(fun free_names bound_var ->
            Name_occurrences.remove_var free_names (VB.var bound_var))
      in
      Name_occurrences.union without_bound_vars free_names_of_defining_expr
    in
    let is_phantom = Name_mode.is_phantom name_mode in
    let uacc =
      (* CR mshinwell: This is reallocating UA twice on every [Let] *)
      UA.with_name_occurrences uacc ~name_occurrences:free_names_of_let
      |> UA.cost_metrics_add
           ~added:(Cost_metrics.increase_due_to_let_expr
              ~is_phantom ~cost_metrics_of_defining_expr)
    in
    let let_expr =
      Let.create bound_vars defining_expr ~body
        ~free_names_of_body:(Known free_names_of_body)
    in
    Expr.create_let let_expr, uacc, Nothing_deleted

let make_new_let_bindings uacc ~bindings_outermost_first ~body =
  (* The name occurrences component of [uacc] is expected to be in the state
     described in the comment at the top of [Simplify_let.rebuild_let]. *)
  ListLabels.fold_left (List.rev bindings_outermost_first) ~init:(body, uacc)
    ~f:(fun (expr, uacc) (bound, defining_expr) ->
      match (defining_expr : Simplified_named.t) with
      | Invalid _ ->
        let uacc =
          UA.with_name_occurrences uacc ~name_occurrences:Name_occurrences.empty
          |> UA.cost_metrics_add ~added:(Cost_metrics.invalid ())
        in
        Expr.create_invalid (), uacc
      | Reachable {
          named = defining_expr;
          free_names = free_names_of_defining_expr;
          cost_metrics = cost_metrics_of_defining_expr;
        } ->
        let defining_expr = Simplified_named.to_named defining_expr in
        match (bound : Bindable_let_bound.t) with
        | Singleton _ | Set_of_closures _ ->
          let expr, uacc, _ =
            create_let uacc bound defining_expr
              ~free_names_of_defining_expr ~body:expr
              ~cost_metrics_of_defining_expr
          in
          expr, uacc
        | Symbols _ ->
          (* Since [Simplified_named] doesn't permit the [Static_consts] case,
             this must be a malformed binding. *)
          Misc.fatal_errorf "Mismatch between bound name(s) and defining \
              expression:@ %a@ =@ %a"
            Bindable_let_bound.print bound
            Named.print defining_expr)

let create_raw_let_symbol uacc bound_symbols scoping_rule static_consts ~body =
  (* Upon entry to this function, [UA.name_occurrences uacc] must precisely
     indicate the free names of [body]. *)
  let bindable = Bindable_let_bound.symbols bound_symbols scoping_rule in
  let free_names_of_static_consts =
    Static_const_with_free_names.Group.free_names static_consts
  in
  let defining_expr, cost_metrics_of_defining_expr =
    let static_consts = Static_const_with_free_names.Group.consts static_consts in
    Named.create_static_consts static_consts, Cost_metrics.static_consts static_consts
  in
  let free_names_of_body = UA.name_occurrences uacc in
  let free_names_of_let =
    (* Care: these bindings can be recursive (e.g. via a set of closures). *)
    let name_occurrences =
      Name_occurrences.union free_names_of_static_consts free_names_of_body
    in
    Code_id_or_symbol.Set.fold (fun code_id_or_sym free_names ->
        Name_occurrences.remove_code_id_or_symbol free_names code_id_or_sym)
      (Bound_symbols.everything_being_defined bound_symbols)
      name_occurrences
  in
  let uacc =
    UA.with_name_occurrences uacc ~name_occurrences:free_names_of_let
    |> UA.cost_metrics_add
         ~added:(Cost_metrics.increase_due_to_let_expr
            ~is_phantom:false
            ~cost_metrics_of_defining_expr)
  in
  let let_expr =
    Let.create bindable defining_expr ~body
      ~free_names_of_body:(Known free_names_of_body)
  in
  Expr.create_let let_expr, uacc

let create_let_symbol0 uacc code_age_relation (bound_symbols : Bound_symbols.t)
      (static_consts : Static_const_with_free_names.Group.t) ~body =
  (* Upon entry to this function, [UA.name_occurrences uacc] must precisely
     indicate the free names of [body]. *)
  let free_names_after = UA.name_occurrences uacc in
  let bound_names_unused =
    Bound_symbols.for_all_everything_being_defined bound_symbols
      ~f:(fun (code_id_or_symbol : Code_id_or_symbol.t) ->
        match code_id_or_symbol with
        | Code_id code_id ->
          (not (Name_occurrences.mem_code_id
            free_names_after code_id))
          &&
          (not (Name_occurrences.mem_newer_version_of_code_id
            free_names_after code_id))
        | Symbol sym ->
          not (Name_occurrences.mem_symbol free_names_after sym))
  in
  if bound_names_unused then body, uacc
  else
    let will_bind_code = Bound_symbols.binds_code bound_symbols in
    (* Turn pieces of code that are only referenced in [newer_version_of]
       fields into [Deleted]. *)
    let code_ids_to_make_deleted =
      if not will_bind_code then Code_id.Set.empty
      else
        (* CR-someday mshinwell: This could be made more precise, but would
           probably require a proper analysis. *)
        let code_ids_static_consts =
          ListLabels.fold_left
            (Static_const_with_free_names.Group.to_list static_consts)
            ~init:Code_id.Set.empty
            ~f:(fun code_ids static_const ->
              Static_const_with_free_names.free_names static_const
              |> Name_occurrences.code_ids
              |> Code_id.Set.union code_ids)
        in
        let all_code_ids_bound_names =
          Bound_symbols.code_being_defined bound_symbols
        in
        Code_id.Set.fold (fun bound_code_id result ->
            let in_newer_version_of_code_ids_after_but_not_code_ids_after =
              Name_occurrences.mem_newer_version_of_code_id
                free_names_after bound_code_id
              && not (Name_occurrences.mem_code_id free_names_after
                bound_code_id)
            in
            let can_make_deleted =
              in_newer_version_of_code_ids_after_but_not_code_ids_after
                && (not (Code_id.Set.mem bound_code_id code_ids_static_consts))
                (* We cannot delete code unless it is certain that a
                   non-trivial join operation between later versions of it
                   cannot happen. *)
                (* CR mshinwell: Think again about whether we need to have these
                   two separate calls. *)
                && Code_age_relation.newer_versions_form_linear_chain
                  code_age_relation bound_code_id
                  ~all_code_ids_still_existing:all_code_ids_bound_names
                && Code_age_relation.newer_versions_form_linear_chain'
                  code_age_relation bound_code_id
                  ~all_free_names_still_existing:free_names_after
            in
            if can_make_deleted then Code_id.Set.add bound_code_id result
            else result)
          all_code_ids_bound_names
          Code_id.Set.empty
    in
    let static_consts =
      if not will_bind_code then static_consts
      else
        Static_const_with_free_names.Group.map static_consts
          ~f:(fun static_const ->
            match
              Static_const_with_free_names.const static_const
              |> Static_const.to_code
            with
            | Some code
              when Code_id.Set.mem (Code.code_id code)
                code_ids_to_make_deleted ->
              let static_const : Static_const.t =
                Code (Code.make_deleted code)
              in
              Static_const_with_free_names.create static_const
                ~free_names:Unknown
            | Some _ | None -> static_const)
    in
    let expr, uacc =
      create_raw_let_symbol uacc bound_symbols Syntactic static_consts ~body
    in
    let uacc =
      if not will_bind_code then uacc
      else
        Static_const_with_free_names.Group.pieces_of_code static_consts
        |> UA.remember_code_for_cmx uacc
    in
    expr, uacc

let remove_unused_closure_vars uacc static_const =
  match Static_const_with_free_names.const static_const with
  | Set_of_closures set_of_closures ->
    let name_occurrences = UA.used_closure_vars uacc in
    let closure_vars = Set_of_closures.closure_elements set_of_closures in
    let closure_elements =
      Var_within_closure.Map.filter (fun closure_var _ ->
          Name_occurrences.mem_closure_var name_occurrences closure_var)
        closure_vars
    in
    let set_of_closures =
      Set_of_closures.create (Set_of_closures.function_decls set_of_closures)
        ~closure_elements
    in
    Static_const_with_free_names.create (Set_of_closures set_of_closures)
      ~free_names:(Known (Set_of_closures.free_names set_of_closures))
  | Code _
  | Block _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | Immutable_float_block _
  | Immutable_float_array _
  | Mutable_string _
  | Immutable_string _ -> static_const

let create_let_symbols uacc (scoping_rule : Symbol_scoping_rule.t)
      code_age_relation lifted_constant ~body =
  let bound_symbols = LC.bound_symbols lifted_constant in
  let symbol_projections = LC.symbol_projections lifted_constant in
  let static_consts =
    Static_const_with_free_names.Group.map (LC.defining_exprs lifted_constant)
      ~f:(remove_unused_closure_vars uacc)
  in
  let expr, uacc =
    match scoping_rule with
    | Syntactic ->
      create_let_symbol0 uacc code_age_relation bound_symbols static_consts
        ~body
    | Dominator ->
      let expr, uacc =
        create_raw_let_symbol uacc bound_symbols scoping_rule static_consts
          ~body
      in
      let uacc =
        (LC.defining_exprs lifted_constant)
        |> Static_const_with_free_names.Group.pieces_of_code
        |> UA.remember_code_for_cmx uacc
      in
      expr, uacc
  in
  Variable.Map.fold (fun var proj (expr, uacc) ->
      let rec apply_projection proj =
        match LC.apply_projection lifted_constant proj with
        | Some simple ->
          (* If the projection is from one of the symbols bound by the
             "let symbol" that we've just created, we'll always end up here,
             avoiding any problem about where to do the projection versus
             the initialisation of a possibly-recursive group of symbols.
             We may end up with a "variable = variable" [Let] here, but
             [Un_cps] (or a subsequent pass of [Simplify]) will remove it.
             This is the same situation as when continuations are inlined;
             we can't use a name permutation to resolve the problem as both
             [var] and [var'] may occur in [expr], and permuting could cause
             an unbound name.
             It is possible for one projection to yield a variable that is
             in turn defined by another symbol projection, so we need to
             expand transitively. *)
          Simple.pattern_match' simple
            ~const:(fun _ ->
              Named.create_simple simple, Cost_metrics.simple simple)
            ~symbol:(fun _ ->
              Named.create_simple simple, Cost_metrics.simple simple)
            ~var:(fun var ->
              match Variable.Map.find var symbol_projections with
              | exception Not_found ->
                Named.create_simple simple, Cost_metrics.simple simple
              | proj -> apply_projection proj)
        | None ->
          let prim : P.t =
            let symbol = Simple.symbol (Symbol_projection.symbol proj) in
            match Symbol_projection.projection proj with
            | Block_load { index; } ->
              let index = Simple.const_int index in
              let block_access_kind : P.Block_access_kind.t =
                Values {
                  tag = Tag.Scannable.zero;
                  size = Unknown;
                  field_kind = Any_value;
                }
              in
              Binary (Block_load (block_access_kind, Immutable), symbol,
                index)
            | Project_var { project_from; var; } ->
              Unary (Project_var { project_from; var; }, symbol)
          in
          Named.create_prim prim Debuginfo.none, Cost_metrics.prim prim
      in
      (* It's possible that this might create duplicates of the same
         projection operation, but it's unlikely there will be a
         significant number, and since we're at toplevel we tolerate
         them. *)
      let defining_expr, cost_metrics_of_defining_expr = apply_projection proj in
      let free_names_of_defining_expr = Named.free_names defining_expr in
      let expr, uacc, _ =
        create_let uacc (BLB.singleton (VB.create var Name_mode.normal))
          defining_expr ~free_names_of_defining_expr ~body:expr
          ~cost_metrics_of_defining_expr
      in
      expr, uacc)
    symbol_projections
    (expr, uacc)

let place_lifted_constants uacc (scoping_rule : Symbol_scoping_rule.t)
      ~lifted_constants_from_defining_expr ~lifted_constants_from_body
      ~put_bindings_around_body ~body ~critical_deps_of_bindings =
  let calculate_constants_to_place lifted_constants ~critical_deps
        ~to_float =
    (* If we are at a [Dominator]-scoped binding, then we float up
       as many constants as we can whose definitions are fully static
       (i.e. do not involve variables) to the nearest enclosing
       [Syntactic]ally-scoped [Let]-binding.  This is done by peeling
       off the definitions starting at the outermost one.  We keep
       track of the "critical dependencies", which are those symbols
       that are definitely going to have their definitions placed at
       the current [Let]-binding, and any reference to which in another
       binding (even if fully static) will cause that binding to be
       placed too. *)
    (* CR-soon mshinwell: This won't be needed once we can remove
       [Dominator]-scoped bindings; every "let symbol" can then have
       [Dominator] scoping.  This should both simplify the code and
       increase speed a fair bit. *)
    match scoping_rule with
    | Syntactic ->
      lifted_constants, to_float, critical_deps
    | Dominator ->
      LCS.fold_outermost_first lifted_constants
        ~init:(LCS.empty, to_float, critical_deps)
        ~f:(fun (to_place, to_float, critical_deps) lifted_const ->
          let must_place =
            (not (LC.is_fully_static lifted_const))
              || Name_occurrences.inter_domain_is_non_empty critical_deps
                    (LC.free_names_of_defining_exprs lifted_const)
          in
          if must_place then
            let critical_deps =
              LC.bound_symbols lifted_const
              |> Bound_symbols.free_names
              |> Name_occurrences.union critical_deps
            in
            let to_place = LCS.add_innermost to_place lifted_const in
            to_place, to_float, critical_deps
          else
            let to_float = LCS.add_innermost to_float lifted_const in
            to_place, to_float, critical_deps)
  in
  (* We handle constants arising from the defining expression, which
     may be used in [bindings], separately from those arising from the
     [body], which may reference the [bindings]. *)
  let to_place_around_defining_expr, to_float, critical_deps =
    calculate_constants_to_place lifted_constants_from_defining_expr
      ~critical_deps:Name_occurrences.empty ~to_float:LCS.empty
  in
  let critical_deps =
    (* Make sure we don't move constants past the binding(s) if there
       is a dependency. *)
    Name_occurrences.union critical_deps critical_deps_of_bindings
  in
  let to_place_around_body, to_float, _critical_deps =
    calculate_constants_to_place lifted_constants_from_body
      ~critical_deps ~to_float
  in
  (* Propagate constants that are to float upwards. *)
  let uacc = UA.with_lifted_constants uacc to_float in
  (* Place constants whose definitions must go at the current binding. *)
  let place_constants uacc ~around constants =
    LCS.fold_innermost_first constants ~init:(around, uacc)
      ~f:(fun (body, uacc) lifted_const ->
        create_let_symbols uacc scoping_rule
          (UA.code_age_relation uacc) lifted_const ~body)
  in
  let body, uacc =
    place_constants uacc ~around:body to_place_around_body
  in
  let body, uacc = put_bindings_around_body uacc ~body in
  place_constants uacc ~around:body to_place_around_defining_expr

let create_switch uacc ~scrutinee ~arms =
  if Target_imm.Map.cardinal arms < 1 then
    Expr.create_invalid (),
    UA.cost_metrics_add ~added:(Cost_metrics.invalid ()) uacc
  else
    let change_to_apply_cont action =
      Expr.create_apply_cont action,
      UA.cost_metrics_add ~added:(Cost_metrics.apply_cont action) uacc
    in
    match Target_imm.Map.get_singleton arms with
    | Some (_discriminant, action) -> change_to_apply_cont action
    | None ->
      (* CR mshinwell: We should do a partial invariant check here (one
         which doesn't require [Invariant_env.t]. *)
      let actions =
        Apply_cont_expr.Set.of_list (Target_imm.Map.data arms)
      in
      match Apply_cont_expr.Set.get_singleton actions with
      | Some action -> change_to_apply_cont action
      | None ->
        let switch = Switch.create ~scrutinee ~arms in
        Expr.create_switch switch,
        UA.cost_metrics_add ~added:(Cost_metrics.switch switch) uacc

