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

(* The constructed values of this type aren't currently used, but will be
   needed when we import the Flambda 1 inliner. *)
type let_creation_result =
  | Have_deleted of Named.t
  | Nothing_deleted

let create_singleton_let uacc (bound_var : VB.t) defining_expr
      ~free_names_of_defining_expr ~body =
  let generate_phantom_lets =
    !Clflags.debug && !Clflags.Flambda.Expert.phantom_lets
  in
  (* CR mshinwell: [let_creation_result] should really be some kind of
     "benefit" type. *)
  let bound_var, keep_binding, let_creation_result =
    let greatest_name_mode =
      Name_occurrences.greatest_name_mode_var free_names_of_body
        (VB.var bound_var)
    in
    let declared_name_mode = VB.name_mode bound_var in
    begin match
      Name_mode.Or_absent.compare_partial_order
         greatest_name_mode
         (Name_mode.Or_absent.present declared_name_mode)
    with
    | None -> ()
    | Some c ->
      if c <= 0 then ()
      else
        Misc.fatal_errorf "[Let]-binding declares variable %a (mode %a) to \
            be bound to@ %a,@ but this variable has occurrences at a higher \
            mode@ (>= %a)@ in the body (free names %a):@ %a"
          VB.print bound_var
          Name_mode.print declared_name_mode
          Named.print defining_expr
          Name_mode.Or_absent.print greatest_name_mode
          Name_occurrences.print free_names_of_body
          print body
    end;
    if not (Named.at_most_generative_effects defining_expr) then begin
      if not (Name_mode.is_normal declared_name_mode)
      then begin
        Misc.fatal_errorf "Cannot [Let]-bind non-normal variable to \
            a primitive that has more than generative effects:@ %a@ =@ %a"
          VB.print bound_var
          Named.print defining_expr
      end;
      bound_var, true, Nothing_deleted
    end else begin
      let has_uses = Name_mode.Or_absent.is_present greatest_name_mode in
      let user_visible =
        Variable.user_visible (VB.var bound_var)
      in
      let will_delete_binding =
        (* CR mshinwell: This should detect whether there is any
           provenance info associated with the variable.  If there isn't, the
           [Let] can be deleted even if debugging information is being
           generated. *)
        not (has_uses || (generate_phantom_lets && user_visible))
      in
      if will_delete_binding then begin
        bound_var, false, Have_deleted defining_expr
      end else
        let name_mode =
          match greatest_name_mode with
          | Absent -> Name_mode.phantom
          | Present name_mode -> name_mode
        in
        assert (Name_mode.can_be_in_terms name_mode);
        let bound_var = VB.with_name_mode bound_var name_mode in
        if Name_mode.is_normal name_mode then bound_var, true, Nothing_deleted
        else bound_var, true, Have_deleted defining_expr
    end
  in
  (* CR mshinwell: When leaving behind phantom lets, maybe we should turn
     the defining expressions into simpler ones by using the type, if possible.
     For example an Unbox_naked_int64 or something could potentially turn
     into a variable.  This defining expression usually never exists as
     the types propagate the information forward.
     mshinwell: this might be done now in Simplify_named, check. *)
  if not keep_binding then body, uacc, let_creation_result
  else
    let free_names_of_body = UA.name_occurrences uacc in
    let free_names_of_defining_expr =
      if not generate_phantom_lets then (* CR mshinwell: refine condition *)
        free_names_of_defining_expr
      else
        Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
          free_names_of_defining_expr (VB.name_mode bound_var)
    in
    let free_names_of_let =
      Name_occurrences.remove_var free_names_of_body (VB.var bound_var)
      |> Name_occurrences.union free_names_of_defining_expr
    in
    let uacc = UA.with_name_occurrences uacc free_names_of_let in
    let let_expr =
      Let_expr.create (Bindable_let_bound.singleton bound_var)
        ~defining_expr ~body
        ~free_names_of_body:(Known free_names_of_body)
    in
    Expr.create_let let_expr, uacc, Nothing_deleted

let create_set_of_closures_let uacc bound_vars defining_expr
      ~free_names_of_defining_expr ~body ~closure_vars =
  (* CR-someday mshinwell: Think about how to phantomise these [Let]s. *)
  let all_bound_vars_unused =
    ListLabels.for_all closure_vars ~f:(fun closure_var ->
      not (Name_occurrences.mem_var free_names_of_body (VB.var closure_var)))
  in
  if all_bound_vars_unused then body, uacc, Have_deleted defining_expr
  else
    let free_names_of_body = UA.name_occurrences uacc in
    let free_names_of_let =
      ListLabels.fold_left closure_vars ~init:free_names_of_body
        ~f:(fun free_names closure_var ->
          Name_occurrences.remove_var free_names (VB.var closure_var))
      |> Name_occurrences.union free_names_of_defining_expr
    in
    let uacc = UA.with_name_occurrences uacc free_names_of_let in
    let let_expr =
      Let_expr.create bound_vars ~defining_expr ~body
        ~free_names_of_body:(Known free_names_of_body)
    in
    Expr.create_let let_expr, uacc, Nothing_deleted

let make_new_let_bindings uacc ~bindings_outermost_first ~body =
  (* The name occurrences component of [uacc] is expected to be in the state
     described in the comment below at the top of [rebuild_let]. *)
  ListLabels.fold_left (List.rev bindings_outermost_first) ~init:(body, uacc)
    ~f:(fun (expr, uacc) (bound, defining_expr) ->
      match (defining_expr : Simplified_named.t) with
      | Invalid _ ->
        let uacc = UA.with_name_occurrences uacc Name_occurrences.empty in
        uacc, Expr.create_invalid ()
      | Reachable {
          named = defining_expr;
          free_names = free_names_of_defining_expr;
        } ->
        match (bound : Bindable_let_bound.t) with
        | Singleton var ->
          let expr, uacc, _ =
            create_singleton_let uacc var defining_expr
              ~free_names_of_defining_expr ~body:expr
          in
          expr, uacc
        | Set_of_closures { closure_vars; _ } ->
          let expr, uacc, _ =
            create_set_of_closures_let uacc bound defining_expr
              ~free_names_of_defining_expr ~body ~closure_vars
          in
          expr, uacc
        | Symbols _ ->
          Misc.fatal_errorf "[make_new_let_bindings] should never be called \
              to bind [Symbols]:@ %a@ =@ %a"
            Bindable_let_bound.print bound
            Simplified_named.print defining_expr)

(* XXX Don't forget that this code needs to adjust uacc for occurrences *)

let create_let_symbol bindable defining_expr body =
  let free_names_of_body = free_names body in
  let free_names_of_bindable = Bindable_let_bound.free_names bindable in
  let let_expr =
    Let_expr.create bindable ~defining_expr ~body
      ~free_names_of_body:(Known free_names_of_body)
  in
  let free_names =
    let from_defining_expr = Named.free_names defining_expr in
    Name_occurrences.union from_defining_expr
      (Name_occurrences.diff free_names_of_body free_names_of_bindable)
  in
  { descr = Let let_expr;
    delayed_permutation = Name_permutation.empty;
    free_names = Some free_names;
  }
  (* XXX *)

let create_let_symbol uacc bound_symbols scoping_rule static_consts body : t =
  let expr, _ =
    create_pattern_let0 uacc
      (Bindable_let_bound.symbols bound_symbols scoping_rule)
      (Named.create_static_consts static_consts)
      body
  in
  expr, uacc

let create_let_symbol0 uacc code_age_relation (bound_symbols : Bound_symbols.t)
      (static_consts : Static_const.Group.With_free_names.t) body =
(*
  Format.eprintf "create_let_symbol %a\n%!" Bound_symbols.print bound_symbols;
*)
  let free_names_after =  XXX Expr.free_names body in
  let bound_names_unused =
    let being_defined =
      Bound_symbols.everything_being_defined bound_symbols
    in
    Code_id_or_symbol.Set.for_all
      (fun (code_id_or_symbol : Code_id_or_symbol.t) ->
        match code_id_or_symbol with
        | Code_id code_id ->
          not (Name_occurrences.mem_code_id free_names_after code_id)
        | Symbol sym ->
          not (Name_occurrences.mem_symbol free_names_after sym))
      being_defined
  in
  let all_code_ids_bound_names =
    let bound_names = Bound_symbols.free_names bound_symbols in
    Name_occurrences.code_ids_and_newer_version_of_code_ids bound_names
  in
  let newer_version_of_code_ids_after =
    Name_occurrences.newer_version_of_code_ids free_names_after
  in
  let code_ids_after =
    Name_occurrences.code_ids free_names_after
  in
  let code_ids_only_used_in_newer_version_of_after =
    Code_id.Set.diff newer_version_of_code_ids_after code_ids_after
  in
  let all_code_ids_free_names_after =
    Code_id.Set.union newer_version_of_code_ids_after code_ids_after
  in
  (* CR mshinwell: Add [Set.are_disjoint]? *)
  if bound_names_unused
    && Code_id.Set.is_empty (Code_id.Set.inter
      all_code_ids_bound_names all_code_ids_free_names_after)
  then body, uacc
  else
    (* Turn pieces of code that are only referenced in [newer_version_of]
       fields into [Deleted]. *)
    let code_ids_to_make_deleted =
      (* CR-someday mshinwell: This could be made more precise, but would
         probably require a proper analysis. *)
      let code_ids_static_consts =
        ListLabels.fold_left (Static_const.Group.to_list static_consts)
          ~init:Code_id.Set.empty
          ~f:(fun code_ids static_const ->
            Static_const.free_names static_const
            |> Name_occurrences.code_ids
            |> Code_id.Set.union code_ids)
      in
      let code_ids_only_used_in_newer_version_of =
        Code_id.Set.inter all_code_ids_bound_names
          (Code_id.Set.diff code_ids_only_used_in_newer_version_of_after
            code_ids_static_consts)
      in
      (* We cannot delete code unless it is certain that a non-trivial join
         operation between later versions of it cannot happen. *)
      Code_id.Set.filter (fun code_id ->
          (* CR mshinwell: Think again about whether we need to have these
             two separate calls. *)
          Code_age_relation.newer_versions_form_linear_chain
            code_age_relation code_id
            ~all_code_ids_still_existing:all_code_ids_bound_names
          &&
          Code_age_relation.newer_versions_form_linear_chain
            code_age_relation code_id
            ~all_code_ids_still_existing:all_code_ids_free_names_after)
        code_ids_only_used_in_newer_version_of
    in
    let static_consts =
      Static_const.Group.With_free_names.map_consts static_consts
        ~f:(fun static_const : Static_const.t ->
          match Static_const.to_code static_const with
          | Some code
            when Code_id.Set.mem (Code.code_id code) code_ids_to_make_deleted ->
            Code (Code.make_deleted code)
          | Some _ | None -> static_const)
    in
    let expr =
      create_let_symbol bound_symbols Syntactic static_consts body
    in
    let uacc =
      Static_const.Group.pieces_of_code_by_code_id static_consts
      |> UA.remember_code_for_cmx uacc
    in
    expr, uacc

let remove_unused_closure_vars uacc static_const =
  match Static_const.With_free_names.const static_const with
  | Set_of_closures set_of_closures ->
    let name_occurrences = UA.name_occurrences uacc in
    let free_names =
      Var_within_closure.Map.fold (fun closure_var _ free_names ->
          if Name_occurrences.mem_closure_var name_occurrences then
            free_names
          else
            Name_occurrences.remove_one_occurrence_of_closure_var free_names
              closure_var)
        (Set_of_closures.closure_elements set_of_closures)
        (Static_const.With_free_names.free_names static_const)
    in
    let closure_elements =
      Set_of_closures.closure_elements set_of_closures
      |> Var_within_closure.Map.filter (fun closure_var _ ->
        Name_occurrences.mem_closure_var name_occurrences closure_var)
    in
    let set_of_closures =
      Set_of_closures.create (Set_of_closures.function_decls set_of_closures)
        ~closure_elements
    in
    Static_const.With_free_names.create (Set_of_closures set_of_closures)
      free_names
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
      code_age_relation lifted_constant body =
  let bound_symbols = LC.bound_symbols lifted_constant in
  let symbol_projections = LC.symbol_projections lifted_constant in
  let static_consts =
    Static_const.Group.With_free_names.map (LC.defining_exprs lifted_constant)
      ~f:(remove_unused_closure_vars_list uacc)
  in
  let expr, uacc =
    match scoping_rule with
    | Syntactic ->
      create_let_symbol0 uacc code_age_relation bound_symbols static_consts body
    | Dominator ->
      let expr =
        Expr.create_let_symbol bound_symbols scoping_rule static_consts body
      in
      let uacc =
        defining_exprs
        |> Static_const.Group.With_free_names.pieces_of_code_by_code_id
        |> UA.remember_code_for_cmx uacc
      in
      expr, uacc
  in
  let expr =
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
              ~const:(fun _ -> Named.create_simple simple)
              ~symbol:(fun _ -> Named.create_simple simple)
              ~var:(fun var ->
                match Variable.Map.find var symbol_projections with
                | exception Not_found -> Named.create_simple simple
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
            Named.create_prim prim Debuginfo.none
        in
        (* It's possible that this might create duplicates of the same
           projection operation, but it's unlikely there will be a
           significant number, and since we're at toplevel we tolerate
           them. *)
        create_let uacc (Var_in_binding_pos.create var NM.normal)
          (apply_projection proj) expr)
      symbol_projections
      expr, uacc
  in
  expr, uacc

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
          (UA.code_age_relation uacc) lifted_const body)
  in
  let body, uacc =
    place_constants uacc ~around:body to_place_around_body
  in
  let body = put_bindings_around_body ~body in
  place_constants uacc ~around:body to_place_around_defining_expr
