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

let create_singleton_let uacc (bound_var : Var_in_binding_pos.t) defining_expr
      ~free_names_of_defining_expr ~body =
  let generate_phantom_lets =
    !Clflags.debug && !Clflags.Flambda.Expert.phantom_lets
  in
  (* CR mshinwell: [let_creation_result] should really be some kind of
     "benefit" type. *)
  let bound_var, keep_binding, let_creation_result =
    let greatest_name_mode =
      Name_occurrences.greatest_name_mode_var free_names_of_body
        (Var_in_binding_pos.var bound_var)
    in
    let declared_name_mode =
      Var_in_binding_pos.name_mode bound_var
    in
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
          Var_in_binding_pos.print bound_var
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
          Var_in_binding_pos.print bound_var
          Named.print defining_expr
      end;
      bound_var, true, Nothing_deleted
    end else begin
      let has_uses =
        Name_mode.Or_absent.is_present greatest_name_mode
      in
      let user_visible =
        Variable.user_visible (Var_in_binding_pos.var bound_var)
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
        let bound_var =
          Var_in_binding_pos.with_name_mode bound_var name_mode
        in
        if Name_mode.is_normal name_mode then
          bound_var, true, Nothing_deleted
        else
          bound_var, true, Have_deleted defining_expr
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
    let free_names_of_body_without_bound_var =
      Name_occurrences.remove_var free_names bound_var
    in
    let free_names_of_defining_expr =
      if not generate_phantom_lets then (* CR mshinwell: refine condition *)
        free_names_of_defining_expr
      else
        Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
          free_names_of_defining_expr
          (Var_in_binding_pos.name_mode bound_var)
    in
    let free_names_of_let =
      Name_occurrences.union free_names_of_body_without_bound_var
        free_names_of_defining_expr
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
      let closure_var = Var_in_binding_pos.var closure_var in
      not (Name_occurrences.mem_var free_names_of_body closure_var))
  in
  if all_bound_vars_unused then body, uacc, Have_deleted defining_expr
  else
    let free_names_of_body = UA.name_occurrences uacc in
    let free_names_of_body_without_bound_vars =
      ListLabels.fold_left closure_vars ~init:free_names_of_body
        ~f:(fun free_names closure_var ->
          let closure_var = Var_in_binding_pos.var closure_var in
          Name_occurrences.remove_var free_names closure_var)
    in
    let free_names_of_let =
      Name_occurrences.union free_names_of_body_without_bound_vars
        free_names_of_defining_expr
    in
    let uacc = UA.with_name_occurrences uacc free_names_of_let in
    let let_expr =
      Let_expr.create bound_vars ~defining_expr ~body
        ~free_names_of_body:(Known free_names_of_body)
    in
    Expr.create_let let_expr, uacc, Nothing_deleted

let make_new_let_bindings uacc ~bindings ~body =
  (* The name occurrences component of [uacc] is expected to be in the state
     described in the comment below at the top of [rebuild_let]. *)
  ListLabels.fold_left (List.rev bindings) ~init:(body, uacc)
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
              to bind [Symbols];@ use the functions in [Simplify_common] \
              instead:@ %a@ =@ %a"
            Bindable_let_bound.print bound
            Simplified_named.print defining_expr)

let rebuild_let bindable_let_bound ~bindings_outermost_first:bindings
      ~lifted_constants_from_defining_expr ~at_unit_toplevel ~body uacc
      ~after_rebuild =
  (* At this point, the free names in [uacc] are the free names of [body]. *)
  let no_constants_from_defining_expr =
    LCS.is_empty lifted_constants_from_defining_expr
  in
  (* The lifted constants present in [uacc] are the ones arising from
     the simplification of [body] which still have to be placed.  We
     augment these with any constants arising from the simplification of
     the defining expression.  Then we place (some of) them and/or return
     them in [uacc] for an outer [Let]-binding to deal with. *)
  let lifted_constants_from_body = UA.lifted_constants uacc in
  let no_constants_to_place =
    no_constants_from_defining_expr
      && LCS.is_empty lifted_constants_from_body
  in
  (* Return as quickly as possible if there is nothing to do.  In this
     case, all constants get floated up to an outer binding. *)
  if no_constants_to_place || not at_unit_toplevel then
    let uacc =
      (* Avoid re-allocating [uacc] unless necessary. *)
      if no_constants_from_defining_expr then uacc
      else
        LCS.union_ordered ~innermost:lifted_constants_from_body
          ~outermost:lifted_constants_from_defining_expr
        |> UA.with_lifted_constants uacc
    in
    let uacc, body = make_new_let_bindings uacc ~bindings ~body in
    after_rebuild body uacc
  else
    let scoping_rule =
      (* If this is a "normal" let rather than a "let symbol", then we
         use [Dominator] scoping for any symbol bindings we place, as the
         types of the symbols may have been used out of syntactic scope.
      *)
      Option.value ~default:Symbol_scoping_rule.Dominator
        (Bindable_let_bound.let_symbol_scoping_rule bindable_let_bound)
    in
    let critical_deps_of_bindings =
      ListLabels.fold_left bindings ~init:Name_occurrences.empty
        ~f:(fun critical_deps (bound, _) ->
          Name_occurrences.union (Bindable_let_bound.free_names bound)
            critical_deps)
    in
    let body, uacc =
      Simplify_common.place_lifted_constants uacc
        scoping_rule
        ~lifted_constants_from_defining_expr
        ~lifted_constants_from_body
        ~put_bindings_around_body:(make_new_let_bindings ~bindings)
        ~body
        ~critical_deps_of_bindings
    in
    after_rebuild body uacc

let simplify_let dacc let_expr ~down_to_up =
  let module L = Flambda.Let in
  L.pattern_match let_expr ~f:(fun bindable_let_bound ~body ->
    (* Remember then clear the lifted constants memory in [DA] so we can
       easily find out which constants are generated during simplification
       of the defining expression and the [body]. *)
    let dacc, prior_lifted_constants = DA.get_and_clear_lifted_constants dacc in
    (* Simplify the defining expression. *)
    let { Simplify_named. bindings_outermost_first; dacc; } =
      Simplify_named.simplify_named dacc bindable_let_bound
        (L.defining_expr let_expr)
    in
    (* First remember any lifted constants that were generated during the
       simplification of the defining expression and sort them, since they
       may be mutually recursive.  Then add back in to [dacc]
       the [prior_lifted_constants] remembered above.  This results in the
       definitions and types for all these constants being available at a
       subsequent [Let_cont].  At such a point, [dacc] will be queried to
       retrieve all of the constants, which are then manually transferred
       into the computed [dacc] at the join point for subsequent
       simplification of the continuation handler(s).
       Note that no lifted constants are ever placed during the simplification
       of the defining expression.  (Not even in the case of a
       [Set_of_closures] binding, since "let symbol" is disallowed under a
       lambda.)
    *)
    let lifted_constants_from_defining_expr =
      Sort_lifted_constants.sort (DA.get_lifted_constants dacc)
    in
    let dacc = DA.add_lifted_constants dacc prior_lifted_constants in
    let at_unit_toplevel = DE.at_unit_toplevel (DA.denv dacc) in
    (* Simplify the body of the let-expression and make the new [Let] bindings
       around the simplified body.  [Simplify_named] will already have
       prepared [dacc] with the necessary bindings for the simplification of
       the body. *)
    Simplify_expr.simplify_expr dacc body
      ~down_to_up:(fun dacc ~rebuild:rebuild_body ->
        down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
          rebuild_body uacc ~after_rebuild:(fun body uacc ->
            rebuild_let bindable_let_bound ~bindings_outermost_first
              ~lifted_constants_from_defining_expr ~at_unit_toplevel ~body uacc
              ~after_rebuild))))
