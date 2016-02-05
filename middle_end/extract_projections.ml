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

(* CR-soon pchambart: should we restrict only to cases
  when the field is aliased to a variable outside
  of the closure (i.e. when we can certainly remove
  the allocation of the block) ?
  Note that this may prevent cases with imbricated
  closures from benefiting from this transformations.
  mshinwell: What word was "imbricated" supposed to be?
  (The code this referred to has been deleted, but the same thing is
  probably still happening).
*)

let from_function_decl ~which_variables
      ~(function_decl : Flambda.function_declaration) =
  let which_variables = Variable.Map.keys which_variables in
  let projections = ref Projection.Set.empty in
  let used_which_variables = ref Variable.Set.empty in
  let check_free_variable var =
    if Variable.Set.mem var which_variables then begin
      used_which_variables := Variable.Set.add var !used_which_variables
    end
  in
  let for_expr (expr : Flambda.expr) =
    match expr with
    | Var var
    | Let_mutable (_, var, _) ->
      check_free_variable var
    (* CR-soon mshinwell: We don't handle [Apply] for the moment to
       avoid disabling unboxing optimizations whenever we see a recursive
       call.  We should improve this analysis.  Leo says this can be
       done by a similar thing to the unused argument analysis. *)
    | Apply _ -> ()
    | Send { meth; obj; args; _ } ->
      check_free_variable meth;
      check_free_variable obj;
      List.iter check_free_variable args
    | Assign { new_value; _ } ->
      check_free_variable new_value
    | If_then_else (var, _, _)
    | Switch (var, _)
    | String_switch (var, _, _) ->
      check_free_variable var
    | Static_raise (_, args) ->
      List.iter check_free_variable args
    | For { from_value; to_value; _ } ->
      check_free_variable from_value;
      check_free_variable to_value
    | Let _ | Let_rec _ | Static_catch _ | While _ | Try_with _
    | Proved_unreachable -> ()
  in
  let for_named (named : Flambda.named) =
    match named with
    | Project_var project_var
        when Variable.Set.mem project_var.closure which_variables ->
      projections :=
        Projection.Set.add (Project_var project_var) !projections
    | Project_closure project_closure
        when Variable.Set.mem project_closure.set_of_closures
          which_variables ->
      projections :=
        Projection.Set.add (Project_closure project_closure) !projections
    | Move_within_set_of_closures move
        when Variable.Set.mem move.closure which_variables ->
      projections :=
        Projection.Set.add (Move_within_set_of_closures move) !projections
    | Prim (Pfield field_index, [var], _dbg)
        when Variable.Set.mem var which_variables ->
      projections :=
        Projection.Set.add (Field (field_index, var)) !projections
    | Set_of_closures set_of_closures ->
      let iter_specialised ~which_variables =
        Variable.Map.iter (fun _ (spec_to : Flambda.specialised_to) ->
            check_free_variable spec_to.var)
          which_variables
      in
      iter_specialised ~which_variables:set_of_closures.free_vars;
      iter_specialised ~which_variables:set_of_closures.specialised_args
    | Prim (_, vars, _) ->
      List.iter check_free_variable vars
    | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
    | Read_symbol_field _ | Project_var _ | Project_closure _
    | Move_within_set_of_closures _
    | Expr _ -> ()
  in
  Flambda_iterators.iter for_expr for_named function_decl.body;
  let projections = !projections in
  let used_which_variables = !used_which_variables in
  (* Don't extract projections whose [projecting_from] variable is also
     used boxed.  We could in the future consider being more sophisticated
     about this based on the uses in the body, but given we are not doing
     that yet, it seems safest in performance terms not to (e.g.) unbox a
     specialised argument whose boxed version is used. *)
  Projection.Set.filter (fun projection ->
      let projecting_from = Projection.projecting_from projection in
      not (Variable.Set.mem projecting_from used_which_variables))
    projections
