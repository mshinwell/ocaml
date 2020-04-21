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

let simplify_named dacc ~(bound_vars : Bindable_let_bound.t)
      (named : Named.t) ~after_traversal =
  match named with
  | Simple simple ->
    let bound_var = Bindable_let_bound.must_be_singleton bound_vars in
    let min_name_mode = Var_in_binding_pos.name_mode bound_var in
    begin match S.simplify_simple dacc simple ~min_name_mode with
    | Bottom, ty ->
      let dacc = DA.add_variable dacc bound_var (T.bottom (T.kind ty)) in
      let defining_expr = Reachable.invalid () in
      after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
        let bindings_outermost_first = [bound_vars, defining_expr] in
        after_rebuild ~bindings_outermost_first uacc)
    | Ok new_simple, ty ->
      let dacc = DA.add_variable dacc bound_var ty in
      let defining_expr =
        if simple == new_simple then Reachable.reachable named
        else Reachable.reachable (Named.create_simple simple)
      in
      after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
        let bindings_outermost_first = [bound_vars, defining_expr] in
        after_rebuild ~bindings_outermost_first uacc)
    end
  | Prim (prim, dbg) ->
    let bound_var = Bindable_let_bound.must_be_singleton bound_vars in
    let term, env_extension, dacc =
      Simplify_primitive.simplify_primitive dacc ~original_named:named
        prim dbg ~result_var:bound_var
    in
    let dacc =
      let kind = P.result_kind' prim in
      let dacc = DA.add_variable dacc bound_var (T.unknown kind) in
      DA.extend_typing_environment dacc env_extension
    in
    (* CR mshinwell: Add check along the lines of: types are unknown
       whenever [not (P.With_fixed_value.eligible prim)] holds. *)
    let defining_expr, dacc, ty =
      (* CR mshinwell: We should be able to do the equivalent of
         [Reify_continuation_param_types] here so long as we are
         at toplevel. *)
      Reification.try_to_reify dacc term ~bound_to:bound_var
    in
    let defining_expr =
      if T.is_bottom (DA.typing_env dacc) ty then Reachable.invalid ()
      else defining_expr
    in
    after_traversal dacc ~rebuild:(fun uacc ~after_rebuild ->
      let bindings_outermost_first = [bound_vars, defining_expr] in
      after_rebuild ~bindings_outermost_first uacc)
  | Set_of_closures set_of_closures ->
    Simplify_set_of_closures.simplify_non_lifted_set_of_closures dacc
      ~bound_vars set_of_closures ~after_traversal
