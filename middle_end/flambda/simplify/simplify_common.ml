(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

type 'a after_rebuild =
     Expr.t
  -> Upwards_acc.t
  -> 'a

type 'a rebuild =
     Upwards_acc.t
  -> after_rebuild:'a after_rebuild
  -> 'a

type ('a, 'b) down_to_up =
     Downwards_acc.t
  -> rebuild:'a rebuild
  -> 'b

type 'a expr_simplifier =
     Downwards_acc.t
  -> 'a
  -> down_to_up:(Expr.t * Upwards_acc.t,
       Expr.t * Upwards_acc.t) down_to_up
  -> Expr.t * Upwards_acc.t

type simplify_toplevel =
     Downwards_acc.t
  -> Expr.t
  -> return_continuation:Continuation.t
  -> return_arity:Flambda_arity.With_subkinds.t
  -> Exn_continuation.t
  -> return_cont_scope:Scope.t
  -> exn_cont_scope:Scope.t
  -> Expr.t * Upwards_acc.t

let rebuild_invalid uacc ~after_rebuild =
  after_rebuild (Expr.create_invalid ()) uacc

let simplify_projection dacc ~original_term ~deconstructing ~shape ~result_var
      ~result_kind =
  let env = DA.typing_env dacc in
  match T.meet_shape env deconstructing ~shape ~result_var ~result_kind with
  | Bottom -> Simplified_named.invalid (), TEE.empty (), dacc
  | Ok env_extension ->
    Simplified_named.reachable original_term, env_extension, dacc

type add_wrapper_for_fixed_arity_continuation0_result =
  | This_continuation of Continuation.t
  | Apply_cont of Apply_cont.t
  | New_wrapper of Continuation.t * Continuation_handler.t * Cost_metrics.t

type cont_or_apply_cont =
  | Continuation of Continuation.t
  | Apply_cont of Apply_cont.t

let add_wrapper_for_fixed_arity_continuation0 uacc cont_or_apply_cont
      ~use_id arity : add_wrapper_for_fixed_arity_continuation0_result =
  let uenv = UA.uenv uacc in
  let cont =
    match cont_or_apply_cont with
    | Continuation cont -> cont
    | Apply_cont apply_cont -> Apply_cont.continuation apply_cont
  in
  let original_cont = cont in
  let cont = UE.resolve_continuation_aliases uenv cont in
  match UE.find_apply_cont_rewrite uenv original_cont with
  | None -> This_continuation cont
  | Some rewrite when Apply_cont_rewrite.does_nothing rewrite ->
    (* CR mshinwell: think more about this check w.r.t. subkinds *)
    let arity = Flambda_arity.With_subkinds.to_arity arity in
    let arity_in_rewrite =
      Apply_cont_rewrite.original_params_arity rewrite
      |> Flambda_arity.With_subkinds.to_arity
    in
    if not (Flambda_arity.equal arity arity_in_rewrite) then begin
      Misc.fatal_errorf "Arity %a provided to fixed-arity-wrapper \
          addition function does not match arity %a in rewrite:@ %a"
        Flambda_arity.print arity
        Flambda_arity.print arity_in_rewrite
        Apply_cont_rewrite.print rewrite
    end;
    This_continuation cont
  | Some rewrite ->
    (* CR-someday mshinwell: This area should be improved and hence
       simplified.  Allowing [Apply] to take extra arguments is probably the
       way forward.  Although unboxing of variants requires untagging
       expressions to be inserted, so wrappers cannot always be avoided. *)
    let params = List.map (fun _kind -> Variable.create "param") arity in
    let kinded_params = List.map2 KP.create params arity in
    let new_wrapper expr ~free_names ~cost_metrics =
      let new_cont = Continuation.create () in
      let new_handler =
        Continuation_handler.create kinded_params ~handler:expr
          ~free_names_of_handler:free_names
          ~is_exn_handler:false
      in
      New_wrapper (new_cont, new_handler, cost_metrics)
    in
    match cont_or_apply_cont with
    | Continuation cont ->
      (* In this case, any generated [Apply_cont] will sit inside a wrapper
         that binds [kinded_params]. *)
      let args = List.map KP.simple kinded_params in
      let apply_cont = Apply_cont.create cont ~args ~dbg:Debuginfo.none in
      begin match Apply_cont_rewrite.rewrite_use rewrite use_id apply_cont with
      | Apply_cont apply_cont ->
        new_wrapper (Expr.create_apply_cont apply_cont)
          ~free_names:(Known (Apply_cont.free_names apply_cont))
          ~cost_metrics:(Cost_metrics.apply_cont apply_cont)
      | Expr build_expr ->
        let expr, cost_metrics, free_names =
          build_expr ~apply_cont_to_expr:(fun apply_cont ->
            Expr.create_apply_cont apply_cont,
            Cost_metrics.apply_cont apply_cont,
            Apply_cont.free_names apply_cont)
        in
        new_wrapper expr ~free_names:(Known free_names) ~cost_metrics
      end
    | Apply_cont apply_cont ->
      let apply_cont = Apply_cont.update_continuation apply_cont cont in
      match Apply_cont_rewrite.rewrite_use rewrite use_id apply_cont with
      | Apply_cont apply_cont -> Apply_cont apply_cont
      | Expr build_expr ->
        let expr, cost_metrics, free_names =
          build_expr ~apply_cont_to_expr:(fun apply_cont ->
            Expr.create_apply_cont apply_cont,
            Cost_metrics.apply_cont apply_cont,
            Apply_cont.free_names apply_cont)
        in
        new_wrapper expr ~free_names:(Known free_names) ~cost_metrics

type add_wrapper_for_switch_arm_result =
  | Apply_cont of Apply_cont.t
  | New_wrapper of Continuation.t * Continuation_handler.t * Cost_metrics.t

let add_wrapper_for_switch_arm uacc apply_cont ~use_id arity
      : add_wrapper_for_switch_arm_result =
  match
    add_wrapper_for_fixed_arity_continuation0 uacc (Apply_cont apply_cont)
      ~use_id arity
  with
  | This_continuation cont ->
    Apply_cont (Apply_cont.update_continuation apply_cont cont)
  | Apply_cont apply_cont -> Apply_cont apply_cont
  | New_wrapper (cont, wrapper, cost_metrics) -> New_wrapper (cont, wrapper, cost_metrics)

let add_wrapper_for_fixed_arity_continuation uacc cont ~use_id arity ~around =
  match
    add_wrapper_for_fixed_arity_continuation0 uacc (Continuation cont)
      ~use_id arity
  with
  | This_continuation cont -> around uacc cont
  | Apply_cont _ -> assert false
  | New_wrapper (new_cont, new_handler, cost_metrics_of_handler) ->
    let body, uacc = around uacc new_cont in
    let added =
      Cost_metrics.increase_due_to_let_cont_non_recursive ~cost_metrics_of_handler
    in
    Let_cont.create_non_recursive new_cont new_handler ~body
      ~free_names_of_body:(Known (Expr.free_names body)),
    UA.cost_metrics_add ~added uacc

let add_wrapper_for_fixed_arity_apply uacc ~use_id arity apply =
  match Apply.continuation apply with
  | Never_returns ->
     Expr.create_apply apply,
     UA.cost_metrics_add ~added:(Cost_metrics.apply apply) uacc
  | Return cont ->
    add_wrapper_for_fixed_arity_continuation uacc cont
      ~use_id arity
      ~around:(fun uacc return_cont ->
        let exn_cont =
          UE.resolve_exn_continuation_aliases (UA.uenv uacc)
            (Apply.exn_continuation apply)
        in
        let apply =
          Apply.with_continuations apply (Return return_cont) exn_cont
        in
        Expr.create_apply apply,
        UA.cost_metrics_add ~added:(Cost_metrics.apply apply) uacc)

let update_exn_continuation_extra_args uacc ~exn_cont_use_id apply =
  let exn_cont_rewrite =
    UE.find_apply_cont_rewrite (UA.uenv uacc)
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
  in
  match exn_cont_rewrite with
  | None -> apply
  | Some rewrite ->
    Apply.with_exn_continuation apply
      (Apply_cont_rewrite.rewrite_exn_continuation rewrite exn_cont_use_id
        (Apply.exn_continuation apply))

(* generate the projection of the i-th field of a n-tuple *)
let project_tuple ~dbg ~size ~field tuple =
  let module BAK = P.Block_access_kind in
  let bak : BAK.t = Values {
    field_kind = Any_value;
    tag = Tag.Scannable.zero;
    size = Known (Targetint.OCaml.of_int size);
  } in
  let mutability : Mutability.t = Immutable in
  let index = Simple.const_int (Targetint.OCaml.of_int field) in
  let prim = P.Binary (Block_load (bak, mutability), tuple, index) in
  Named.create_prim prim dbg

let split_direct_over_application apply ~param_arity =
  let arity = List.length param_arity in
  let args = Apply.args apply in
  assert (arity < List.length args);
  let full_app_args, remaining_args = Misc.Stdlib.List.split_at arity args in
  let func_var = Variable.create "full_apply" in
  let perform_over_application =
    Apply.create ~callee:(Simple.var func_var)
      ~continuation:(Apply.continuation apply)
      (Apply.exn_continuation apply)
      ~args:remaining_args
      ~call_kind:(Call_kind.indirect_function_call_unknown_arity ())
      (Apply.dbg apply)
      ~inline:(Apply.inline apply)
      ~inlining_state:(Apply.inlining_state apply)
  in
  let after_full_application = Continuation.create () in
  let after_full_application_handler =
    let func_param = KP.create func_var K.With_subkind.any_value in
    let free_names_of_expr = Apply.free_names perform_over_application in
    Continuation_handler.create [func_param]
      ~handler:(Expr.create_apply perform_over_application)
      ~free_names_of_handler:(Known free_names_of_expr)
      ~is_exn_handler:false
  in
  let full_apply =
    Apply.with_continuation_callee_and_args apply
      (Return after_full_application)
      ~callee:(Apply.callee apply)
      ~args:full_app_args
  in
  let expr =
    Let_cont.create_non_recursive after_full_application
      after_full_application_handler
      ~body:(Expr.create_apply full_apply)
      ~free_names_of_body:(Known (Apply.free_names full_apply))
  in
  expr
