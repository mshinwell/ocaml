(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module E = Simplify_env_and_result.Env
module H = Unbox_one_variable.How_to_unbox
(*module CAV = Invariant_params.Continuations.Continuation_and_variable*)

let find_unboxings ~env ~continuation_uses ~handlers =
  Continuation.Map.filter_map handlers
    ~f:(fun cont (handler : Flambda.Continuation_handler.t) ->
      if handler.stub then
        None
      else
        match handler.params with
        | [] -> None
        | params ->
          match Continuation.Map.find cont continuation_uses with
          | exception Not_found ->
            None
          | args_tys ->
            let params = Flambda.Typed_parameter.List.vars params in
            let params_to_tys =
              Variable.Map.of_list (List.combine params args_tys)
            in
            let unboxings =
              Variable.Map.filter_map params_to_tys
                ~f:(fun unboxee unboxee_ty ->
                  Unbox_one_variable.how_to_unbox ~env
                    ~unboxee ~unboxee_ty ~is_unbox_returns:false)
            in
            if Variable.Map.is_empty unboxings then None
            else Some unboxings)

(* CR mshinwell: If we get everything using [Unbox_one_variable] then
   this function should be able to move to [Invariant_params] *)
let propagate_invariant_params_flow ~handlers:_ ~backend:_ ~unboxings_by_cont =
  unboxings_by_cont
  (* CR mshinwell: This needs fixing and re-enabling.  It needs altering to
     have the correct names for other continuations the information is being
     propagated to.  It's possibly easiest just to call [Unbox_one_variable]
     again with the original approximation but the other continuation's
     parameters to get the [how_to_unbox] record with the right names. *)
(*
  let invariant_params_flow =
    Invariant_params.Continuations.invariant_param_sources handlers ~backend
  in
Format.eprintf "Invariant params:\n@;%a\n"
(Variable.Map.print
  Invariant_params.Continuations.Continuation_and_variable.Set.print)
  invariant_params_flow;
  let unboxings_by_cont' =
    Continuation.Map.fold (fun cont unboxings_by_param unboxings_by_cont' ->
        Variable.Map.fold (fun param unboxing unboxings_by_cont' ->
            match Variable.Map.find param invariant_params_flow with
            | exception Not_found -> unboxings_by_cont'
            | flow ->
              CAV.Set.fold (fun (target_cont, target_param)
                      unboxings_by_cont' ->
                  if Continuation.equal cont target_cont then
                    unboxings_by_cont'
                  else
                    let target_unboxings =
                      match
                        Continuation.Map.find target_cont unboxings_by_cont'
                      with
                      | exception Not_found -> Variable.Map.empty
                      | target_unboxings -> target_unboxings
                    in
                    Continuation.Map.add target_cont
                      (Variable.Map.add target_param unboxing
                        target_unboxings)
                      unboxings_by_cont')
                flow
                unboxings_by_cont')
          unboxings_by_param
          unboxings_by_cont')
      unboxings_by_cont
      Continuation.Map.empty
  in
  Continuation.Map.union (fun _cont unboxings1 unboxings2 ->
      Some (Variable.Map.disjoint_union unboxings1 unboxings2))
    unboxings_by_cont unboxings_by_cont'
*)

let for_continuations ~env ~continuation_uses ~handlers ~backend
      : Flambda.Expr.with_wrapper Continuation.Map.t option =
(*
Format.eprintf "Unbox_continuation_params starting with continuations %a\n%!"
  Continuation.Set.print (Continuation.Map.keys handlers);
*)
  let unboxings_by_cont = find_unboxings ~env ~continuation_uses ~handlers in
  if Continuation.Map.is_empty unboxings_by_cont then begin
    None
  end else begin
    let unboxings_by_cont =
      propagate_invariant_params_flow ~handlers ~backend ~unboxings_by_cont
    in
    let with_wrappers =
      Continuation.Map.fold (fun cont handler new_handlers ->
          let do_not_unbox () =
            let with_wrapper : Flambda.Expr.with_wrapper =
              Unchanged { handler; }
            in
            Continuation.Map.add cont with_wrapper new_handlers
          in
          match Continuation.Map.find cont unboxings_by_cont with
          | exception Not_found -> do_not_unbox ()
          | unboxings ->
            let handler : Flambda.Continuation_handler.t =
              match Continuation.Map.find cont handlers with
              | exception Not_found -> assert false
              | handler -> handler
            in
            let new_cont = Continuation.create () in
            let how_to_unbox = H.merge_variable_map unboxings in
            let _wrapper_params_map, wrapper_params =
              Flambda_utils.create_wrapper_params ~params:handler.params
                ~freshening_already_assigned:
                  how_to_unbox.unboxee_to_wrapper_params_unboxee
            in
            let wrapper_body =
              let initial_body : Flambda.Expr.t =
                let args =
                  how_to_unbox.new_arguments_for_call_in_wrapper
                    @ (Flambda.Typed_parameter.List.vars wrapper_params)
                in
                let args = List.map (fun var -> Simple.var var) args in
                Apply_cont (new_cont, None, args)
              in
              how_to_unbox.add_bindings_in_wrapper initial_body
            in
            assert (not handler.is_exn_handler);
            let with_wrapper : Flambda.Expr.with_wrapper =
              let existing_params =
                List.map (fun param ->
                    let var = Flambda.Typed_parameter.var param in
                    match
                      Variable.Map.find var how_to_unbox.new_unboxee_types
                    with
                    | exception Not_found -> param
                    | ty -> Flambda.Typed_parameter.with_type param ty)
                  handler.params
              in
              let params = how_to_unbox.new_params @ existing_params in
  Format.eprintf "Unbox_continuation_params has unboxed:\n@;%a\n%!"
    Flambda.Let_cont_handlers.print (Flambda.Let_cont_handlers.Recursive
      (Continuation.Map.add cont handler Continuation.Map.empty));
  Format.eprintf "Unboxed version has \
      wrapper (params@ %a)\n@ %a = %a\n@ and \
      new handler (params@ %a):\n@ %a = %a\n%!"
    Flambda.Typed_parameter.List.print wrapper_params
    Continuation.print cont
    Flambda.Expr.print wrapper_body
    Flambda.Typed_parameter.List.print params
    Continuation.print new_cont
    Flambda.Expr.print handler.handler;
                With_wrapper {
                  new_cont;
                  new_handler = {
                    params;
                    stub = handler.stub;
                    is_exn_handler = false;
                    handler = handler.handler;
                  };
                  wrapper_handler = {
                    params = wrapper_params;
                    stub = true;
                    is_exn_handler = false;
                    handler = wrapper_body;
                  };
                }
            in
            Continuation.Map.add cont with_wrapper new_handlers)
          handlers
          Continuation.Map.empty
    in
    Some with_wrappers
  end

let for_non_recursive_continuation ~name ~handler ~env ~arg_tys
      : Flambda.Expr.with_wrapper =
  let backend = E.backend env in
  let handlers =
    Continuation.Map.add name handler Continuation.Map.empty
  in
  let continuation_uses =
    Continuation.Map.add name arg_tys Continuation.Map.empty
  in
  let result = for_continuations ~env ~continuation_uses ~handlers ~backend in
  match result with
  | None -> Unchanged { handler; }
  | Some wrappers ->
    match Continuation.Map.bindings wrappers with
    | [name', with_wrapper] ->
      assert (Continuation.equal name name');
      with_wrapper
    | _ -> assert false

let for_recursive_continuations ~handlers ~env ~arg_tys =
  let backend = E.backend env in
  let result =
    for_continuations ~env ~continuation_uses:arg_tys ~handlers ~backend
  in
  match result with
  | None ->
    Continuation.Map.map (fun handler : Flambda.Expr.with_wrapper ->
          Unchanged { handler; })
      handlers
  | Some with_wrappers -> with_wrappers
