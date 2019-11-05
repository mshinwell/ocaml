(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

(* CR mshinwell: Move to generic iterators/mappers in [Flambda]? *)

let rec collect_vars_expr used_closure_vars expr =
  match Expr.descr expr with
  | Let let_expr ->
    collect_vars_named used_closure_vars (Let.defining_expr let_expr);
    Let.pattern_match let_expr ~f:(fun ~bound_vars:_ ~body ->
      collect_vars_expr used_closure_vars body)
  | Let_cont let_cont ->
    begin match let_cont with
    | Non_recursive { handler; _ } ->
      Non_recursive_let_cont_handler.pattern_match handler
        ~f:(fun _cont ~body -> collect_vars_expr used_closure_vars body);
      let cont_handler = Non_recursive_let_cont_handler.handler handler in
      Continuation_params_and_handler.pattern_match
        (Continuation_handler.params_and_handler cont_handler)
        ~f:(fun _params ~handler -> collect_vars_expr used_closure_vars handler)
    | Recursive rec_handlers ->
      Recursive_let_cont_handlers.pattern_match rec_handlers
        ~f:(fun ~body handlers ->
          collect_vars_expr used_closure_vars body;
          Continuation.Map.iter (fun _cont cont_handler ->
              Continuation_params_and_handler.pattern_match
                (Continuation_handler.params_and_handler cont_handler)
                ~f:(fun _params ~handler ->
                  collect_vars_expr used_closure_vars handler))
            (Continuation_handlers.to_map handlers))
    end
  | Apply _
  | Apply_cont _
  | Switch _
  | Invalid _ -> ()

and collect_vars_named used_closure_vars (named : Named.t) =
  match named with
  | Simple _ -> ()
  | Prim (Unary (Project_var closure_var, _closure), _dbg) ->
    used_closure_vars :=
      Var_within_closure.Set.add closure_var !used_closure_vars
  | Prim _ -> ()
  | Set_of_closures set_of_closures ->
    collect_vars_set_of_closures used_closure_vars set_of_closures

and collect_vars_set_of_closures used_closure_vars set_of_closures =
  let function_decls =
    Function_declarations.funs (Set_of_closures.function_decls set_of_closures)
  in
  Closure_id.Map.iter (fun _closure_id function_decl ->
      Function_params_and_body.pattern_match
        (Function_declaration.params_and_body function_decl)
        ~f:(fun ~return_continuation:_ _exn_continuation _params ~body
                ~my_closure:_ ->
          collect_vars_expr used_closure_vars body))
    function_decls

let rec remove_vars_expr used_closure_vars expr =
  match Expr.descr expr with
  | Let let_expr ->
    let defining_expr =
      remove_vars_named used_closure_vars (Let.defining_expr let_expr)
    in
    Let.pattern_match let_expr ~f:(fun ~bound_vars ~body ->
      let body = remove_vars_expr used_closure_vars body in
      Expr.create_pattern_let bound_vars defining_expr body)
  | Let_cont let_cont ->
    begin match let_cont with
    | Non_recursive { handler; _ } ->
      Non_recursive_let_cont_handler.pattern_match handler
        ~f:(fun cont ~body ->
          let body = remove_vars_expr used_closure_vars body in
          let cont_handler = Non_recursive_let_cont_handler.handler handler in
          let params_and_handler =
            Continuation_params_and_handler.pattern_match
              (Continuation_handler.params_and_handler cont_handler)
              ~f:(fun params ~handler ->
                let handler = remove_vars_expr used_closure_vars handler in
                Continuation_params_and_handler.create params ~handler)
          in
          let cont_handler =
            Continuation_handler.with_params_and_handler cont_handler
              params_and_handler
          in
          Let_cont.create_non_recursive cont cont_handler ~body)
    | Recursive rec_handlers ->
      Recursive_let_cont_handlers.pattern_match rec_handlers
        ~f:(fun ~body handlers ->
          let body = remove_vars_expr used_closure_vars body in
          let handler_map =
            Continuation.Map.map (fun cont_handler ->
                let params_and_handler =
                  Continuation_params_and_handler.pattern_match
                    (Continuation_handler.params_and_handler cont_handler)
                    ~f:(fun params ~handler ->
                      let handler = remove_vars_expr used_closure_vars handler in
                      Continuation_params_and_handler.create params ~handler)
                in
                Continuation_handler.with_params_and_handler cont_handler
                  params_and_handler)
              (Continuation_handlers.to_map handlers)
          in
          Let_cont.create_recursive handler_map ~body)
    end
  | Apply _
  | Apply_cont _
  | Switch _
  | Invalid _ -> expr

and remove_vars_named used_closure_vars (named : Named.t) =
  match named with
  | Simple _ | Prim _ -> named
  | Set_of_closures set_of_closures ->
    let set_of_closures =
      remove_vars_set_of_closures used_closure_vars set_of_closures
    in
    Named.create_set_of_closures set_of_closures

and remove_vars_set_of_closures used_closure_vars set_of_closures =
  let function_decls =
    Function_declarations.funs (Set_of_closures.function_decls set_of_closures)
  in
  let function_decls =
    Closure_id.Map.map (fun function_decl ->
        let params_and_body =
          Function_params_and_body.pattern_match
            (Function_declaration.params_and_body function_decl)
            ~f:(fun ~return_continuation exn_continuation params ~body
                    ~my_closure ->
              let body = remove_vars_expr used_closure_vars body in
              Function_params_and_body.create ~return_continuation
                exn_continuation params ~body ~my_closure)
        in
        Function_declaration.update_params_and_body function_decl
          params_and_body)
      function_decls
  in
  let function_decls = Function_declarations.create function_decls in
  let closure_elements =
    Var_within_closure.Map.filter (fun closure_var _ ->
        Var_within_closure.Set.mem closure_var used_closure_vars)
      (Set_of_closures.closure_elements set_of_closures)
  in
  Set_of_closures.create function_decls ~closure_elements

let run program =
  let used_closure_vars = ref Var_within_closure.Set.empty in
  Profile.record_call ~accumulate:true "finding" (fun () ->
    Flambda_static.Program.iter_body program ~f:(fun body ->
      Flambda_static.Program_body.iter_definitions body ~f:(fun defn ->
        Flambda_static.Program_body.Definition.iter_computation defn
          ~f:(fun computation ->
            Flambda_static.Program_body.Computation.iter_expr computation
              ~f:(fun expr -> collect_vars_expr used_closure_vars expr));
        Flambda_static.Program_body.Definition.iter_static_parts defn
          { f = (fun (type k) (static_part : k  Flambda_static.Static_part.t) ->
            match static_part with
            | Set_of_closures set_of_closures ->
              collect_vars_set_of_closures used_closure_vars set_of_closures
            | _ -> ());
          })));
  let used_closure_vars = !used_closure_vars in
  Profile.record_call ~accumulate:true "removal" (fun () ->
    Flambda_static.Program.map_body program ~f:(fun body ->
      Flambda_static.Program_body.map_definitions body ~f:(fun defn ->
        let defn =
          Flambda_static.Program_body.Definition.map_computation defn
            ~f:(fun computation ->
              Flambda_static.Program_body.Computation.map_expr computation
                ~f:(fun expr -> remove_vars_expr used_closure_vars expr))
        in
        Flambda_static.Program_body.Definition.map_static_parts defn
          { f = (fun (type k) (static_part : k Flambda_static.Static_part.t)
                  : k Flambda_static.Static_part.t ->
              match static_part with
              | Set_of_closures set_of_closures ->
                let set_of_closures =
                  remove_vars_set_of_closures used_closure_vars set_of_closures
                in
                Set_of_closures set_of_closures
              | static_part -> static_part);
          })))
