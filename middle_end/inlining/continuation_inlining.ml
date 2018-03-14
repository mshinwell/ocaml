(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2018 OCamlPro SAS                                    *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module R = Simplify_env_and_result.Result

let for_toplevel_expression expr r =
  (* This pass only performs linear inlining, even for stubs.  Non-linear
     inlining for stubs is done by [Simplify]. *)
  let used_linearly =
    R.non_recursive_continuations_used_linearly_in_inlinable_position r
  in
  if Continuation.Set.is_empty used_linearly then begin
    expr, r
  end else begin
(*
    Format.eprintf "Continuations used linearly: %a\nTerm:@ \n%a%!"
      Continuation.Set.print used_linearly Flambda.Expr.print expr;
*)
    let r = ref r in
    (* CR mshinwell: Consider adding phys-equal checks and use of the tail
       recursive [Let] mapper. *)
    let rec substitute env (expr : Flambda.Expr.t) : Flambda.Expr.t =
      match expr with
      | Let ({ var; body; _ } as let_expr) ->
        let module W = Flambda.With_free_names in
        let defining_expr = W.of_defining_expr_of_let let_expr in
        let body = substitute env body in
        W.create_let_reusing_defining_expr var defining_expr body
      | Let_mutable ({ body; _ } as let_mutable) ->
        let body = substitute env body in
        Let_mutable { let_mutable with body; }
      | Let_cont { body; handlers = Non_recursive { name; handler; }; } ->
        let handler =
          { handler with
            handler = substitute env handler.handler;
          }
        in
        let used_linearly = Continuation.Set.mem name used_linearly in
        let body_env =
          if used_linearly then Continuation.Map.add name handler env
          else env
        in
        let body = substitute body_env body in
        if not (used_linearly || R.continuation_defined !r name) then begin
          Misc.fatal_errorf "Continuation %a: if it's not used linearly \
              then it should be defined in [r] (note that zero uses does \
              not count as ``linearly used''): %a"
            Continuation.print name
            Flambda.Expr.print expr
        end;
        (* Beware: we may have failed to inline---see comment below.
           In that case the [Let_cont] must stay. *)
        if R.continuation_defined !r name then begin
          (* Continuation specialisation, which runs after this pass, takes
             the code of continuations from their approximations.  As such if
             a continuation has been changed by inlining then its new version
             must be recorded. *)
          let approx =
            Continuation_approx.create ~name
              ~handlers:(Non_recursive handler)
              ~params:handler.params
          in
          r := R.update_defined_continuation_approx !r name approx;
          Let_cont { body; handlers = Non_recursive { name; handler; }; }
        end else begin
          body
        end
      | Let_cont { body; handlers = Recursive handlers; } ->
        let body = substitute env body in
        let handlers =
          Continuation.Map.map (fun (handler : Flambda.Continuation_handler.t) ->
              { handler with
                (* Do not inline continuations into recursive continuations.
                   This can cause high nesting depth of [catch rec] in the
                   backend and very bad compilation time performance, e.g.
                   during liveness analysis. *)
                handler = substitute Continuation.Map.empty handler.handler;
              })
            handlers
        in
        Continuation.Map.iter (fun name
                (handler : Flambda.Continuation_handler.t) ->
            let approx =
              Continuation_approx.create ~name
                ~handlers:(Recursive handlers)
                ~params:handler.params
            in
            r := R.update_defined_continuation_approx !r name approx)
          handlers;
        Let_cont { body; handlers = Recursive handlers; }
      | Apply_cont (cont, trap_action, args) ->
        begin match Continuation.Map.find cont env with
        | exception Not_found -> expr
        | (handler : Flambda.Continuation_handler.t) ->
          begin match trap_action with
          | None -> ()
          | Some _ ->
            Misc.fatal_errorf "Continuation %a should not have been deemed \
                as used ``linearly in inlinable position'' when it occurs \
                in an [Apply_cont] expression with a trap handler action"
              Continuation.print cont
          end;
          r := R.forget_continuation_definition !r cont;
          List.fold_left2 (fun expr param arg ->
              let var = Flambda.Typed_parameter.var param in
              let kind = Flambda.Typed_parameter.kind param in
              Flambda.Expr.create_let var kind (Simple arg) expr)
            handler.handler
            handler.params args
        end
      | Apply _ | Switch _ | Invalid _ -> expr
    in
    let expr = substitute Continuation.Map.empty expr in
(*
    Format.eprintf "After continuation inlining:@ \n%a" Flambda.Expr.print expr;
*)
    expr, !r
  end
