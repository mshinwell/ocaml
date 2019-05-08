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

open! Flambda.Import

module E = Simplify_env_and_result.Env
module R = Simplify_env_and_result.Result

module Make (Simplify_expr : Simplify_expr_intf.S) = struct
  (* Values of two types hold the information propagated during simplification:
     - [E.t] "environments", top-down, almost always called "env";
     - [R.t] "results", bottom-up approximately following the evaluation order,
       almost always called "r".  These results come along with rewritten
       Flambda terms.
  *)
  let simplify_toplevel env expr ~continuation ~continuation_params
       ~exn_continuation ~descr ~scope_level_for_lifted_constants =
    if not (E.mem_continuation env continuation) then begin
      Misc.fatal_errorf "The continuation parameter (%a) must be in the \
          environment before calling [simplify_toplevel]"
        Continuation.print continuation
    end;
    if not (E.mem_exn_continuation env exn_continuation) then begin
      Misc.fatal_errorf "The exception continuation parameter (%a) must be in \
          the environment before calling [simplify_toplevel]"
        Continuation.print exn_continuation
    end;
    (* CR mshinwell: Clear [env] here? *)
    let env =
      E.set_scope_level_for_lifted_constants env
        scope_level_for_lifted_constants
    in
    let r = R.create () in
    let expr, r = Simplify_expr.simplify_expr env r expr in
    let lifted_constants = R.get_lifted_constants r in
    expr, r, lifted_constants
end
