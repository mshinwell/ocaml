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

module E = Simplify_env_and_result.Env
module R = Simplify_env_and_result.Result

module Make (Simplify_expr : Simplify_expr_intf.S) = struct
  (* Values of two types hold the information propagated during simplification:
     - [E.t] "environments", top-down, almost always called "env";
     - [R.t] "results", bottom-up approximately following the evaluation order,
       almost always called "r".  These results come along with rewritten
       Flambda terms.
  *)
  let simplify_toplevel env r_outer expr ~return_continuation
       exn_continuation =
    E.check_continuation_is_bound env return_continuation;
    E.check_exn_continuation_is_bound env exn_continuation;
    let r = R.create ~resolver:(E.resolver env) in
    let expr, r = Simplify_expr.simplify_expr env r expr in
    let r_outer = R.add_lifted_constants r_outer ~from:r in
    expr, r_outer
end
