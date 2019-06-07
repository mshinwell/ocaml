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

module DA = Downwards_acc
module R = Simplify_env_and_result.Result
module UA = Upwards_acc
module UE = Simplify_env_and_result.Upwards_env

let simplify_toplevel dacc expr ~return_continuation exn_continuation =
  DA.check_continuation_is_bound dacc return_continuation;
  DA.check_exn_continuation_is_bound dacc exn_continuation;
  let expr, cont_uses_env, uacc =
    Simplify_expr.simplify_expr dacc expr (fun cont_uses_env r ->
      cont_uses_env, UA.create UE.empty r)
  in
  let r_outer = R.add_lifted_constants (DA.r dacc) ~from:(UA.r uacc) in
  expr, cont_uses_env, r_outer
