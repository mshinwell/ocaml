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
module T = Flambda_type
module TEE = Flambda_type.Typing_env_extension

let meet_skeleton env r ~original_term ~deconstructing ~skeleton ~result
      ~result_kind : _ Or_bottom.t =
  let env = E.typing_env env in
  let ty = TE.find_exn env deconstructing in
  match T.meet_skeleton env ty ~skeleton ~result ~result_kind with
  | Bottom -> Bottom
  | Ok env_extension -> Ok (original_term, env_extension, r)

let simplify_project_closure env r ~original_term closure_id ~set_of_closures
      ~result_var =
  meet_skeleton env r ~original_term ~deconstructing:set_of_closures
    ~skeleton:(T.set_of_closures_containing_at_least closure)
    ~result ~result_kind:(K.value ())

let simplify_project_var env r ~original_term closure closure_element
      ~result_var : _ Or_bottom.t =
  meet_skeleton env r ~original_term ~deconstructing:closure_id
    ~skeleton:(T.closure_containing_at_least closure_element)
    ~result ~result_kind:(K.value ())

let simplify_unary_primitive env r (prim : Flambda_primitive.unary_primitive)
      arg dbg ~result_var : Reachable.t * TEE.t * R.t =
  match prim with
  | Project_closure closure_id ->
    simplify_project_closure env r prim closure_id ~set_of_closures:arg dbg
      ~result_var
  | Project_var (closure_id, closure_element) ->
    simplify_project_closure env r prim closure_id closure_element dbg
      ~result_var
  | _ ->
    let arg = S.simplify_simple_and_drop_type env arg in
    let named = Named.create_prim (Unary (prim, arg)) dbg in
    let ty = T.any_value () in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Reachable.reachable named, env_extension, r
