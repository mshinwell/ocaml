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
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module T = Flambda_type
module TEE = Flambda_type.Typing_env_extension

let simplify_projection env r ~original_term ~deconstructing ~skeleton ~result
      ~result_kind : Reachable.t * TEE.t * R.t =
  match T.meet_skeleton env ty ~skeleton ~result ~result_kind with
  | Bottom -> Reachable.invalid (), TEE.empty, r
  | Ok env_extension -> Reachable.reachable original_term, env_extension, r

let simplify_project_closure env r ~original_term ~set_of_closures_ty closure_id
      ~result_var =
  simplify_projection env r ~original_term ~deconstructing:set_of_closures_ty
    ~skeleton:(T.set_of_closures_containing_at_least closure)
    ~result ~result_kind:K.value

let simplify_project_var env r ~original_term ~closure_ty closure_element
      ~result_var =
  simplify_projection env r ~original_term ~deconstructing:closure_ty
    ~skeleton:(T.closure_containing_at_least closure_element)
    ~result ~result_kind:K.value

let simplify_unary_primitive env r (prim : Flambda_primitive.unary_primitive)
      arg dbg ~result_var : Reachable.t * TEE.t * R.t =
  let arg, arg_ty = S.simplify_simple_and_drop_type env arg in
  let original_term = Named.create_prim (Unary (prim, arg), dbg) in
  match prim with
  | Project_closure closure_id ->
    simplify_project_closure env r ~original_term ~set_of_closures_ty:arg_ty
      closure_id ~result_var
  | Project_var closure_element ->
    simplify_project_closure env r ~original_term ~closure_ty:arg_ty
      closure_element ~result_var
  | _ ->
    (* CR mshinwell: temporary code *)
    let arg = S.simplify_simple_and_drop_type env arg in
    let named = Named.create_prim (Unary (prim, arg)) dbg in
    let ty = T.any_value () in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Reachable.reachable named, env_extension, r
