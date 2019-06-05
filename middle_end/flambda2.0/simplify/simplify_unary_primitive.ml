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

let simplify_projection env r ~original_term ~deconstructing ~shape ~result_var
      ~result_kind : Reachable.t * TEE.t * R.t =
  let env = E.typing_env env in
Format.eprintf "simplify_projection: original_term %a@ shape:@ %a@ deconstructing:@ %a\n%!"
  Named.print original_term
  T.print shape
  T.print deconstructing;
  match T.meet_shape env deconstructing ~shape ~result_var ~result_kind with
  | Bottom -> Reachable.invalid (), TEE.empty, r
  | Ok env_extension ->
Format.eprintf "Returned env extension:@ %a\n%!" TEE.print env_extension;
    Reachable.reachable original_term, env_extension, r

let simplify_project_closure env r ~original_term ~set_of_closures_ty closure_id
      ~result_var =
  simplify_projection env r ~original_term ~deconstructing:set_of_closures_ty
    ~shape:(T.set_of_closures_containing_at_least closure_id
      ~closure_var:result_var)
    ~result_var ~result_kind:K.value

let simplify_project_var env r ~original_term ~closure_ty closure_element
      ~result_var =
  simplify_projection env r ~original_term ~deconstructing:closure_ty
    ~shape:(T.closure_containing_at_least closure_element)
    ~result_var ~result_kind:K.value

let simplify_unary_primitive env r (prim : Flambda_primitive.unary_primitive)
      arg dbg ~result_var : Reachable.t * TEE.t * R.t =
begin match (arg : Simple.t) with
| Name (Var arg) ->
Format.eprintf "simplify_unary_primitive: type of arg:@ %a\n%!"
  T.print (E.find_variable env arg)
| _ -> ()
end;
  let arg, arg_ty = Simplify_simple.simplify_simple env arg in
  let original_term = Named.create_prim (Unary (prim, arg)) dbg in
  match prim with
  | Project_closure closure_id ->
    simplify_project_closure env r ~original_term ~set_of_closures_ty:arg_ty
      closure_id ~result_var
  | Project_var closure_element ->
    simplify_project_var env r ~original_term ~closure_ty:arg_ty
      closure_element ~result_var
  | _ ->
    (* CR mshinwell: temporary code *)
    let arg = Simplify_simple.simplify_simple_and_drop_type env arg in
    let named = Named.create_prim (Unary (prim, arg)) dbg in
    let ty = T.any_value () in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Reachable.reachable named, env_extension, r
