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

module K = Flambda_kind
module T = Flambda_type
module TEE = Flambda_type.Typing_env_extension

let simplify_project_closure dacc ~original_term ~set_of_closures_ty closure_id
      ~result_var =
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:set_of_closures_ty
    ~shape:(T.set_of_closures_containing_at_least closure_id
      ~closure_var:result_var)
    ~result_var ~result_kind:K.value

let simplify_project_var dacc ~original_term ~closure_ty closure_element
      ~result_var =
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:closure_ty
    ~shape:(T.closure_containing_at_least closure_element
      ~closure_element_var:result_var)
    ~result_var ~result_kind:K.value

let simplify_unary_primitive dacc (prim : Flambda_primitive.unary_primitive)
      arg dbg ~result_var =
(*
begin match (arg : Simple.t) with
| Name (Var arg) ->
Format.eprintf "simplify_unary_primitive: type of arg %a:@ %a@ Env:@ %a%!"
  Variable.print arg
  T.print (E.find_variable env arg)
  E.print env
| _ -> ()
end;
*)
  let arg, arg_ty = Simplify_simple.simplify_simple dacc arg in
  let original_term = Named.create_prim (Unary (prim, arg)) dbg in
  match prim with
  | Project_closure closure_id ->
    simplify_project_closure dacc ~original_term ~set_of_closures_ty:arg_ty
      closure_id ~result_var
  | Project_var closure_element ->
    simplify_project_var dacc ~original_term ~closure_ty:arg_ty
      closure_element ~result_var
  | _ ->
    (* CR mshinwell: temporary code *)
    let arg = Simplify_simple.simplify_simple_and_drop_type dacc arg in
    let named = Named.create_prim (Unary (prim, arg)) dbg in
    let ty = T.any_value () in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Reachable.reachable named, env_extension, dacc
