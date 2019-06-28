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

let simplify_project_var dacc ~original_term ~closure_ty closure_element
      ~result_var =
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:closure_ty
    ~shape:(T.closure_containing_at_least closure_element
      ~closure_element_var:result_var)
    ~result_var ~result_kind:K.value

let simplify_unbox_number dacc ~original_term ~boxed_number_ty
      (boxable_number_kind : K.Boxable_number.t) ~result_var =
  let shape, result_kind =
    match boxable_number_kind with
    | Naked_float -> Misc.fatal_error "Not yet implemented"
    | Naked_int32 ->
      T.boxed_int32_alias_to ~naked_int32:result_var, K.naked_int32
    | Naked_int64 ->
      T.boxed_int64_alias_to ~naked_int64:result_var, K.naked_int64
    | Naked_nativeint -> Misc.fatal_error "Not yet implemented"
  in
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:boxed_number_ty
    ~shape ~result_var ~result_kind

let simplify_box_number dacc ~original_term ~naked_number_ty
      (boxable_number_kind : K.Boxable_number.t) ~result_var =
  let ty =
    match boxable_number_kind with
    | Naked_float -> Misc.fatal_error "Not yet implemented"
    | Naked_int32 -> T.box_int32 naked_number_ty
    | Naked_int64 -> T.box_int64 naked_number_ty
    | Naked_nativeint -> Misc.fatal_error "Not yet implemented"
  in
  Reachable.reachable original_term,
    TEE.one_equation (Name.var result_var) ty,
    dacc

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
  let min_occurrence_kind = Var_in_binding_pos.occurrence_kind result_var in
  let result_var = Var_in_binding_pos.var result_var in
  match Simplify_simple.simplify_simple dacc arg ~min_occurrence_kind with
  | Bottom kind ->
    let env_extension =
      TEE.one_equation (Name.var result_var) (T.bottom kind)
    in
    Reachable.invalid (), env_extension, dacc
  | Ok (arg, arg_ty) ->
    let original_term = Named.create_prim (Unary (prim, arg)) dbg in
    match prim with
    | Project_var closure_element ->
      simplify_project_var dacc ~original_term ~closure_ty:arg_ty
        closure_element ~result_var
    | Unbox_number boxable_number_kind ->
      simplify_unbox_number dacc ~original_term ~boxed_number_ty:arg_ty
        boxable_number_kind ~result_var
    | Box_number boxable_number_kind ->
      simplify_box_number dacc ~original_term ~naked_number_ty:arg_ty
        boxable_number_kind ~result_var
    | _ ->
      (* CR mshinwell: temporary code *)
      let named = Named.create_prim (Unary (prim, arg)) dbg in
      let kind = Flambda_primitive.result_kind_of_unary_primitive' prim in
      let ty = T.unknown kind in
      let env_extension = TEE.one_equation (Name.var result_var) ty in
      Reachable.reachable named, env_extension, dacc
