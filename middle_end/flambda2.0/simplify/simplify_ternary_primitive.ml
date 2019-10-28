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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

open! Simplify_import

let try_cse dacc prim arg1 arg2 arg3 ~min_occurrence_kind ~result_var
      : Simplify_common.cse =
  match
    S.simplify_simple dacc arg1
      ~min_occurrence_kind:Name_occurrence_kind.min_in_types
  with
  | Bottom, ty -> Invalid ty
  | Ok arg1, _arg1_ty ->
    match
      S.simplify_simple dacc arg2
        ~min_occurrence_kind:Name_occurrence_kind.min_in_types
    with
    | Bottom, ty -> Invalid ty
    | Ok arg2, _arg2_ty ->
      match
        S.simplify_simple dacc arg3
          ~min_occurrence_kind:Name_occurrence_kind.min_in_types
      with
      | Bottom, ty -> Invalid ty
      | Ok arg3, _arg3_ty ->
        let original_prim : P.t =
          Ternary (prim, arg1, arg2, arg3)
        in
        let result_kind =
          P.result_kind_of_ternary_primitive' prim
        in
        Simplify_common.try_cse dacc ~original_prim ~result_kind
          ~min_occurrence_kind ~result_var

let simplify_ternary_primitive dacc (prim : P.ternary_primitive)
      arg1 arg2 arg3 dbg ~result_var =
  let min_occurrence_kind = Var_in_binding_pos.occurrence_kind result_var in
  let result_var' = Var_in_binding_pos.var result_var in
  let invalid ty =
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Reachable.invalid (), env_extension, dacc
  in
  match
    try_cse dacc prim arg1 arg2 arg3 ~min_occurrence_kind
      ~result_var:result_var'
  with
  | Invalid ty -> invalid ty
  | Applied result -> result
  | Not_applied dacc ->
    match S.simplify_simple dacc arg1 ~min_occurrence_kind with
    | Bottom, ty -> invalid ty
    | Ok arg1, _arg1_ty ->
      match S.simplify_simple dacc arg2 ~min_occurrence_kind with
      | Bottom, ty -> invalid ty
      | Ok arg2, _arg2_ty ->
        match S.simplify_simple dacc arg3 ~min_occurrence_kind with
        | Bottom, ty -> invalid ty
        | Ok arg3, _arg3_ty ->
          match prim with
          | Block_set _
          | Bytes_or_bigstring_set _ ->
            let named =
              Named.create_prim (Ternary (prim, arg1, arg2, arg3)) dbg
            in
            let kind = P.result_kind_of_ternary_primitive' prim in
            let ty = T.unknown kind in
            let env_extension = TEE.one_equation (Name.var result_var') ty in
            Reachable.reachable named, env_extension, dacc
