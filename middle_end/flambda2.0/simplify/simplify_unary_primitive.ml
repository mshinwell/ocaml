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

open! Simplify_import

let simplify_project_closure closure_id dacc ~original_term ~arg:_
      ~arg_ty:set_of_closures_ty ~result_var =
  let result = Simple.var (Var_in_binding_pos.var result_var) in
  let closures =
    Closure_id.Map.empty
    |> Closure_id.Map.add closure_id (T.alias_type_of K.value result)
  in
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:set_of_closures_ty
    ~shape:(T.set_of_closures_containing_at_least closures)
    ~result_var ~result_kind:K.value

let simplify_move_within_set_of_closures ~move_from ~move_to
      dacc ~original_term ~arg:closure ~arg_ty:closure_ty ~result_var =
  (* CR mshinwell: We're assuming here that the argument to the move is
     always the closure whose ID is [move_from].  We should document this
     somewhere most probably, e.g. flambda_primitive.mli. *)
  (* CR mshinwell: We talked about enhancing [Row_like] so a tag could be
     specified in the "at least" cases.  In this case we would set the tag
     to [move_from].  Think again as to whether we really need this, as it
     will complicate [Row_like]. *)
  let result = Simple.var (Var_in_binding_pos.var result_var) in
  let closures =
    Closure_id.Map.empty
    |> Closure_id.Map.add move_from (T.alias_type_of K.value closure)
    |> Closure_id.Map.add move_to (T.alias_type_of K.value result)
  in
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:closure_ty
    ~shape:(T.set_of_closures_containing_at_least closures)
    ~result_var ~result_kind:K.value

let simplify_project_var closure_element dacc ~original_term
      ~arg:_ ~arg_ty:closure_ty ~result_var =
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:closure_ty
    ~shape:(T.closure_containing_at_least closure_element
      ~closure_element_var:(Var_in_binding_pos.var result_var))
    ~result_var ~result_kind:K.value

let simplify_unbox_number (boxable_number_kind : K.Boxable_number.t)
      dacc ~original_term ~arg:_ ~arg_ty:boxed_number_ty ~result_var =
  let shape, result_kind =
    let result_var = Var_in_binding_pos.var result_var in
    match boxable_number_kind with
    | Naked_float ->
      T.boxed_float_alias_to ~naked_float:result_var, K.naked_float
    | Naked_int32 ->
      T.boxed_int32_alias_to ~naked_int32:result_var, K.naked_int32
    | Naked_int64 ->
      T.boxed_int64_alias_to ~naked_int64:result_var, K.naked_int64
    | Naked_nativeint ->
      T.boxed_nativeint_alias_to ~naked_nativeint:result_var, K.naked_nativeint
  in
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:boxed_number_ty
    ~shape ~result_var ~result_kind

let simplify_box_number (boxable_number_kind : K.Boxable_number.t)
      dacc ~original_term ~arg:_ ~arg_ty:naked_number_ty ~result_var =
  let ty =
    match boxable_number_kind with
    | Naked_float -> T.box_float naked_number_ty
    | Naked_int32 -> T.box_int32 naked_number_ty
    | Naked_int64 -> T.box_int64 naked_number_ty
    | Naked_nativeint -> T.box_nativeint naked_number_ty
  in
  Reachable.reachable original_term,
    TEE.one_equation (Name.var (Var_in_binding_pos.var result_var)) ty,
    dacc

let simplify_is_int dacc ~original_term ~arg: _~arg_ty:scrutinee_ty
      ~result_var =
  let name = Name.var (Var_in_binding_pos.var result_var) in
  let typing_env = DE.typing_env (DA.denv dacc) in
  let proof = T.prove_is_int typing_env scrutinee_ty in
  let proved discriminant =
    let ty = T.this_discriminant discriminant in
    let env_extension = TEE.one_equation name ty in
    Reachable.reachable original_term, env_extension, dacc
  in
  match proof with
  | Proved true -> proved Discriminant.bool_true
  | Proved false -> proved Discriminant.bool_false
  | Unknown ->
    let ty = T.these_discriminants Discriminant.all_bools_set in
    Reachable.reachable original_term, TEE.one_equation name ty, dacc
  | Invalid ->
    let ty = T.bottom K.fabricated in
    Reachable.invalid (), TEE.one_equation name ty, dacc

let simplify_get_tag dacc ~original_term ~arg:_ ~arg_ty:block_ty ~result_var =
  let name = Name.var (Var_in_binding_pos.var result_var) in
  let typing_env = DE.typing_env (DA.denv dacc) in
  let type_for_tags tags_to_sizes =
    let discrs =
      Tag.Map.fold (fun tag _size discrs ->
          let discr = Discriminant.of_tag tag in
          Discriminant.Set.add discr discrs)
        tags_to_sizes
        Discriminant.Set.empty
    in
    T.these_discriminants discrs
  in
  match T.prove_tags_and_sizes typing_env block_ty with
  | Proved tags ->
    let ty = type_for_tags tags in
    Reachable.reachable original_term, TEE.one_equation name ty, dacc
  | Unknown ->
    let ty = T.unknown K.fabricated in
    Reachable.reachable original_term, TEE.one_equation name ty, dacc
  | Invalid ->
    let ty = T.bottom K.fabricated in
    Reachable.invalid (), TEE.one_equation name ty, dacc

let simplify_array_length dacc ~original_term ~arg:_ ~arg_ty:array_ty
      ~result_var =
  let result = Simple.var (Var_in_binding_pos.var result_var) in
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:array_ty
    ~shape:(T.array_of_length ~length:(T.alias_type_of_as_ty_value result))
    ~result_var ~result_kind:K.value

let try_cse dacc prim arg ~min_occurrence_kind ~result_var
      : Simplify_primitive_common.cse =
  match S.simplify_simple dacc arg ~min_occurrence_kind with
  | Bottom, ty -> Invalid ty
  | Ok arg, _arg_ty ->
    let original_prim : P.t = Unary (prim, arg) in
    let result_kind = P.result_kind_of_unary_primitive' prim in
    Simplify_primitive_common.try_cse dacc ~original_prim ~result_kind
      ~min_occurrence_kind ~result_var

let simplify_unary_primitive dacc (prim : P.unary_primitive)
      arg dbg ~result_var =
(*
Format.eprintf "Simplifying %a\n%!" P.print
  ((P.Unary (prim, arg)) : P.t);
*)
  let min_occurrence_kind = Var_in_binding_pos.occurrence_kind result_var in
  let result_var' = Var_in_binding_pos.var result_var in
  let invalid ty =
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Reachable.invalid (), env_extension, dacc
  in
  match try_cse dacc prim arg ~min_occurrence_kind ~result_var:result_var' with
  | Invalid ty -> invalid ty
  | Applied result -> result
  | Not_applied dacc ->
    match S.simplify_simple dacc arg ~min_occurrence_kind with
    | Bottom, ty -> invalid ty
    | Ok arg, arg_ty ->
      let original_prim : P.t = Unary (prim, arg) in
      let original_term = Named.create_prim original_prim dbg in
      let simplifier =
        match prim with
        | Project_closure closure_id -> simplify_project_closure closure_id
        | Project_var closure_element -> simplify_project_var closure_element
        | Move_within_set_of_closures { move_from; move_to; } ->
          simplify_move_within_set_of_closures ~move_from ~move_to
        | Unbox_number boxable_number_kind ->
          simplify_unbox_number boxable_number_kind
        | Box_number boxable_number_kind ->
          simplify_box_number boxable_number_kind
        | Is_int -> simplify_is_int
        | Get_tag -> simplify_get_tag
        | Array_length (Array (Value _)) -> simplify_array_length
        | Duplicate_block _
        | Array_length _
        | Bigarray_length _
        | String_length _
        | Int_as_pointer
        | Opaque_identity
        | Int_arith _
        | Float_arith _
        | Num_conv _
        | Boolean_not ->
          fun dacc ~original_term:_ ~arg ~arg_ty:_ ~result_var:_ ->
            (* CR mshinwell: temporary code *)
            let named = Named.create_prim (Unary (prim, arg)) dbg in
            let kind = P.result_kind_of_unary_primitive' prim in
            let ty = T.unknown kind in
            let env_extension = TEE.one_equation (Name.var result_var') ty in
            Reachable.reachable named, env_extension, dacc
      in
      simplifier dacc ~original_term ~arg ~arg_ty ~result_var
