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

module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module K = Flambda_kind
module T = Flambda_type
module TEE = Flambda_type.Typing_env_extension

let simplify_project_closure dacc ~original_term ~set_of_closures_ty closure_id
      ~result_var =
  let result = Simple.var (Var_in_binding_pos.var result_var) in
  let closures =
    Closure_id.Map.empty
    |> Closure_id.Map.add closure_id (T.alias_type_of K.value result)
  in
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:set_of_closures_ty
    ~shape:(T.set_of_closures_containing_at_least closures)
    ~result_var ~result_kind:K.value

let simplify_move_within_set_of_closures dacc ~original_term ~closure
      ~closure_ty ~move_from ~move_to ~result_var =
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

let simplify_project_var dacc ~original_term ~closure_ty closure_element
      ~result_var =
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:closure_ty
    ~shape:(T.closure_containing_at_least closure_element
      ~closure_element_var:(Var_in_binding_pos.var result_var))
    ~result_var ~result_kind:K.value

let simplify_unbox_number dacc ~original_term ~boxed_number_ty
      (boxable_number_kind : K.Boxable_number.t) ~result_var =
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

let simplify_box_number dacc ~original_term ~naked_number_ty
      (boxable_number_kind : K.Boxable_number.t) ~result_var =
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

let simplify_is_int dacc ~original_term ~scrutinee_ty ~result_var =
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

let simplify_get_tag dacc ~original_term ~tags_to_sizes:claimed_tags
      ~block_ty ~result_var =
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
  | Proved inferred_tags ->
    let tags =
      Tag.Map.merge (fun tag claimed inferred ->
          (* CR mshinwell: Maybe one or both of these fatal error cases
             should actually just be [Invalid]. *)
          match claimed, inferred with
          | None, None
          | Some _, None -> None
          | Some claimed_size, Some inferred_size ->
            if Targetint.OCaml.equal claimed_size inferred_size then
              Some claimed_size
            else
              Misc.fatal_errorf "Binding of [Get_tag] to %a: primitive \
                  mentions tag %a with size %a, but inferred type has \
                  size %a:@ %a"
                Var_in_binding_pos.print result_var
                Tag.print tag
                Targetint.OCaml.print claimed_size
                Targetint.OCaml.print inferred_size
                T.print block_ty
          | None, Some size ->
            Misc.fatal_errorf "Binding of [Get_tag] to %a: primitive does \
                not mention tag %a / size %a combination, but inferred \
                type does:@ %a"
              Var_in_binding_pos.print result_var
              Tag.print tag
              Targetint.OCaml.print size
              T.print block_ty)
        claimed_tags
        inferred_tags
    in
    let ty = type_for_tags tags in
    Reachable.reachable original_term, TEE.one_equation name ty, dacc
  | Unknown ->
    let ty = type_for_tags claimed_tags in
    Reachable.reachable original_term, TEE.one_equation name ty, dacc
  | Invalid ->
    let ty = T.bottom K.fabricated in
    Reachable.invalid (), TEE.one_equation name ty, dacc

let simplify_discriminant_of_int dacc ~original_term ~int_ty ~result_var =
  let name = Name.var (Var_in_binding_pos.var result_var) in
  let typing_env = DE.typing_env (DA.denv dacc) in
  let invalid () =
    let ty = T.bottom K.fabricated in
    Reachable.invalid (), TEE.one_equation name ty, dacc
  in
  let proof = T.prove_equals_tagged_immediates typing_env int_ty in
  match proof with
  | Proved imms ->
    let discrs =
      Immediate.Set.fold (fun imm discrs ->
          let as_int = Immediate.to_targetint imm in
          match Discriminant.create as_int with
          | Some discr -> Discriminant.Set.add discr discrs
          | None -> discrs)
        imms
        Discriminant.Set.empty
    in
    if Discriminant.Set.cardinal discrs <> Immediate.Set.cardinal imms then
      invalid ()
    else
      let ty = T.these_discriminants discrs in
      let env_extension = TEE.one_equation name ty in
      Reachable.reachable original_term, env_extension, dacc
  | Unknown ->
    let ty = T.unknown K.fabricated in
    Reachable.reachable original_term, TEE.one_equation name ty, dacc
  | Invalid -> invalid ()

let simplify_unary_primitive dacc (prim : Flambda_primitive.unary_primitive)
      arg dbg ~result_var =
(*
Format.eprintf "Simplifying %a\n%!" Flambda_primitive.print
  ((Flambda_primitive.Unary (prim, arg)) : Flambda_primitive.t);
*)
  let min_occurrence_kind = Var_in_binding_pos.occurrence_kind result_var in
  let result_var' = Var_in_binding_pos.var result_var in
  match Simplify_simple.simplify_simple dacc arg ~min_occurrence_kind with
  | Bottom, ty ->
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Reachable.invalid (), env_extension, dacc
  | Ok arg, arg_ty ->
(*
Format.eprintf "simplify_unary_primitive: type of arg %a:@ %a\n%!"
  Simple.print arg
  T.print arg_ty;
*)
    let original_term = Named.create_prim (Unary (prim, arg)) dbg in
    match prim with
    | Project_closure closure_id ->
      simplify_project_closure dacc ~original_term ~set_of_closures_ty:arg_ty
        closure_id ~result_var
    | Project_var closure_element ->
      simplify_project_var dacc ~original_term ~closure_ty:arg_ty
        closure_element ~result_var
    | Move_within_set_of_closures { move_from; move_to; } ->
      simplify_move_within_set_of_closures dacc ~original_term ~closure:arg
        ~closure_ty:arg_ty ~move_from ~move_to ~result_var
    | Unbox_number boxable_number_kind ->
      simplify_unbox_number dacc ~original_term ~boxed_number_ty:arg_ty
        boxable_number_kind ~result_var
    | Box_number boxable_number_kind ->
      simplify_box_number dacc ~original_term ~naked_number_ty:arg_ty
        boxable_number_kind ~result_var
    | Is_int ->
      simplify_is_int dacc ~original_term ~scrutinee_ty:arg_ty ~result_var
    | Get_tag { tags_to_sizes; } ->
      simplify_get_tag dacc ~original_term ~tags_to_sizes ~block_ty:arg_ty
        ~result_var
    | Discriminant_of_int ->
      simplify_discriminant_of_int dacc ~original_term ~int_ty:arg_ty
        ~result_var
    | _ ->
      (* CR mshinwell: temporary code *)
      let named = Named.create_prim (Unary (prim, arg)) dbg in
      let kind = Flambda_primitive.result_kind_of_unary_primitive' prim in
      let ty = T.unknown kind in
      let env_extension = TEE.one_equation (Name.var result_var') ty in
      Reachable.reachable named, env_extension, dacc
