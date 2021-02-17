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

let simplify_make_block_of_values dacc _prim dbg tag ~shape
      ~(mutable_or_immutable : Mutability.t)
      args_with_tys ~result_var =
  let denv = DA.denv dacc in
  let args, _arg_tys = List.split args_with_tys in
  let invalid () =
    let ty = T.bottom K.value in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.invalid (), env_extension, args, dacc
  in
  if List.compare_lengths shape args <> 0 then begin
    (* CR mshinwell: improve message *)
    Misc.fatal_errorf "GC value_kind indications in [Make_block] don't \
        match up 1:1 with arguments: %a"
      Simple.List.print args
  end;
  (* CR mshinwell: This could probably be done more neatly. *)
  let found_bottom = ref false in
  let fields =
    List.map2 (fun ((arg : Simple.t), arg_ty) _block_of_values_kind ->
        (* CR mshinwell: There should be a meet against a skeleton type
           computed from [block_of_values_kind]. *)
        if T.is_bottom (DE.typing_env denv) arg_ty then begin
          found_bottom := true
        end;
        Simple.pattern_match arg
          ~const:(fun _ -> arg_ty)
          ~name:(fun name -> T.alias_type_of K.value (Simple.name name)))
      args_with_tys shape
  in
  if !found_bottom then begin
    invalid ()
  end else begin
    assert (List.compare_lengths fields shape = 0);
    let term : Named.t =
      Named.create_prim
        (Variadic (
          Make_block (Values (tag, shape), mutable_or_immutable),
          args))
        dbg
    in
    let tag = Tag.Scannable.to_tag tag in
    let ty =
      match mutable_or_immutable with
      | Immutable ->
        T.immutable_block ~is_unique:false tag ~field_kind:K.value ~fields
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~field_kind:K.value ~fields
      | Mutable -> T.any_value ()
    in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.reachable term, env_extension, args, dacc
  end

let simplify_variadic_primitive dacc (prim : P.variadic_primitive)
      ~args_with_tys dbg ~result_var =
  let result_var' = Var_in_binding_pos.var result_var in
  match prim with
  | Make_block (Values (tag, shape), mutable_or_immutable) ->
    simplify_make_block_of_values dacc prim dbg tag ~shape
      ~mutable_or_immutable
      args_with_tys ~result_var:result_var'
  | Make_block (Naked_floats, _) | Make_array _ ->
    (* CR mshinwell: The typing here needs to be improved *)
    let args, _tys = List.split args_with_tys in
    let named = Named.create_prim (Variadic (prim, args)) dbg in
    let ty =
      match prim with
      | Make_block _ -> T.any_block ()
      | Make_array _ ->
        let length =
          match Targetint.OCaml.of_int_option (List.length args) with
          | Some ti ->
            T.this_tagged_immediate (Target_imm.int ti)
          | None ->
            T.unknown K.value
        in
        T.array_of_length ~length
    in
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Simplified_named.reachable named, env_extension, args, dacc
