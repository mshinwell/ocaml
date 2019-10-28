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

let simplify_make_block dacc _prim dbg
      ~(make_block_kind : P.make_block_kind)
      ~(mutable_or_immutable : Effects.mutable_or_immutable)
      args_with_tys ~result_var =
      (*
Format.eprintf "simplifying make_block on %a (num args %d)\n%!"
  Variable.print result_var
  (List.length args_with_tys);
  *)
  let denv = DA.denv dacc in
  let args, _arg_tys = List.split args_with_tys in
  let invalid () =
    let ty = T.bottom K.value in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Reachable.invalid (), env_extension, dacc
  in
  match make_block_kind with
  | Full_of_values (tag, value_kinds) ->
    if List.compare_lengths value_kinds args <> 0 then begin
      (* CR mshinwell: improve message *)
      Misc.fatal_errorf "GC value_kind indications in [Make_block] don't \
          match up 1:1 with arguments: %a"
        Simple.List.print args
    end;
    (* CR mshinwell: This could probably be done more neatly. *)
    let found_bottom = ref false in
    let fields =
      assert (List.compare_lengths value_kinds args_with_tys = 0);
      List.map2 (fun ((arg : Simple.t), arg_ty) _value_kind ->
          if T.is_bottom (DE.typing_env denv) arg_ty then begin
           found_bottom := true
          end;
          match Simple.descr arg with
          | Const _ -> arg_ty
          | Name name -> T.alias_type_of K.value (Simple.name name))
        args_with_tys value_kinds
    in
    if !found_bottom then begin
      invalid ()
    end else begin
      assert (List.compare_lengths fields value_kinds = 0);
      let term : Named.t =
        Named.create_prim
          (Variadic (
            Make_block (Full_of_values (tag, value_kinds),
              mutable_or_immutable),
            args))
          dbg
      in
      let tag = Tag.Scannable.to_tag tag in
      let ty =
        match mutable_or_immutable with
        | Immutable -> T.immutable_block tag ~fields
        | Mutable -> T.any_value ()
      in
      let env_extension = TEE.one_equation (Name.var result_var) ty in
      Reachable.reachable term, env_extension, dacc
    end
  | Full_of_naked_floats -> Misc.fatal_error "Not yet implemented"
  | Generic_array _spec -> Misc.fatal_error "Not yet implemented"

let try_cse dacc prim args ~min_occurrence_kind ~result_var
      : Simplify_common.cse =
  match
    S.simplify_simples dacc args
      ~min_occurrence_kind:Name_occurrence_kind.min_in_types
  with
  | Bottom ->
    let kind = P.result_kind_of_variadic_primitive' prim in
    Invalid (T.bottom kind)
  | Ok args_with_tys ->
    let args, _tys = List.split args_with_tys in
    let original_prim : P.t = Variadic (prim, args) in
    let result_kind =
      P.result_kind_of_variadic_primitive' prim
    in
    Simplify_common.try_cse dacc ~original_prim ~result_kind
      ~min_occurrence_kind ~result_var

let simplify_variadic_primitive dacc
      (prim : P.variadic_primitive) args dbg ~result_var =
  let min_occurrence_kind = Var_in_binding_pos.occurrence_kind result_var in
  let result_var' = Var_in_binding_pos.var result_var in
  let invalid ty =
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Reachable.invalid (), env_extension, dacc
  in
  match try_cse dacc prim args ~min_occurrence_kind ~result_var:result_var' with
  | Invalid ty -> invalid ty
  | Applied result -> result
  | Not_applied dacc ->
    match S.simplify_simples dacc args ~min_occurrence_kind with
    | Bottom ->
      let kind = P.result_kind_of_variadic_primitive' prim in
      invalid (T.bottom kind)
    | Ok args_with_tys ->
      match prim with
      | Make_block ((Full_of_values _) as make_block_kind, mutable_or_immutable) ->
        simplify_make_block dacc prim dbg ~make_block_kind ~mutable_or_immutable
          args_with_tys ~result_var:result_var'
      | Make_block _
      | Bigarray_set _ (*(_is_safe, _num_dims, _kind, _layout) *)
      | Bigarray_load _ -> (* (_is_safe, _num_dims, _kind, _layout) -> *)
        let named = Named.create_prim (Variadic (prim, args)) dbg in
        let kind = P.result_kind_of_variadic_primitive' prim in
        let ty = T.unknown kind in
        let env_extension = TEE.one_equation (Name.var result_var') ty in
        Reachable.reachable named, env_extension, dacc
