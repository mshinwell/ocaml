(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module B = Inlining_cost.Benefit
module E = Simplify_env_and_result.Env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type

module Named = Flambda.Named
module Reachable = Flambda.Reachable

let simplify_block_set_known_index env r _prim ~block_access_kind
      ~init_or_assign:_ ~block_ty ~index ~new_value:_ _dbg
      ~original_term ~invalid =
  if Targetint.OCaml.compare index Targetint.OCaml.zero < 0 then begin
    Misc.fatal_errorf "[Block_set] with bad field index %a: %a"
      Targetint.OCaml.print index
      T.print block_ty
  end;
  let field_kind =
    Flambda_primitive.Block_access_kind.kind_this_element block_access_kind
  in
  let result_kind = K.unit () in
  let ok () =
    Reachable.reachable (original_term ()), T.unknown result_kind, r
  in
  let proof =
    (* Even though we're setting not getting, the "prove get field" function
       does exactly what we want in terms of checks on the block, to determine
       if the set is invalid. *)
    (E.type_accessor env T.prove_get_field_from_block) block_ty ~index
      ~field_kind
  in
  begin match proof with
  | Proved _ | Unknown -> ok ()
  | Invalid -> invalid ()
  end

let simplify_block_set env r prim dbg ~block_access_kind ~init_or_assign
      ~block ~index ~new_value =
  let block, block_ty = S.simplify_simple env block in
  let index, index_ty = S.simplify_simple env index in
  let new_value, new_value_ty = S.simplify_simple env new_value in
  let original_term () : Named.t =
    Prim (Ternary (prim, block, index, new_value), dbg)
  in
  let kind_of_all_fields =
    Flambda_primitive.Block_access_kind.kind_all_elements block_access_kind
  in
  let field_kind =
    Flambda_primitive.Block_access_kind.kind_this_element block_access_kind
  in
  let invalid () =
    Reachable.invalid (), T.bottom (K.unit ()),
      R.map_benefit r (B.remove_primitive (Ternary prim))
  in
  let new_value_kind = (E.type_accessor env T.kind) new_value_ty in
  if not (K.compatible new_value_kind ~if_used_at:field_kind) then begin
    Misc.fatal_errorf "New value for [Block_set] has kind %a, incompatible \
        with %a.  Block type: %a"
      K.print new_value_kind
      K.print field_kind
      T.print block_ty
  end;
  let index_proof = (E.type_accessor env T.prove_tagged_immediate) index_ty in
  let unique_index_unknown () =
    let block_proof =
      (E.type_accessor env T.prove_is_a_block) block_ty ~kind_of_all_fields
    in
    match block_proof with
    | Unknown | Proved true ->
      Reachable.reachable (original_term ()), T.unknown field_kind, r
    | Proved false | Invalid -> invalid ()
  in
  match index_proof with
  | Proved indexes ->
    begin match Immediate.Set.get_singleton indexes with
    | Some index ->
      let index = Immediate.to_targetint index in
      simplify_block_set_known_index env r prim ~block_access_kind
        ~init_or_assign ~block_ty ~index ~new_value dbg
        ~original_term ~invalid
    | None -> unique_index_unknown ()
    end
  | Unknown -> unique_index_unknown ()
  | Invalid -> invalid ()

let simplify_bytes_or_bigstring_set env r prim dbg
      (bytes_like_value : Flambda_primitive.bytes_like_value)
      ~string_accessor_width ~str ~index ~new_value =
  let str, str_ty = S.simplify_simple env str in
  let index, index_ty = S.simplify_simple env index in
  let new_value, new_value_ty = S.simplify_simple env new_value in
  let original_term () : Named.t =
    Prim (Ternary (prim, str, index, new_value), dbg)
  in
  let result_kind = Flambda_kind.unit () in
  let ok () = Reachable.reachable (original_term ()), T.unit (), r in
  let invalid () =
    Reachable.invalid (), T.bottom result_kind,
      R.map_benefit r (B.remove_primitive (Ternary prim))
  in
  (* For the moment just check that the container is of kind [Value]:
     we don't track anything in the type system about bigarrays or values
     of type [bytes]. *)
  let _ty_value =
    (E.type_accessor env T.prove_of_kind_value_with_expected_value_kind)
      str_ty Unknown
    (* CR mshinwell: We should do more here (e.g. make sure the type is
       Unknown) -- it would rule out obviously wrong cases such as
       being presented with a closure instead of a string. *)
  in
  let new_value_kind = (E.type_accessor env T.kind) new_value_ty in
  let field_kind =
    Flambda_primitive.kind_of_string_accessor_width string_accessor_width
  in
  if not (K.compatible new_value_kind ~if_used_at:field_kind) then begin
    Misc.fatal_errorf "New value for bytes / Bigstring set has kind %a, \
        incompatible with %a."
      K.print new_value_kind
      K.print field_kind
  end;
  let index_proof = (E.type_accessor env T.prove_tagged_immediate) index_ty in
  match index_proof with
  | Proved indexes ->
    assert (not (Immediate.Set.is_empty indexes));
    let max_string_length =
      match bytes_like_value with
      | Bytes -> Targetint.OCaml.max_string_length
      | Bigstring -> Targetint.OCaml.max
    in
    let all_indexes_out_of_range =
      Simplify_aux.all_indexes_out_of_range ~width:string_accessor_width
        indexes ~max_string_length
    in
    if all_indexes_out_of_range then invalid ()
    else ok ()
  | Unknown -> ok ()
  | Invalid -> invalid ()

let simplify_ternary_primitive env r
      (prim : Flambda_primitive.ternary_primitive) arg1 arg2 arg3 dbg =
  match prim with
  | Block_set (block_access_kind, init_or_assign) ->
    simplify_block_set env r prim dbg ~block_access_kind ~init_or_assign
      ~block:arg1 ~index:arg2 ~new_value:arg3
  | Bytes_or_bigstring_set (bytes_like_value, string_accessor_width) ->
    simplify_bytes_or_bigstring_set env r prim dbg bytes_like_value
      ~string_accessor_width ~str:arg1 ~index:arg2 ~new_value:arg3
