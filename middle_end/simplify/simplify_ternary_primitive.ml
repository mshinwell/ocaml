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
module T = Flambda_type

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int = Numbers.Int
module Named = Flambda.Named
module Reachable = Flambda.Reachable

let simplify_block_set_known_index env r prim ~block_access_kind
      ~init_or_assign ~block ~index ~new_value dbg =
  if field < 0 then begin
    Misc.fatal_errorf "[Block_set] with bad field index %d: %a"
      field
      Flambda_primitive.print prim
  end;
  let field_kind' = Flambda_primitive.kind_of_block_set_kind field_kind in
  let original_term () : Named.t = Prim (Binary (prim, block, index), dbg) in
  let result_kind = K.unit () in
  let invalid () = Reachable.invalid (), T.bottom result_kind in
  let ok () =
    match new_value_proof with
    | Proved _ ->
      Reachable.reachable (original_term ()), T.unknown result_kind Other
    | Invalid -> invalid ()
  in
  let block_ty, r =
    refine_block_ty_upon_access env r ~block ~block_ty ~field_index
      ~block_access_kind Store
  in
  match block_access_kind with
  | Definitely_immediate | Must_scan ->
    let proof = (E.type_accessor env T.prove_blocks) block_ty in
    begin match proof with
    | Proved blocks ->
      if not (T.Blocks.valid_field_access blocks ~field) then invalid ()
      else ok ()
    | Unknown -> ok ()
    | Invalid -> invalid ()
    end
  | Naked_float ->
    let block_proof = (E.type_accessor env T.prove_float_array) block_ty in
    let new_value_proof =
      (E.type_accessor env T.prove_naked_float) new_value_ty
    in
    begin match block_proof with
    | Proved sizes ->
      if not (Int.Set.exists (fun size -> size > field) sizes) then invalid ()
      else ok ()
    | Proved Unknown -> ok ()
    | Invalid -> invalid ()
    end
  | Generic_array _spec -> Misc.fatal_error "Not yet implemented"
    (* CR mshinwell: Finish off
    Simplify_generic_array.simplify_block_set env r prim ~field spec args
    *)

let simplify_block_set env r prim dbg ~block_access_kind ~init_or_assign
      ~block ~index ~new_value =
  let block, block_ty = S.simplify_simple env block in
  let index, index_ty = S.simplify_simple env index in
  let new_value, new_value_ty = S.simplify_simple env new_value in
  let original_term () : Named.t = Prim (Binary (prim, block, index), dbg) in
  let invalid () = Reachable.invalid (), T.bottom field_kind in
  let proof = (E.type_accessor env T.prove_tagged_immediate) arg in
  let unique_index_unknown () =
    (* XXX See [Simplify_binary_primitive.simplify_block_load] *)
    if (E.type_accessor env T.is_bottom) ty then
      invalid ()
    else
      Reachable.reachable (original_term ()), T.unknown field_kind Other
  in
  let term, ty =
    match proof with
    | Proved (Exactly indexes) ->
      begin match Immediate.Set.get_singleton indexes with
      | Some index ->
        simplify_block_set_known_index env r prim ~block_access_kind
          ~init_or_assign ~block ~index ~new_value dbg
      | None -> unique_index_unknown ()
      end
    | Proved Not_all_values_known -> unique_index_unknown ()
    | Invalid -> invalid ()
  in
  term, ty, r

let simplify_bytes_or_bigstring_set env r prim dbg bytes_like_value
      string_accessor_width ~str ~index ~new_value =
  let str, str_ty = S.simplify_simple env str in
  let index, index_ty = S.simplify_simple env index in
  let new_value, new_value_ty = S.simplify_simple env new_value in
  let original_term () : Named.t =
    Prim (Ternary (prim, str, index, new_value), dbg)
  in
  let result_kind = Flambda_kind.unit () in
  let invalid () = Reachable.invalid (), T.bottom result_kind in
  (* CR mshinwell: Factor out with
     [Simplify_binary_primitive.simplify_string_or_bigstring_load].  In fact,
     maybe we could share the whole skeleton of this function? *)
  let str_proof : T.string_proof =
    match string_like_value with
    | String | Bytes -> (E.type_accessor env T.prove_string) str
    | Bigstring ->
      (* For the moment just check that the bigstring is of kind [Value]. *)
      let proof =
        (E.type_accessor env T.prove_of_kind_value_with_expected_scanning
          Must_scan) str
      in
      match proof with
      | Proved _ ->
        (* At the moment we don't track anything in the type system about
           bigarrays. *)
        Proved Not_all_values_known
      | Invalid -> Invalid
  in
  let index_proof = (E.type_accessor env T.prove_tagged_immediate) index in
  let all_the_empty_string strs =
    (* CR mshinwell: Move into [T.String_info] *)
    T.String_info.Set.for_all (fun (info : T.String_info.t) ->
        info.size = 0)
      strs
  in
  let check_new_value () =
    let new_value_proof =
      (E.type_accessor env T.prove_of_kind result_kind) new_value
    in
    match new_value_proof with
    | Proved _ ->
      [], Reachable.reachable (original_term ()),
        T.unknown result_kind Other, r
    | Invalid -> invalid ()
  in
  match str_proof, index_proof with
  | Proved (Exactly strs), Proved (Exactly indexes) ->
    assert (not (T.String_info.Set.is_empty strs));
    assert (not (Immediate.Set.is_empty indexes));
    let strs_and_indexes =
      String_info_and_immediate.Set.create_from_cross_product strs indexes
    in
    let at_least_one_valid_case =
      String_info_and_immediate.Set.exits
        (fun ((info : T.String_info.t), index_in_bytes) ->
          let in_range =
            XXX.bounds_check ~string_length_in_bytes:info.size ~index_in_bytes
              ~result_size_in_bytes
          in
          match in_range with
          | Out_of_range -> false
          | In_range -> true)
        strs_and_indexes
    in
    if at_least_one_valid_case then check_new_value ()
    else invalid ()
  | Proved strs, Proved Not_all_values_known ->
    assert (not (T.String_info.Set.is_empty strs));
    if all_the_empty_string strs then invalid ()
    else check_new_value ()
  | Proved Not_all_values_known, Proved indexes ->
    assert (not (Immediate.Set.is_empty indexes));
    let max_string_length =
      match string_like_value with
      | String | Bytes -> Targetint.OCaml.max_string_length
      | Bigstring -> Targetint.OCaml.max_int
    in
    let all_indexes_out_of_range =
      all_indexes_out_of_range indexes ~max_string_length
        ~result_size_in_bytes
    in
    if all_indexes_out_of_range indexes then invalid ()
    else check_new_value ()
  | Invalid _, _ | _, Invalid _ -> invalid ()

let simplify_ternary_primitive env r prim arg1 arg2 arg3 dbg =
  match prim with
  | Block_set (value_kind, init_or_assign) ->
    simplify_block_set env r prim dbg ~value_kind ~init_or_assign
      arg1 arg2 arg3
  | Bytes_set string_accessor_width ->
    simplify_bytes_set env r prim dbg ~string_accessor_width arg1 arg2 arg3
  | Bigstring_set bigstring_accessor_width ->
    simplify_bigstring_set env r prim dbg ~bigstring_accessor_width
      arg1 arg2 arg3
