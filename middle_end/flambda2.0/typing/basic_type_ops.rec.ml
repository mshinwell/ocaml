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

open Type_grammar


let apply_rec_info_of_kind_naked_number (type k)
      (of_kind_naked_number : k Type_grammar.of_kind_naked_number) rec_info
      : k Type_grammar.of_kind_naked_number Or_bottom.t =
  if Rec_info.is_initial rec_info then Ok of_kind_naked_number
  else Bottom

let apply_rec_info_ty (type of_kind_foo)
      (apply_rec_info_of_kind_foo :
        (of_kind_foo -> Rec_info.t -> of_kind_foo Or_bottom.t))
      (ty : of_kind_foo Type_grammar.ty)
      rec_info : of_kind_foo Type_grammar.ty Or_bottom.t =
  match ty with
  | Equals simple ->
    let newer_rec_info = Some rec_info in
    begin match Simple.merge_rec_info simple ~newer_rec_info with
    | None -> Bottom
    | Some simple -> Ok (Equals simple)
    end
  | Type _ -> Misc.fatal_error "Not yet implemented"
  | No_alias Unknown -> Ok ty
  | No_alias Bottom -> Bottom
  | No_alias (Ok of_kind_foo) ->
    match apply_rec_info_of_kind_foo of_kind_foo rec_info with
    | Bottom -> Bottom
    | Ok of_kind_foo -> Ok (No_alias (Ok of_kind_foo))


and apply_rec_info_of_kind_value (of_kind_value : Type_grammar.of_kind_value)
      rec_info : Type_grammar.of_kind_value Or_bottom.t =
  match of_kind_value with
  | Closures { by_closure_id; } ->
    Or_bottom.map
      (Closures_entry_by_set_of_closures_contents.map_function_decl_types
        by_closure_id
        ~f:(fun (decl : Type_grammar.function_declaration)
              : Type_grammar.function_declaration Or_bottom.t ->
          match decl with
          | Non_inlinable _ -> Ok decl
          | Inlinable { function_decl; rec_info = old_rec_info; } ->
            let rec_info = Rec_info.merge old_rec_info ~newer:rec_info in
            Ok (Inlinable { function_decl; rec_info; })))
      ~f:(fun by_closure_id -> Closures { by_closure_id; })
  | Blocks_and_tagged_immediates _
  | Boxed_number _
  | String _
  | Array _ ->
    if Rec_info.is_initial rec_info then Ok of_kind_value
    else Bottom

and apply_rec_info_of_kind_fabricated
      (of_kind_fabricated : Type_grammar.of_kind_fabricated)
      rec_info : Type_grammar.of_kind_fabricated Or_bottom.t =
  match of_kind_fabricated with
  | Discriminants _ ->
    if Rec_info.is_initial rec_info then Ok of_kind_fabricated
    else Bottom

let apply_name_permutation_unknown_or_join apply_name_permutation_of_kind_foo
      (unknown_or_join : _ Type_grammar.unknown_or_join) perm
      : _ Type_grammar.unknown_or_join =
  match unknown_or_join with
  | Unknown | Bottom -> unknown_or_join
  | Ok of_kind_foo ->
    let of_kind_foo' = apply_name_permutation_of_kind_foo of_kind_foo perm in
    if of_kind_foo == of_kind_foo' then unknown_or_join
    else Ok of_kind_foo'

let apply_name_permutation_ty apply_name_permutation_of_kind_foo
      (ty : _ Type_grammar.ty) perm
      : _ Type_grammar.ty =
  match ty with
  | No_alias unknown_or_join ->
    let unknown_or_join' =
      apply_name_permutation_unknown_or_join apply_name_permutation_of_kind_foo
        unknown_or_join perm
    in
    if unknown_or_join == unknown_or_join' then ty
    else No_alias unknown_or_join'
  | Type _ -> ty
  | Equals simple ->
    let simple' = Simple.apply_name_permutation simple perm in
    if simple == simple' then ty
    else Equals simple'

let apply_name_permutation_of_kind_naked_number (type n)
      (of_kind_naked_number : n Type_grammar.of_kind_naked_number) _perm
      : n Type_grammar.of_kind_naked_number =
  of_kind_naked_number

let rec apply_name_permutation (t : Type_grammar.t) perm : Type_grammar.t =
  match t with
  | Value ty_value ->
    let ty_value' =
      apply_name_permutation_ty apply_name_permutation_of_kind_value
        ty_value perm
    in
    if ty_value == ty_value' then t
    else Value ty_value'
  | Naked_number (ty_naked_number, kind) ->
    let ty_naked_number' =
      apply_name_permutation_ty apply_name_permutation_of_kind_naked_number
        ty_naked_number perm
    in
    if ty_naked_number == ty_naked_number' then t
    else Naked_number (ty_naked_number', kind)
  | Fabricated ty_fabricated ->
    let ty_fabricated' =
      apply_name_permutation_ty apply_name_permutation_of_kind_fabricated
        ty_fabricated perm
    in
    if ty_fabricated == ty_fabricated' then t
    else Fabricated ty_fabricated'


and apply_name_permutation_of_kind_fabricated
      (of_kind_fabricated : Type_grammar.of_kind_fabricated) perm
      : Type_grammar.of_kind_fabricated =
  match of_kind_fabricated with
  | Discriminants discrs ->
    let discrs' = Discriminants.apply_name_permutation discrs perm in
    if discrs == discrs' then of_kind_fabricated
    else Discriminants discrs'
