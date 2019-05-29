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

let erase_aliases_or_alias ~allowed erase_aliases_contents
      (or_alias : _ Flambda_types.or_alias)
      : _ Flambda_types.unknown_or_join Flambda_types.or_alias =
  match or_alias with
  | No_alias contents -> No_alias (erase_aliases_contents ~allowed contents)
  | Type _export_id -> or_alias
  | Equals simple ->
    match simple with
    | Const _ | Discriminant _ | Name (Symbol _) -> or_alias
    | Name (Var var) ->
      if Variable.Set.mem var allowed then or_alias
      else No_alias Unknown

let erase_aliases_unknown_or_join ~allowed erase_aliases_contents
      (o : _ Flambda_types.unknown_or_join) : _ Flambda_types.unknown_or_join =
  match o with
  | Unknown | Bottom -> o
  | Ok contents -> Ok (erase_aliases_contents ~allowed contents)

let erase_aliases_ty ~allowed erase_aliases_contents ty =
  erase_aliases_or_alias ~allowed
    (erase_aliases_unknown_or_join erase_aliases_contents)
    ty

let erase_aliases_of_kind_naked_number (type n) ~allowed:_
      (ty : n Flambda_types.of_kind_naked_number) = ty

let rec erase_aliases ~allowed (t : Flambda_types.t) : Flambda_types.t =
  match t with
  | Value ty ->
    Value (erase_aliases_ty ~allowed erase_aliases_of_kind_value ty)
  | Naked_number (ty, kind) ->
    Naked_number (
      erase_aliases_ty ~allowed erase_aliases_of_kind_naked_number ty,
      kind)
  | Fabricated ty ->
    Fabricated (erase_aliases_ty ~allowed erase_aliases_of_kind_fabricated ty)

and erase_aliases_of_kind_value ~allowed (of_kind : Flambda_types.of_kind_value)
      : Flambda_types.of_kind_value =
  match of_kind with
  | Blocks_and_tagged_immediates { blocks; immediates; } ->
    let blocks =
      Or_unknown.map blocks ~f:(fun blocks ->
        Blocks.erase_aliases blocks ~allowed)
    in
    let immediates =
      Or_unknown.map immediates ~f:(fun immediates ->
        Immediates.erase_aliases immediates ~allowed);
    in
    Blocks_and_tagged_immediates {
      blocks;
      immediates;
    }
  | Boxed_number (Boxed_float ty) ->
    Boxed_number (Boxed_float (
      erase_aliases_ty ~allowed erase_aliases_of_kind_naked_number ty))
  | Boxed_number (Boxed_int32 ty) ->
    Boxed_number (Boxed_int32 (
      erase_aliases_ty ~allowed erase_aliases_of_kind_naked_number ty))
  | Boxed_number (Boxed_int64 ty) ->
    Boxed_number (Boxed_int64 (
      erase_aliases_ty ~allowed erase_aliases_of_kind_naked_number ty))
  | Boxed_number (Boxed_nativeint ty) ->
    Boxed_number (Boxed_nativeint (
      erase_aliases_ty ~allowed erase_aliases_of_kind_naked_number ty))
  | Closures { by_closure_id; } ->
    Closures {
      by_closure_id =
        Closures_entry_by_closure_id.erase_aliases by_closure_id ~allowed;
    }
  | String _ -> of_kind

and erase_aliases_of_kind_fabricated ~allowed
      (of_kind : Flambda_types.of_kind_fabricated)
      : Flambda_types.of_kind_fabricated =
  match of_kind with
  | Discriminants discrs ->
    Discriminants (Discriminants.erase_aliases discrs ~allowed)
  | Set_of_closures { closures; } ->
    Set_of_closures {
      closures = Closure_ids.erase_aliases closures ~allowed;
    }

let erase_aliases_ty_fabricated ~allowed ty =
  erase_aliases_ty ~allowed erase_aliases_of_kind_fabricated ty
