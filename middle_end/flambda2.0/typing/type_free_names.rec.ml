(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let free_names_or_alias free_names_contents
      (or_alias : _ Flambda_types.or_alias) : Name_occurrences.t =
  match or_alias with
  | No_alias contents -> free_names_contents contents
  | Type _export_id -> Name_occurrences.empty
  | Equals simple -> Simple.free_names_in_types simple

let free_names_unknown_or_join free_names_contents
      (o : _ Flambda_types.unknown_or_join) : Name_occurrences.t =
  match o with
  | Unknown | Bottom -> Name_occurrences.empty
  | Ok contents -> free_names_contents contents

let free_names_ty free_names_contents ty : Name_occurrences.t =
  free_names_or_alias (free_names_unknown_or_join free_names_contents) ty

let free_names_of_kind_naked_number (type n)
      (_ty : n Flambda_types.of_kind_naked_number) =
  Name_occurrences.empty

let rec free_names (t : Flambda_types.t) =
  match t with
  | Value ty -> free_names_ty free_names_of_kind_value ty
  | Naked_number (ty, _kind) ->
    free_names_ty free_names_of_kind_naked_number ty
  | Fabricated ty -> free_names_ty free_names_of_kind_fabricated ty

and free_names_of_kind_value (of_kind : Flambda_types.of_kind_value)
      : Name_occurrences.t =
  match of_kind with
  | Blocks_and_tagged_immediates { blocks; immediates; } ->
    Name_occurrences.union
      (Or_unknown.free_names Blocks.free_names blocks)
      (Or_unknown.free_names Immediates.free_names immediates)
  | Boxed_number (Boxed_float n) ->
    free_names_ty free_names_of_kind_naked_number n
  | Boxed_number (Boxed_int32 n) ->
    free_names_ty free_names_of_kind_naked_number n
  | Boxed_number (Boxed_int64 n) ->
    free_names_ty free_names_of_kind_naked_number n
  | Boxed_number (Boxed_nativeint n) ->
    free_names_ty free_names_of_kind_naked_number n
  | Closures { by_closure_id; } ->
    Closures_entry_by_set_of_closures_contents.free_names by_closure_id
  | String _ -> Name_occurrences.empty
  | Array { length; } ->
    free_names_ty free_names_of_kind_value length

and free_names_of_kind_fabricated
      (of_kind : Flambda_types.of_kind_fabricated) =
  match of_kind with
  | Discriminants discrs -> Discriminants.free_names discrs

let free_names_of_ty_fabricated ty =
  free_names_ty free_names_of_kind_fabricated ty
