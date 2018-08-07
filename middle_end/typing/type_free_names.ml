(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Flambda_types = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  let free_names_or_alias free_names_contents
        (or_alias : _ Flambda_types.or_alias) : Name_occurrences.t =
    match or_alias with
    | No_alias contents -> free_names_contents contents
    | Type _export_id -> Name_occurrences.create ()
    | Equals simple ->
      Name_occurrences.create_from_set_in_types (Simple.free_names simple)

  let free_names_unknown_or_join free_names_contents
        (o : _ Flambda_types.unknown_or_join) : Name_occurrences.t =
    match o with
    | Unknown -> Name_occurrences.create ()
    | Join contents_list ->
      List.fold_left (fun free_names (contents, perm) ->
          let names =
            Name_occurrences.apply_name_permutation
              (free_names_contents contents)
              perm
          in
          Name_occurrences.union names free_names)
        (Name_occurrences.create ())
        contents_list

  let free_names_ty free_names_contents ty : Name_occurrences.t =
    free_names_or_alias (free_names_unknown_or_join free_names_contents) ty

  let free_names_of_kind_naked_number (type n)
        (_ty : n Flambda_types.of_kind_naked_number) =
    Name_occurrences.create ()

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
      Name_occurrences.union (Blocks.free_names blocks)
        (Immediates.free_names immediates)
    | Boxed_number (Boxed_float n) ->
      free_names_ty free_names_of_kind_naked_number n
    | Boxed_number (Boxed_int32 n) ->
      free_names_ty free_names_of_kind_naked_number n
    | Boxed_number (Boxed_int64 n) ->
      free_names_ty free_names_of_kind_naked_number n
    | Boxed_number (Boxed_nativeint n) ->
      free_names_ty free_names_of_kind_naked_number n
    | Closures { by_closure_id; } ->
      Closures_entry_by_closure_id.free_names by_closure_id
    | String _ -> Name_occurrences.create ()

  and free_names_of_kind_fabricated
        (of_kind : Flambda_types.of_kind_fabricated) =
    match of_kind with
    | Discriminants discrs -> Discriminants.free_names discrs
    | Set_of_closures { closures; } -> Closure_ids.free_names closures

  let free_names_of_ty_fabricated ty =
    free_names_ty free_names_of_kind_fabricated ty
end
