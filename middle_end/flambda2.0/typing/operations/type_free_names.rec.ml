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
      (or_alias : _ Type_grammar.or_alias) : Name_occurrences.t =
  match or_alias with
  | No_alias contents -> free_names_contents contents
  | Type _export_id -> Name_occurrences.empty
  | Equals simple -> Simple.free_names_in_types simple

let free_names_unknown_or_join free_names_contents
      (o : _ Type_grammar.unknown_or_join) : Name_occurrences.t =
  match o with
  | Unknown | Bottom -> Name_occurrences.empty
  | Ok contents -> free_names_contents contents

let free_names_ty free_names_contents ty : Name_occurrences.t =
  free_names_or_alias (free_names_unknown_or_join free_names_contents) ty

let free_names_of_kind_naked_number (type n)
      (_ty : n Type_grammar.of_kind_naked_number) =
  Name_occurrences.empty



and free_names_of_kind_fabricated
      (of_kind : Type_grammar.of_kind_fabricated) =
  match of_kind with
  | Discriminants discrs -> Discriminants.free_names discrs
