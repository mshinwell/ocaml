(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Logical_variable

let free_names t =
  Name_occurrences.create_from_set_in_types (
    Name.Set.singleton (Name.logical_var t))

let bound_names _t = Name_occurrences.create ()

let apply_name_permutation t perm =
  Name_permutation.apply_logical_variable perm t

let freshen t freshening =
  apply_name_permutation t (Freshening.name_permutation freshening)

let name t = Name.logical_var t
