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

module Make (N : Name_like_intf.S) = struct
  include N.Set

  let rename t =
    N.Set.fold (fun component result ->
        N.Set.add (N.rename component) result)
      t
      N.Set.empty

  let free_names t =
    N.Set.fold (fun component result ->
        Name_occurrences.union (N.free_names component)
          result)
      t
      (Name_occurrences.create ())

  let apply_name_permutation t perm =
    N.Set.map (fun component ->
        N.apply_name_permutation component perm)
      t

  let permutation_to_swap t1 t2 =
    if N.Set.cardinal t1 <> N.Set.cardinal t2
    then begin
      Misc.fatal_error "Mismatched cardinality in binding position (1)"
    end else begin
      let components1 = N.Set.elements t1 in
      let components2 = N.Set.elements t2 in
      let free_names component =
        Bindable_name.Set.elements (Name_occurrences.everything (
          N.free_names component))
      in
      List.fold_left2 (fun perm component1 component2 ->
          let free_names1 = free_names component1 in
          let free_names2 = free_names component2 in
          if List.compare_lengths free_names1 free_names2 <> 0
          then begin
            Misc.fatal_error "Mismatched cardinality in binding \
              position (2)"
          end else begin
            List.fold_left2
              (fun perm (free_name1 : Bindable_name.t)
                   (free_name2 : Bindable_name.t) ->
                match free_name1, free_name2 with
                | Name free_name1, Name free_name2 ->
                  Name_permutation.add_name perm free_name1 free_name2
                | _, _ ->
                  Misc.fatal_error "Only expected [Name]s")
              perm
              free_names1 free_names2
          end)
        (Name_permutation.create ())
        components1 components2
    end
end
