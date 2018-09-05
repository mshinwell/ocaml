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

include Kinded_parameter.List

let permutation_to_swap t1 t2 =
  if List.compare_lengths t1 t2 <> 0 then begin
    Misc.fatal_error "Mismatched cardinality in binding position (1)"
  end else begin
    let free_names param =
      Bindable_name.Set.elements (Name_occurrences.everything (
        Kinded_parameter.free_names param))
    in
    List.fold_left2 (fun perm param1 param2 ->
        let free_names1 = free_names param1 in
        let free_names2 = free_names param2 in
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
      t1 t2
  end
