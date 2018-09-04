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

(* CR mshinwell: Generate this and [Bound_kinded_nameeter_set] from
   a functor. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = Name.Set.t

let free_names t =
  Name.Set.fold (fun name free_names ->
      Name_occurrences.add free_names (Name name) In_terms)
    t
    (Name_occurrences.create ())

let apply_name_permutation t perm =
  let changed = ref false in
  let result =
    Name.Set.fold (fun name result ->
        let name' = Name_permutation.apply_name perm name in
        if not (name == name') then begin
          changed := true;
        end;
        Name.Set.add name' result)
      t
      Name.Set.empty
  in
  if not !changed then t
  else result
