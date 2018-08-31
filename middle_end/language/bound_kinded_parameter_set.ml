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

type t = Kinded_parameter.Set.t

let free_names t =
  Kinded_parameter.Set.fold (fun param free_names ->
      Name_occurrences.add free_names In_terms
        (Kinded_parameter.name param))
    t
    (Name_occurrences.create ())

let apply_name_permutation t perm =
  let changed = ref false in
  let result =
    Kinded_parameter.Set.fold (fun param result ->
        let param' = Kinded_parameter.apply_name_permutation param perm in
        if not (param == param') then begin
          changed := true;
        end;
        Kinded_parameter.Set.add param' result)
      t
      Kinded_parameter.Set.empty
  in
  if not !changed then t
  else result
