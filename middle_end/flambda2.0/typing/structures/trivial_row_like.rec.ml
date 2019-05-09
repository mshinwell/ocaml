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

module Make (Thing_without_names : Identifiable.S) = struct
  module Thing_without_names_and_unit =
    Hashtbl.Make_with_map_pair (Thing_without_names) (Unit)

  module RL =
    Row_like.Make (Thing_without_names) (Unit)
      (Thing_without_names_and_unit) (Unit)

  type t = RL.t

  let create things =
    let things =
      Thing_without_names.Set.fold (fun thing result ->
          Thing_without_names_and_unit.Map.add (thing, ()) () result)
        things_with_env_extensions
        Thing_without_names_and_unit.Map.empty
    in
    RL.create_exactly_multiple things

  let create_bottom = RL.create_bottom
  let create_unknown = RL.create_unknown

  let print = RL.print
  let equal = RL.equal

  let meet = RL.meet
  let join = RL.join

  let free_names _ = Name_occurrences.empty
  let apply_name_permutation t _perm = t

  let all t : _ Or_unknown.t =
    match RL.at_least t, RL.known t with
    | Unknown, _ | _, Unknown -> Unknown
    | Known indexes, Known known ->
      if not (Unit.Map.is_empty indexes) then Unknown
      else
        let things =
          Thing_without_names_and_unit.Set.fold (fun (thing, ()) things ->
              Thing_without_names.Set.add thing things)
            (Thing_without_names_and_unit.Map.keys known)
            Thing_without_names.Set.empty
        in
        Known things

  let classify = RL.classify

  let get_singleton t =
    match RL.get_singleton t with
    | None -> None
    | Some ((thing, ()), ()) -> Some (thing, ())
end
