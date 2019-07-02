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

module Unit_and_closure_id_set = struct
  type t = unit * Closure_id.Set.t
  include Identifiable.Make_pair (Unit) (Closure_id_set)
end

include Row_like.Make (Unit) (Closure_id_set) (Unit_and_closure_id_set)
  (Set_of_closures_entry)

type open_or_closed = Open | Closed

let create closure_ids_map open_or_closed : t =
  match open_or_closed with
  | Open -> create_at_least_multiple closure_ids_map
  | Closed ->
    let closure_ids_map =
      Closure_id_set.Map.fold
        (fun closure_ids set_of_closures_entry result ->
          Unit_and_closure_id_set.Map.add ((), closure_ids)
            set_of_closures_entry result)
        closure_ids_map
        Unit_and_closure_id_set.Map.empty
    in
    create_exactly_multiple closure_ids_map

let map_closure_types t ~f =
  map_maps_to t ~f:(fun closures_entry ->
    Set_of_closures_entry.map_closure_types closures_entry ~f)
