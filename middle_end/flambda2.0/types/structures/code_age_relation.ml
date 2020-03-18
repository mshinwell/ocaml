(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(* If a value of type [t] maps [id1] to [id2], it means that [id1] is a
   newer version of [id2].  The relation forms a partial order.
   These relations are expected to be small in the majority of cases. *)
type t = Code_id.t Code_id.Map.t

let print ppf t = Code_id.Map.print Code_id.print ppf t

let empty = Code_id.Map.empty

(* CR mshinwell: There should be a well-formedness check during [add], otherwise
   the functions below may not work correctly. *)

let add t ~newer ~older = Code_id.Map.add newer older t

let rec all_ids_up_to_root t id =
  match Code_id.Map.find id t with
  | exception Not_found -> Code_id.Set.empty
  | older -> Code_id.Set.add older (all_ids_up_to_root t older)

let num_ids_up_to_root t id =
  Code_id.Set.cardinal (all_ids_up_to_root t id)

(* CR mshinwell: There are no doubt better implementations than the below. *)

let meet t id1 id2 : _ Or_bottom.t =
  (* Whichever of [id1] and [id2] is newer (or the same as the other one),
     in the case where they are comparable; otherwise bottom. *)
  if Code_id.equal id1 id2 then Ok id1
  else
    let id1_to_root = all_ids_up_to_root t id1 in
    let id2_to_root = all_ids_up_to_root t id2 in
    if Code_id.Set.mem id1 id2_to_root then Ok id2
    else if Code_id.Set.mem id2 id1_to_root then Ok id1
    else Bottom

let join t id1 id2 : _ Or_unknown.t =
  (* Lowest ("newest") common ancestor, if such exists. *)
  if Code_id.equal id1 id2 then Known id1
  else
    let id1_to_root = all_ids_up_to_root t id1 in
    let id2_to_root = all_ids_up_to_root t id2 in
    let shared_ids = Code_id.Set.inter id1_to_root id2_to_root in
    if Code_id.Set.is_empty shared_ids then Unknown
    else
      let newest_shared_id, _ =
        shared_ids
        |> Code_id.Set.elements
        |> List.map (fun id -> id, num_ids_up_to_root t id)
        |> List.sort (fun (_, len1) (_, len2) -> - (Int.compare len1 len2))
        |> List.hd
      in
      Known newest_shared_id

type at_most_one_newer =
  | No_newer_version
  | Exactly_one_newer_version of Code_id.t
  | More_than_one_newer_version

let has_at_most_one_newer_version t id =
  let newer_to_id =
    Code_id.Map.filter (fun _newer older -> Code_id.equal older id) t
  in
  if Code_id.Map.is_empty newer_to_id then No_newer_version
  else
    match Code_id.Map.get_singleton newer_to_id with
    | Some (newer, id') ->
      assert (Code_id.equal id id');
      Exactly_one_newer_version newer
    | None -> More_than_one_newer_version

let rec newer_versions_form_linear_chain t id ~all_code_ids_still_existing =
  if not (Code_id.Set.mem id all_code_ids_still_existing) then true
  else
    match has_at_most_one_newer_version t id with
    | No_newer_version -> true
    | Exactly_one_newer_version id ->
      newer_versions_form_linear_chain t id ~all_code_ids_still_existing
    | More_than_one_newer_version -> false
