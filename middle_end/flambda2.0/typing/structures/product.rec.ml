(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind
module T = Flambda_types

module Make (Index : Name_like_intf.S) = struct
  type t = {
    components_by_index : T.t Index.Map.t;
  }

  let invariant t =
    Index.Map.cardinal t.components_by_index > 0

  let print ppf { components_by_index; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(components_by_index@ %a)@]\
        )@]"
      (Index.Map.print Type_printers.print) components_by_index

  let print_with_cache ~cache:_ ppf t = print ppf t

  let equal env result
        { components_by_index = components_by_index1; }
        { components_by_index = components_by_index2; } =
    let equal =
      Index.Map.fold2_stop_on_key_mismatch
        (fun _index component1 component2 equal ->
          equal && Type_equality.equal env env component1 component2)
        components_by_index1 components_by_index2
        true
    in
    match equal with
    | None | Some false -> false
    | Some true -> true

  let indexes t = Index.Map.keys t.components_by_index

  let meet env
        { components_by_index = components_by_index1; }
        { components_by_index = components_by_index2; } : _ Or_bottom.t =
    let all_bottom = ref true in
    let components_by_index =
      Index.Map.merge (fun ty1_opt ty2_opt ->
          match ty1_opt, ty2_opt with
          | None, None | Some _, None | None, Some _ -> None
          | Some ty1, Some ty2 ->
            match T.meet env ty1 ty2 with
            | Ok ty ->
              all_bottom := false;
              Some ty
            | Bottom -> Some (T.bottom K.value))
        components_by_index1
        components_by_index2
    in
    if !all_bottom then Bottom
    else Ok { components_by_index; }

  let join env
        { components_by_index = components_by_index1; }
        { components_by_index = components_by_index2; } =
    let components_by_index =
      Index.Map.merge (fun ty1_opt ty2_opt ->
          match ty1_opt, ty2_opt with
          | None, None -> None
          | Some ty, None | None, Some ty -> Some ty
          | Some ty1, Some ty2 -> Some (T.join env ty1 ty2))
        components_by_index1
        components_by_index2
    in
    { components_by_index; }

  let widen t ~to_match =
    let missing_indexes = Index.Set.diff (indexes to_match) (indexes t) in
    let components_by_index =
      Index.Set.fold (fun index components_by_index ->
          Index.Map.add index (T.any_value ()) components_by_index)
        missing_indexes
        t.components_by_index
    in
    { t with components_by_index; }
end
