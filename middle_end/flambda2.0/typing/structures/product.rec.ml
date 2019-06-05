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
module TEE = Typing_env_extension

module Make (Index : Identifiable.S) = struct
  type t = {
    components_by_index : T.t Index.Map.t;
  }

  let _invariant t =
    Index.Map.cardinal t.components_by_index > 0

  let print ppf { components_by_index; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(components_by_index@ %a)@]\
        )@]"
      (Index.Map.print Type_printers.print) components_by_index

  let print_with_cache ~cache:_ ppf t = print ppf t

  let equal env
        { components_by_index = components_by_index1; }
        { components_by_index = components_by_index2; } =
    let equal =
      Index.Map.fold2_stop_on_key_mismatch
        (fun _index component1 component2 equal ->
          equal &&
            (* CR mshinwell: Why is this [bound_name] needed? *)
            Type_equality.equal_with_env ?bound_name:None env
              component1 component2)
        components_by_index1 components_by_index2
        true
    in
    match equal with
    | None | Some false -> false
    | Some true -> true

  let create components_by_index =
    { components_by_index;
    }

  (* XXX This isn't right -- it seems the 0-arity one isn't bottom.  This
     presumably needs to take a kind? *)
  let create_bottom () =
    { components_by_index = Index.Map.empty;
    }

  let indexes t = Index.Map.keys t.components_by_index

  let meet env
        { components_by_index = components_by_index1; }
        { components_by_index = components_by_index2; } : _ Or_bottom.t =
    let all_bottom = ref true in
    let env_extension = ref TEE.empty in
    let components_by_index =
      Index.Map.merge (fun _index ty1_opt ty2_opt ->
          match ty1_opt, ty2_opt with
          | None, None | Some _, None | None, Some _ -> None
          | Some ty1, Some ty2 ->
            let ty, env_extension' = Api_meet_and_join.meet env ty1 ty2 in
            if Flambda_type0_core.is_obviously_bottom ty then begin
              Some (Flambda_type0_core.bottom K.value)
            end else begin
              all_bottom := false;
              env_extension := TEE.meet env !env_extension env_extension';
              Some ty
            end)
        components_by_index1
        components_by_index2
    in
    if !all_bottom && Index.Map.cardinal components_by_index > 0 then Bottom
    else Ok ({ components_by_index; }, !env_extension)

  let join env
        { components_by_index = components_by_index1; }
        { components_by_index = components_by_index2; } =
    let components_by_index =
      Index.Map.merge (fun _index ty1_opt ty2_opt ->
          match ty1_opt, ty2_opt with
          | None, None -> None
          | Some ty, None | None, Some ty -> Some ty
          | Some ty1, Some ty2 -> Some (Api_meet_and_join.join env ty1 ty2))
        components_by_index1
        components_by_index2
    in
    { components_by_index; }

  let widen t ~to_match =
    let missing_indexes = Index.Set.diff (indexes to_match) (indexes t) in
    let components_by_index =
      Index.Set.fold (fun index components_by_index ->
          Index.Map.add index (Flambda_type0_core.any_value ())
            components_by_index)
        missing_indexes
        t.components_by_index
    in
    { components_by_index; }

  let erase_aliases { components_by_index; } ~allowed =
    let components_by_index =
      Index.Map.map (fun typ -> Type_erase_aliases.erase_aliases typ ~allowed)
        components_by_index
    in
    { components_by_index; }

  let free_names { components_by_index; } =
    Index.Map.fold (fun _index ty free_names ->
        Name_occurrences.union (Type_free_names.free_names ty) free_names)
      components_by_index
      Name_occurrences.empty
end
