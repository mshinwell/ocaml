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
module T = Type_grammar
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
      (Index.Map.print Type_grammar.print) components_by_index

  let print_with_cache ~cache:_ ppf t = print ppf t

  let create components_by_index =
    { components_by_index;
    }

  (* XXX This isn't right -- it seems the 0-arity one isn't bottom.  This
     presumably needs to take a kind? *)
  let create_bottom () =
    { components_by_index = Index.Map.empty;
    }

  let indexes t = Index.Map.keys t.components_by_index

  let width t =
    Targetint.OCaml.of_int (Index.Map.cardinal t.components_by_index)

  let components t = Index.Map.data t.components_by_index

  let meet env
        { components_by_index = components_by_index1; }
        { components_by_index = components_by_index2; } : _ Or_bottom.t =
    let all_bottom = ref true in
    let env_extension = ref (TEE.empty ()) in
    let components_by_index =
      Index.Map.merge (fun _index ty1_opt ty2_opt ->
          match ty1_opt, ty2_opt with
          | None, None | Some _, None | None, Some _ -> None
          | Some ty1, Some ty2 ->
            let ty, env_extension' = Type_grammar.meet' env ty1 ty2 in
            if Type_grammar.is_obviously_bottom ty then begin
              Some (Type_grammar.bottom K.value)
            end else begin
              all_bottom := false;
              (*
Format.eprintf "Product TEE meet:@ TEE1: %a@ TEE2: %a\n%!"
  TEE.print !env_extension
  TEE.print env_extension';
  *)
              (* XXX bad performance! *)
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
          | Some ty1, Some ty2 -> Some (Type_grammar.join env ty1 ty2))
        components_by_index1
        components_by_index2
    in
    { components_by_index; }

  let widen t ~to_match =
    let missing_indexes = Index.Set.diff (indexes to_match) (indexes t) in
    let components_by_index =
      Index.Set.fold (fun index components_by_index ->
          Index.Map.add index (Type_grammar.any_value ())
            components_by_index)
        missing_indexes
        t.components_by_index
    in
    { components_by_index; }

  let apply_name_permutation ({ components_by_index; } as t) perm =
    let components_by_index' =
      Index.Map.map_sharing (fun typ ->
          Type_grammar.apply_name_permutation typ perm)
        components_by_index
    in
    if components_by_index == components_by_index' then t
    else { components_by_index = components_by_index'; }

  let free_names { components_by_index; } =
    Index.Map.fold (fun _index ty free_names ->
        Name_occurrences.union (Type_grammar.free_names ty) free_names)
      components_by_index
      Name_occurrences.empty

  let map_types ({ components_by_index; } as t)
        ~(f : Type_grammar.t -> Type_grammar.t Or_bottom.t)
        : _ Or_bottom.t =
    let found_bottom = ref false in
    let components_by_index' =
      Index.Map.map_sharing (fun ty ->
          match f ty with
          | Bottom ->
            found_bottom := true;
            ty
          | Ok ty -> ty)
        components_by_index
    in
    if !found_bottom then Bottom
    else if components_by_index == components_by_index' then Ok t
    else Ok { components_by_index = components_by_index'; }
end

module Int_indexed = Make (Numbers.Int)

module Closure_id_indexed = Make (Closure_id)

module Var_within_closure_indexed = Make (Var_within_closure)
