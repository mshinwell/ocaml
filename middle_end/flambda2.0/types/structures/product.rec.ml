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

  (* Product are a set of constraints: each new field reduces the
     concrete set. The empty product is Top. There is no bottom. All
     components must be of the same kind.

     { 1 => Unknown; 2 => V } is equal to { 2 => V } *)
  type t = {
    components_by_index : T.t Index.Map.t;
    kind : Flambda_kind.t;
  }

  let print ppf { components_by_index; kind = _ } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(components_by_index@ %a)@]\
        )@]"
      (Index.Map.print Type_grammar.print) components_by_index

  let print_with_cache ~cache:_ ppf t = print ppf t

  let create kind components_by_index =
    (* CR mshinwell: Check that the types are all of the same kind *)
    { components_by_index;
      kind;
    }

  let create_bottom _ = assert false

  (* CR mshinwell: This "bottom" stuff is still dubious.
     We can't treat 0-sized blocks as bottom; it's legal to bind one of
     those (e.g. an empty module). *)

  let is_bottom t =
    Index.Map.exists (fun _ typ -> Type_grammar.is_obviously_bottom typ)
      t.components_by_index

  let width t =
    Targetint.OCaml.of_int (Index.Map.cardinal t.components_by_index)

  let components t = Index.Map.data t.components_by_index

  let meet env
        { components_by_index = components_by_index1; kind = kind1; }
        { components_by_index = components_by_index2; kind = kind2; }
    : _ Or_bottom.t =
    if not (Flambda_kind.equal kind1 kind2) then
      Misc.fatal_errorf "Product.meet between unmatching kinds %a and %a@."
        Flambda_kind.print kind1 Flambda_kind.print kind2;
    let any_bottom = ref false in
    let env_extension = ref (TEE.empty ()) in
    let components_by_index =
      Index.Map.union (fun _index ty1 ty2 ->
          let ty, env_extension' = Type_grammar.meet' env ty1 ty2 in
          env_extension := TEE.meet env !env_extension env_extension';
          if (Type_grammar.is_obviously_bottom ty) then begin
            any_bottom := true
          end;
          Some ty)
        components_by_index1
        components_by_index2
    in
    if !any_bottom then Bottom
    else Ok ({ components_by_index; kind = kind1; }, !env_extension)

  let join env
        { components_by_index = components_by_index1; kind = kind1; }
        { components_by_index = components_by_index2; kind = kind2; } =
    if not (Flambda_kind.equal kind1 kind2) then
      Misc.fatal_errorf "Product.join between unmatching kinds %a and %a@."
        Flambda_kind.print kind1 Flambda_kind.print kind2;
    let components_by_index =
      Index.Map.inter (fun _index ty1 ty2 ->
          Some (Type_grammar.join' env ty1 ty2))
        components_by_index1
        components_by_index2
    in
    { components_by_index; kind = kind1; }

  let apply_name_permutation ({ components_by_index; kind; } as t) perm =
    let components_by_index' =
      Index.Map.map_sharing (fun typ ->
          Type_grammar.apply_name_permutation typ perm)
        components_by_index
    in
    if components_by_index == components_by_index' then t
    else { components_by_index = components_by_index'; kind = kind; }

  let free_names { components_by_index; kind = _; } =
    Index.Map.fold (fun _index ty free_names ->
        Name_occurrences.union (Type_grammar.free_names ty) free_names)
      components_by_index
      Name_occurrences.empty

  let map_types ({ components_by_index; kind } as t)
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
    else Ok { components_by_index = components_by_index'; kind; }

  let to_map t = t.components_by_index
end

module Closure_id_indexed = Make (Closure_id)

module Var_within_closure_indexed = Make (Var_within_closure)

module Int_indexed = struct
  (* CR mshinwell: Add [Or_bottom].  However what should [width] return for
     [Bottom]?  Maybe we can circumvent that question if removing [Row_like].
  *)
  type t = T.t array

  let print ppf t =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
      (Array.to_list t)

  let print_with_cache ~cache:_ ppf t = print ppf t

  let create _ = Misc.fatal_error "Not implemented"

  let create_from_list _kind tys = Array.of_list tys

  let create_empty _kind = [| |]

  let create_bottom () = [| (Type_grammar.bottom Flambda_kind.value) |]

  let is_bottom t =
    Array.exists Type_grammar.is_obviously_bottom t

  let width t = Targetint.OCaml.of_int (Array.length t)

  let components t = Array.to_list t

  let meet env t1 t2 : _ Or_bottom.t =
    let all_bottom = ref true in
    let env_extension = ref (TEE.empty ()) in
    let t =
      Array.init (Array.length t1) (fun index ->
        let ty, env_extension' = Type_grammar.meet' env t1.(index) t2.(index) in
        env_extension := TEE.meet env !env_extension env_extension';
        if not (Type_grammar.is_obviously_bottom ty) then begin
          all_bottom := false
        end;
        ty)
    in
    if !all_bottom && Array.length t > 0 then Bottom
    else Ok (t, !env_extension)

  let join env t1 t2 =
    Array.init (Array.length t1) (fun index ->
      Type_grammar.join' env t1.(index) t2.(index))

  let widen t ~to_match =
    let t_len = Array.length t in
    let to_match_len = Array.length to_match in
    if t_len >= to_match_len then t
    else
      let kind = Type_grammar.kind to_match.(0) in
      Array.init to_match_len (fun index ->
        if index < t_len then t.(index)
        else Type_grammar.unknown kind)

  let apply_name_permutation t perm =
    let t = Array.copy t in
    for i = 0 to Array.length t - 1 do
      t.(i) <- Type_grammar.apply_name_permutation t.(i) perm
    done;
    t

  let free_names t =
    Array.fold_left (fun free_names ty ->
        Name_occurrences.union (Type_grammar.free_names ty) free_names)
      Name_occurrences.empty
      t

  let map_types t ~(f : Type_grammar.t -> Type_grammar.t Or_bottom.t)
        : _ Or_bottom.t =
    let found_bottom = ref false in
    let t = Array.copy t in
    for i = 0 to Array.length t - 1 do
      match f t.(i) with
      | Bottom -> found_bottom := true
      | Ok typ -> t.(i) <- typ
    done;
    if !found_bottom then Bottom
    else Ok t

  let to_map _ = Misc.fatal_error "Not implemented"
end
