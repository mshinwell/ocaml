(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let _check_invariants = false

module Make (N : Identifiable.S) = struct
  type t =
    | Empty
    | Leaf_branch of { n1 : N.t; n2 : N.t; older : t; }
    | Branch of { newer : t; older : t; }

  let empty = Empty

(*
  let to_map t =
    let rec collect t map =
      match t with
      | [] -> map
      | n1::n2::t ->
        let map = N.Map.add n1 n2 map in
        collect t map
      | _ -> assert false
    in
    collect t N.Map.empty

  let print ppf t = N.Map.print N.print ppf (to_map t)
*)

  let print _ _ = Misc.fatal_error "To implement"

  let [@inline always] invariant _ = ()

  let apply t n =
    let rec apply t n =
      match t with
      | Empty -> n
      | Leaf_branch { n1; n2; older = Empty; } ->
        if N.equal n n1 then n2
        else if N.equal n n2 then n1
        else n
      | Leaf_branch { n1; n2; older; } ->
        let n = apply older n in
        if N.equal n n1 then n2
        else if N.equal n n2 then n1
        else n
      | Branch { newer; older; } -> apply newer (apply older n)
    in
    apply t n

  let is_empty t =
    match t with
    | Empty -> true
    | Leaf_branch _ | Branch _ -> false

  let compose_one ~first n1 n2 =
    Leaf_branch {
      n1;
      n2;
      older = first;
    }

  let compose ~second ~first =
    if is_empty second then first
    else if is_empty first then second
    else
      Branch {
        newer = second;
        older = first;
      }
end
