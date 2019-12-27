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

  let rec print0 ppf t =
    match t with
    | Empty -> Format.fprintf ppf "Empty"
    | Leaf_branch { n1; n2; older = Empty; } ->
      Format.fprintf ppf "@[(%a %a)@]" N.print n1 N.print n2
    | Leaf_branch { n1; n2; older; } ->
      Format.fprintf ppf "@[(%a %a)@]@ %a" N.print n1 N.print n2 print0 older
    | Branch { newer = Empty; older; } -> print0 ppf older
    | Branch { newer; older = Empty; } -> print0 ppf newer
    | Branch { newer; older; } ->
      Format.fprintf ppf "%a@ %a" print0 newer print0 older

  let print ppf t = Format.fprintf ppf "@[<hov 2>%a@]" print0 t

  let [@inline always] invariant _ = ()

  let apply t n =
    let rec apply t n =
      match t with
      | Empty -> n
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
    Branch {
      newer = second;
      older = first;
    }
end
