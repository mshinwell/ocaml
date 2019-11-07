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
  type t = N.t -> N.t

  let empty = fun n -> n

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

  let apply t n = t n

  (* This is wrong, but does it matter? *)
  let is_empty _ = false

  let compose_one ~first n1 n2 = fun n ->
    if N.equal n n1 then n2
    else if N.equal n n2 then n1
    else first n

  let compose ~second ~first = fun n -> second (first n)

end
