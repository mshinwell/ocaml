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

let check_invariants = false

module Make (N : Identifiable.S) = struct
  type backwards =
    | Not_computed
    | Ok of N.t N.Map.t

  let print_not_computed ppf nc =
    match nc with
    | Not_computed -> Format.fprintf ppf "Not_computed"
    | Ok map -> N.Map.print N.print ppf map

  type t = {
    forwards : N.t N.Map.t;
    backwards : backwards;
  }

  let create () =
    { forwards = N.Map.empty;
      backwards = Not_computed;
    }

  let backwards t =
    match t.backwards with
    | Not_computed -> t.forwards
    | Ok backwards -> backwards

  let print ppf { forwards; backwards; } =
    Format.fprintf ppf "@[((forwards %a)@ (backwards %a))@]"
      (N.Map.print N.print) forwards
      print_not_computed backwards

  let [@inline always] invariant ({ forwards; backwards = _; } as t) =
    if check_invariants then begin
      let is_bijection map =
        let domain = N.Map.keys map in
        let range_list = N.Map.data map in
        let range = N.Set.of_list range_list in
        N.Set.equal domain range
      in
      assert (is_bijection forwards);
      assert (N.Map.cardinal forwards = N.Map.cardinal (backwards t));
      assert (N.Map.for_all (fun n1 n2 ->
          assert (N.compare n1 n2 <> 0);
          match N.Map.find n2 (backwards t) with
          | exception Not_found -> false
          | n1' -> N.compare n1 n1' = 0)
        forwards)
    end

  let mem t n =
    N.Map.mem n t.forwards
      || match t.backwards with
         | Not_computed -> false
         | Ok backwards -> N.Map.mem n backwards
(*
  let domain t =
    let forwards = N.Map.keys t.forwards in
    match t.backwards with
    | Not_computed -> forwards
    | Ok backwards -> N.Set.union forwards (N.Map.keys backwards)
*)

  let apply t n =
    match N.Map.find n t.forwards with
    | exception Not_found -> n
    | n -> n

  let apply_backwards t n =
    match N.Map.find n (backwards t) with
    | exception Not_found -> n
    | n -> n

  let add_to_map n1 n2 map =
    if N.equal n1 n2 then N.Map.remove n1 map
    else N.Map.add n1 n2 map

  let flip t =
    { forwards = backwards t;
      backwards = Ok t.forwards;
    }

  let [@inline always] post_swap t n1 n2 =
    let n1' = apply_backwards t n1 in
    let n2' = apply_backwards t n2 in
    (* CR mshinwell: These next two lines are a major source of allocation *)
    let forwards = add_to_map n1' n2 (add_to_map n2' n1 t.forwards) in
    let backwards : backwards =
      Ok (add_to_map n2 n1' (add_to_map n1 n2' (backwards t)))
    in
    let t = { forwards; backwards; } in
    invariant t;
    t

  let pre_swap t n1 n2 =
    flip (post_swap (flip t) n1 n2)

  let is_empty t =
    N.Map.is_empty t.forwards

  let compose0 ~second ~first =
    let rec compose ~second ~first =
      match N.Map.choose second.forwards with
      | exception Not_found -> first
      | n1, n2 ->
        let first = post_swap first n1 n2 in
        let second = pre_swap second n1 n2 in
        compose ~second ~first
    in
    let t = compose ~second ~first in
    t

  let compose_one ~first n1 n2 =
    if mem first n1 || mem first n2 then post_swap first n1 n2
    else
      let forwards = N.Map.add n1 n2 (N.Map.add n2 n1 first.forwards) in
      let backwards : backwards =
        match first.backwards with
        | Not_computed -> Not_computed
        | Ok backwards -> Ok (N.Map.add n1 n2 (N.Map.add n2 n1 backwards))
      in
      { forwards;
        backwards;
      }

  let compose ~second ~first =
(*
Format.eprintf "second %a first %a\n%!" print second print first;
*)
    if is_empty second then first
    else if is_empty first then second
    else
      match N.Map.disjoint_union second.forwards first.forwards with
      | exception (Invalid_argument _) -> compose0 ~second ~first
      | forwards ->
        let backwards : backwards =
          match second.backwards, first.backwards with
          | Not_computed, Not_computed -> Not_computed
          | Not_computed, Ok backwards_first ->
            Ok (N.Map.disjoint_union (backwards second) backwards_first)
          | Ok backwards_second, Not_computed ->
            Ok (N.Map.disjoint_union (backwards first) backwards_second)
          | Ok backwards_first, Ok backwards_second ->
            Ok (N.Map.disjoint_union backwards_first backwards_second)
        in
        { forwards;
          backwards;
        }
end
