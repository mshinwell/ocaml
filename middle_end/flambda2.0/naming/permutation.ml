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
    | Freshening of {
        forwards : N.t N.Map.t;
        everything_involved : N.Set.t;
    }
    | Leaf_branch of { n1 : N.t; n2 : N.t; older : t; }
    | Branch of { newer : t; older : t; }

  let empty = Empty

  let squash_freshening t =
    match t with
    | Empty | Leaf_branch _ | Branch _ -> t
    | Freshening { forwards; _ } ->
      N.Map.fold (fun n1 n2 older ->
          Leaf_branch {
            n1;
            n2;
            older;
          })
        forwards empty

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
    match t with
    | Empty -> n
    | Freshening { forwards; _; } ->
      begin match N.Map.find n forwards with
      | exception Not_found -> n
      | n -> n
      end
    | Leaf_branch _ | Branch _ ->
      let rec apply t n =
        match t with
        | Empty -> n
        | Freshening _ -> assert false
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
    | Freshening _ | Leaf_branch _ | Branch _ -> false

  let compose_one ~first n1 n2 =
    Leaf_branch {
      n1;
      n2;
      older = squash_freshening first;
    }

  let compose_one_fresh existing n1 ~fresh:n2 =
    match existing with
    | Leaf_branch _ | Branch _ -> compose_one ~first:existing n1 n2
    | Empty ->
      Freshening {
        forwards = N.Map.singleton n1 n2;
        everything_involved = N.Set.add n1 (N.Set.singleton n2);
      }
    | Freshening { forwards; everything_involved; } ->
      (* CR mshinwell: For invariant checks, ensure [n1] and [n2] are neither
         in [forwards] or inverse. *)
      let forwards = N.Map.add n1 n2 forwards in
      let everything_involved =
        N.Set.add n1 (N.Set.add n2 everything_involved)
      in
      Freshening { forwards; everything_involved; }

  let compose ~second ~first =
    if is_empty second then first
    else if is_empty first then second
    else
      match second, first with
      | Freshening {
          forwards = forwards2;
          everything_involved = everything_involved2;
        },
        Freshening {
          forwards = forwards1;
          everything_involved = everything_involved1;
        } ->
        let still_fresh =
          N.Set.intersection_is_empty everything_involved1 everything_involved2
        in
        if still_fresh then
          let forwards =
            N.Map.union (fun _ _ _ -> assert false) forwards1 forwards2
          in
          let everything_involved =
            N.Set.union everything_involved1 everything_involved2
          in
          Freshening { forwards; everything_involved; }
        else
          let () =
            Format.eprintf "Broken freshening:@\n\
              second: %a@ first:%a@ \nBacktrace:\n%s\n%!"
              (N.Map.print N.print) forwards2
              (N.Map.print N.print) forwards1
              (Printexc.raw_backtrace_to_string (Printexc.get_callstack 20))
          in
          Branch {
            newer = squash_freshening second;
            older = squash_freshening first;
          }
      | _, _ ->
        Branch {
          newer = squash_freshening second;
          older = squash_freshening first;
        }
end
