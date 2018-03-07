(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module One_cache = struct
  type seen = S : _ * string -> seen

  type t = {
    prefix : string;
    mutable next_id : int;
    mutable printed : seen list;
  }

  let create prefix =
    { prefix;
      next_id = 0;
      printed = [];
    }

  let with_cache t ppf obj printer =
    let rec find = function
      | (S (name, obj'))::printed ->
        if obj == obj' then Format.fprintf ppf "*%s" name
        else find printed
      | [] ->
        let name = Printf.sprintf "%s%d" t.prefix t.next_id in
        t.next_id <- t.next_id + 1;
        t.printed <- (S (name, obj)) :: t.printed;
        Format.fprintf ppf "@[&%s =@ %a@]" printer ()
    in
    find t.printed
end

type t = one_cache String.Map.t

let create () = String.Map.empty

let with_cache t ppf prefix obj printer =
  match String.Map.find prefix t with
  | exception Not_found ->
    let cache = One_cache.create prefix in
    One_cache.with_cache cache ppf obj printer
  | cache ->
    One_cache.with_cache cache ppf obj printer
