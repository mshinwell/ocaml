(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Sort = struct
  type t =
    | Int
    | Tag
    | Is_int

  let to_lowercase_string t =
    match t with
    | Int -> "int"
    | Tag -> "tag"
    | Is_int -> "is_int"

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Int -> Format.fprintf ppf "Int"
      | Tag -> Format.fprintf ppf "Tag"
      | Is_int -> Format.fprintf ppf "Is_int"

    let output _ _ = Misc.fatal_error "Not yet implemented"

    let compare t1 t2 = Stdlib.compare t1 t2
    let equal t1 t2 = (compare t1 t2 = 0)
    let hash t = Hashtbl.hash t
  end)
end

type t = {
  sort : Sort.t;
  int : Targetint.OCaml.t;
}

let create sort i =
  if Targetint.OCaml.compare i Targetint.OCaml.zero < 0 then None
  else
    Some {
      sort;
      int = i;
    }

let create_exn sort i =
  match create sort i with
  | Some t -> t
  | None ->
    Misc.fatal_errorf "Discriminant.of_int_exn: invalid discriminant %a"
      Targetint.OCaml.print i

let of_int_exn sort i =
  let ti = Targetint.OCaml.of_int i in
  match create sort ti with
  | Some t -> t
  | None ->
    Misc.fatal_errorf "Discriminant.of_int_exn: invalid discriminant %d" i

let of_tag t =
  let tag = Tag.to_int t in
  let ti = Targetint.OCaml.of_int tag in
  match create Tag ti with
  | Some t -> t
  | None -> assert false

let to_tag t = Tag.create_from_targetint t.int

let to_int t = t.int

let sort t = t.sort

let zero = create_exn Int Targetint.OCaml.zero

let bool_false = create_exn Int Targetint.OCaml.zero
let bool_true = create_exn Int Targetint.OCaml.one

let is_int_false = create_exn Is_int Targetint.OCaml.zero
let is_int_true = create_exn Is_int Targetint.OCaml.one

include Identifiable.Make (struct
  type nonrec t = t

  let compare { sort = sort1; int = int1; } { sort = sort2; int = int2; } =
    let c = Sort.compare sort1 sort2 in
    if c <> 0 then c
    else Stdlib.compare int1 int2

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash { sort; int; } = Hashtbl.hash (Sort.hash sort, int)

  let print ppf { sort = _; int; } =
    Format.fprintf ppf "@[@<0>%s%a@<0>%s@]"
      (Flambda_colours.discriminant ())
      Targetint.OCaml.print int
      (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let all_bools_set = Set.of_list [bool_false; bool_true]
let all_is_int_set = Set.of_list [is_int_false; is_int_true]
