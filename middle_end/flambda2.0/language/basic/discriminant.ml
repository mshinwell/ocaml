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

module Kind = struct
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
  kind : Kind.t;
  int : Targetint.OCaml.t;
}

let create kind i =
  if Targetint.OCaml.compare i Targetint.OCaml.zero < 0 then None
  else
    { kind;
      int = i;
    }

let create_exn i =
  match create i with
  | Some t -> t
  | None ->
    Misc.fatal_errorf "Discriminant.of_int_exn: invalid discriminant %a"
      Targetint.OCaml.print i

let of_int_exn kind i =
  let ti = Targetint.OCaml.of_int i in
  match create kind ti with
  | Some t -> t
  | None ->
    Misc.fatal_errorf "Discriminant.of_int_exn: invalid discriminant %d" i

let of_tag t =
  let tag = Tag.to_int t in
  let ti = Targetint.OCaml.of_int i in
  match create Tag ti with
  | Some t -> t
  | None -> assert false

let to_tag t = Tag.create_from_targetint t.int

let to_int t = t.int

let zero = create Int Targetint.OCaml.zero

let bool_false = create Int zero
let bool_true = create Int Targetint.OCaml.one

include Identifiable.Make (struct
  type nonrec t = t

  let compare { kind = kind1; int = int1; } { kind = kind2; int = int2; } =
    let c = Kind.compare kind1 kind2 in
    if c <> 0 then c
    else Stdlib.compare int1 int2

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash { kind; int; } = Hashtbl.hash (Kind.hash kind, int)

  let print ppf { kind; int; } =
    Format.fprintf ppf "@[%s(%a@ %a)%s@]"
      (Flambda_colours.discriminant ())
      Kind.print kind
      Targetint.OCaml.print int
      (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let all_bools_set = Set.of_list [bool_false; bool_true]

      let kind =
        Discriminant.Set.fold (fun discr kind ->
            let kind' = Discriminant.kind discr in
            match kind with
            | None -> Some kind
            | Some kind ->
              if not (Discriminant.Kind.equal kind kind') then begin
                Misc.fatal_errorf "Discriminants not all of same kind:@ %a"
                  Discriminant.Set.print discrs
              end;
              Some kind)
          discrs
          None
      in
