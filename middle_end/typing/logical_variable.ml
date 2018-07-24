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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = {
  name : Name.t;
  kind : Flambda_kind.t;
}

include Hashtbl.Make_with_map (struct
  type nonrec t = t

  let compare 
        { name = name1; kind = kind1; }
        { name = name2; kind = kind2; } =
    let c = Name.compare name1 name2 in
    if c <> 0 then c
    else Flambda_kind.compare kind1 kind2

  let hash { name; kind; } =
    Hashtbl.hash (Name.hash name, Flambda_kind.hash kind)

  let print ppf { name; kind; } =
    Format.fprintf ppf "@[(%a ::@ %a)@]"
      Name.print name
      Flambda_kind.print kind
end)

let create kind =
  { name = Name.var (Variable.create "lv");
    kind;
  }

let name t = t.name
let kind t = t.kind

let apply_name_permutation { name; kind; } perm =
  let name = Name_permutation.apply_name perm name in
  { name; kind; }

let freshen t freshening =
  apply_name_permutation t (Freshening.name_permutation freshening)
