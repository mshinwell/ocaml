(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  name : Name.t;
  occurrence_kind : Name_occurrences.Kind.t;
}

let create name occurrence_kind =
  { name;
    occurrence_kind;
  }

let name t = t.name
let occurrence_kind t = t.occurrence_kind

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf { name; occurrence_kind; } =
    Format.fprintf ppf "@[<hov 1>\
        @[<hov 1>(name@ %a)@]@ \
        @[<hov 1>(occurrence_kind@ %a)@]\
        @]"
      Name.print name
      Name_occurrences.Kind.print occurrence_kind

  let compare
        { name = name1; occurrence_kind = occurrence_kind1; }
        { name = name2; occurrence_kind = occurrence_kind2; } =
    let c = Name.compare name1 name2 in
    if c <> 0 then c
    else Name_occurrences.Kind.compare occurrence_kind1 occurrence_kind2

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash _ = Misc.fatal_error "Not yet implemented"

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)
