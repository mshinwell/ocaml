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
  var : Variable.t;
  occurrence_kind : Name_occurrences.Kind.t;
}

let create var occurrence_kind =
  { var;
    occurrence_kind;
  }

let var t = t.var
let occurrence_kind t = t.occurrence_kind

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf { var; occurrence_kind; } =
    Format.fprintf ppf "@[<hov 1>\
        @[<hov 1>(var@ %a)@]@ \
        @[<hov 1>(occurrence_kind@ %a)@]\
        @]"
      Variable.print var
      Name_occurrences.Kind.print occurrence_kind

  let compare
        { var = var1; occurrence_kind = occurrence_kind1; }
        { var = var2; occurrence_kind = occurrence_kind2; } =
    let c = Variable.compare var1 var2 in
    if c <> 0 then c
    else Name_occurrences.Kind.compare occurrence_kind1 occurrence_kind2

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash _ = Misc.fatal_error "Not yet implemented"

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)
