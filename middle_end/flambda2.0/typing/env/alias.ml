(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  simple : Simple.t;
  kind : Flambda_kind.t;
  binding_time : Binding_time.t;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare
        { simple = simple1; kind = kind1; binding_time = binding_time1; }
        { simple = simple2; kind = kind2; binding_time = binding_time2; } =
    let c = Simple.compare simple1 simple2 in
    if c <> 0 then c
    else
      let c = Flambda_kind.compare kind1 kind2 in
      if c <> 0 then c
      else
        Binding_time.compare binding_time1 binding_time2

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash { simple; kind; binding_time; } =
    Hashtbl.hash (Simple.hash simple,
      Flambda_kind.hash kind,
      Binding_time.hash binding_time)

  let print ppf { simple; kind; binding_time; } =
    Format.fprintf ppf "@[<hov 1>\
        @[<hov 1>(simple@ %a)@]@ \
        @[<hov 1>(kind@ %a)@]@ \
        @[<hov 1>(binding_time@ %a)@]\
        @]"
      Simple.print simple
      Flambda_kind.print kind
      Binding_time.print binding_time

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let create kind simple binding_time =
  { simple;
    kind;
    binding_time;
  }

let create_name kind name binding_time =
  create kind (Simple.name name) binding_time

let defined_earlier t ~than =
  match Simple.descr t.simple, Simple.descr than.simple with
  | (Const _ | Discriminant _), (Const _ | Discriminant _) ->
    Simple.compare t.simple than.simple <= 0
  | (Const _ | Discriminant _), Name _ -> true
  | Name _, (Const _ | Discriminant _) -> false
  | Name name1, Name name2 ->
    if Name.equal name1 name2 then
      false
    else
      let time1 = t.binding_time in
      let time2 = than.binding_time in
      if Binding_time.equal time1 time2 then begin
          Misc.fatal_errorf "Unequal [Alias]es with same binding time: \
              %a and %a"
            print t
            print than
        end;
      Binding_time.strictly_earlier time1 ~than:time2

let simple t = t.simple
let kind t = t.kind
