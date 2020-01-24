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
  name_mode : Name_mode.t;
}

type elt = t

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let { simple = simple1; kind = kind1; binding_time = binding_time1;
            name_mode = name_mode1; } =
        t1
      in
      let { simple = simple2; kind = kind2; binding_time = binding_time2;
            name_mode = name_mode2; } =
        t2
      in
      let c = Simple.compare simple1 simple2 in
      if c <> 0 then c
      else
        let c = Flambda_kind.compare kind1 kind2 in
        if c <> 0 then c
        else
          let c = Binding_time.compare binding_time1 binding_time2 in
          if c <> 0 then c
          else
            Name_mode.compare_total_order name_mode1
              name_mode2

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash { simple; kind; binding_time; name_mode; } =
    Hashtbl.hash (Simple.hash simple,
      Flambda_kind.hash kind,
      Binding_time.hash binding_time,
      Name_mode.hash name_mode)

  let print ppf { simple; kind; binding_time; name_mode; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(%a :: %a)@]@ \
        @[<hov 1>(binding_time@ %a)@]@ \
        @[<hov 1>(mode@ %a)@]\
        )@]"
      Simple.print simple
      Flambda_kind.print kind
      Binding_time.print binding_time
      Name_mode.print name_mode

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let create kind simple binding_time name_mode =
  let simple = Simple.without_rec_info simple in
  { simple;
    kind;
    binding_time;
    name_mode;
  }

let create_name kind name binding_time name_mode =
  create kind (Simple.name name) binding_time name_mode

let defined_earlier t ~than =
  match Simple.descr t.simple, Simple.descr than.simple with
  | Const _, Const _ ->
    Simple.compare t.simple than.simple < 0
  | Const _, Name _ -> true
  | Name _, Const _ -> false
  | Name name1, Name name2 ->
    if Name.equal name1 name2 then
      false
    else
      let time1 = t.binding_time in
      let time2 = than.binding_time in
      if Binding_time.equal time1 time2 then begin
        let () =
          Format.eprintf "Equal times:@ t=%a@ and@ than=%a\n!"
            print t
            print than
        in
        (* CR mshinwell: We should think about this some more, it seems tricky.
           Currently we assign all imported variables the same binding time
           so we may need to break a tie here.  We just use [compare], but is
           this ok? *)
        let this_comp_unit = Compilation_unit.get_current_exn () in
        let name1_is_imported_var =
          match name1 with
          | Var var ->
            Compilation_unit.equal (Variable.compilation_unit var)
              this_comp_unit
          | Symbol _ -> false
        in
        let name2_is_imported_var =
          match name2 with
          | Var var ->
            Compilation_unit.equal (Variable.compilation_unit var)
              this_comp_unit
          | Symbol _ -> false
        in
        match name1_is_imported_var, name2_is_imported_var with
        | true, true -> Name.compare name1 name2 <= 0
        | false, true -> false
        | true, false -> true
        | false, false ->
          Misc.fatal_errorf "Unequal [Alias]es with same binding time: \
              %a and %a"
            print t
            print than
      end else begin
        Binding_time.strictly_earlier time1 ~than:time2
      end

let simple t = t.simple
let kind t = t.kind
let name_mode t = t.name_mode

let name t =
  match Simple.descr t.simple with
  | Name name -> Some name
  | _ -> None

let implicitly_bound_and_canonical t =
  match Simple.descr t.simple with
  | Const _ -> true
  | Name name ->
    let comp_unit = Name.compilation_unit name in
    let this_comp_unit = Compilation_unit.get_current_exn () in
    not (Compilation_unit.equal comp_unit this_comp_unit)

module Order_within_equiv_class = Name_mode

let order_within_equiv_class t = t.name_mode

module Set_ordered_by_binding_time = Stdlib.Set.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if equal t1 t2 then 0
    else if defined_earlier t1 ~than:t2 then -1
    else 1
end)
