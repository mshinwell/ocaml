(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = Table_by_int_id.Id.t

module Data = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    linkage_name : Linkage_name.t;
  }

  let print ppf { compilation_unit; linkage_name; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(linkage_name@ %a)@]\
        )@]"
      Compilation_unit.print compilation_unit
      Linkage_name.print linkage_name

  let hash { compilation_unit; linkage_name; } =
    Hashtbl.hash (Compilation_unit.hash compilation_unit,
      Linkage_name.hash linkage_name)

  let equal
      { compilation_unit = compilation_unit1; linkage_name = linkage_name1; }
      { compilation_unit = compilation_unit2; linkage_name = linkage_name2; } =
    Compilation_unit.equal compilation_unit1 compilation_unit2
      && Linkage_name.equal linkage_name1 linkage_name2
end

module Table = Table_by_int_id.Make (Data)

let global_table = ref (Table.create ())

let initialise () =
  global_table := Table.create ()

let compilation_unit t = (Table.find !global_table t).compilation_unit
let linkage_name t = (Table.find !global_table t).linkage_name

include Identifiable.Make (struct
  include Table_by_int_id.Id

  let print ppf t =
    Format.fprintf ppf "@<0>%s" (Flambda_colours.symbol ());
    Compilation_unit.print ppf (compilation_unit t);
    Format.pp_print_string ppf ".";
    Linkage_name.print ppf (linkage_name t);
    Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let unsafe_create compilation_unit linkage_name =
  let data : Data.t = { compilation_unit; linkage_name; } in
  Table.add !global_table data

let create compilation_unit linkage_name =
  let unit_linkage_name =
    Linkage_name.to_string
      (Compilation_unit.get_linkage_name compilation_unit)
  in
  let linkage_name =
    Linkage_name.create
      (unit_linkage_name ^ "__" ^ (Linkage_name.to_string linkage_name))
  in
  unsafe_create compilation_unit linkage_name

let rename t =
  create (compilation_unit t) (Linkage_name.rename (linkage_name t))

let import_for_pack _ ~pack:_ = Misc.fatal_error "Not yet implemented"

let in_compilation_unit t comp_unit =
  Compilation_unit.equal (compilation_unit t) comp_unit

let is_predefined_exception t =
  Compilation_unit.is_predefined_exception (compilation_unit t)
