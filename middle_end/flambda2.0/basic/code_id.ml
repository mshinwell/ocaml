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

(* CR mshinwell: Move to Reg_width_things so we can then do
   Code_id_or_symbol for free *)

[@@@ocaml.warning "+a-30-40-41-42"]

module Id = Table_by_int_id.Id

module Code_id_data = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    name : string;
    name_stamp : int;
  }

  let flags = 0

  let print ppf { compilation_unit; name; name_stamp; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(name@ %s)@]@ \
        @[<hov 1>(name_stamp@ %d)@]\
        )@]"
      Compilation_unit.print compilation_unit
      name
      name_stamp

  let hash { compilation_unit; name = _; name_stamp; } =
    (* The [name_stamp] uniquely determines [name]. *)
    Hashtbl.hash (Compilation_unit.hash compilation_unit, name_stamp)

  let equal
      { compilation_unit = compilation_unit1; name = _;
        name_stamp = name_stamp1; }
      { compilation_unit = compilation_unit2; name = _;
        name_stamp = name_stamp2; }
      =
    Int.equal name_stamp1 name_stamp2
      && Compilation_unit.equal compilation_unit1 compilation_unit2
end

type t = Id.t

module Table = Table_by_int_id.Make (Code_id_data)
let grand_table_of_code_ids = ref (Table.create ())

let initialise () =
  grand_table_of_code_ids := Table.create ()

let find_data t = Table.find !grand_table_of_code_ids t

let get_compilation_unit t = (find_data t).compilation_unit
let name t = (find_data t).name
let name_stamp t = (find_data t).name_stamp

let previous_name_stamp = ref (-1)

let create ~name compilation_unit =
  let name_stamp =
    (* CR mshinwell: check for overflow on 32 bit *)
    incr previous_name_stamp;
    !previous_name_stamp
  in
  let data : Code_id_data.t =
    { compilation_unit;
      name;
      name_stamp;
    }
  in
  Table.add !grand_table_of_code_ids data

let rename t = create ~name:(name t) (get_compilation_unit t)

let in_compilation_unit t comp_unit =
  Compilation_unit.equal (get_compilation_unit t) comp_unit

let code_symbol t =
  let unique_name = Printf.sprintf "%s_%d" (name t) (name_stamp t) in
  let linkage_name = Linkage_name.create (unique_name ^ "_code") in
  Symbol.create (get_compilation_unit t) linkage_name

module T0 = struct
  let compare = Id.compare
  let equal = Id.equal
  let hash = Id.hash

  let print ppf t =
    Format.fprintf ppf "@<0>%s%s/%d@<0>%s"
      (Flambda_colours.code_id ())
      (name t) (name_stamp t)
      (Flambda_colours.normal ())

  let output chan t = print (Format.formatter_of_out_channel chan) t
end

include T0
module T = struct
  type nonrec t = t
  include T0
end

module Set = Patricia_tree.Make_set (struct let print = print end)
module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
module Tbl = Identifiable.Make_tbl (Numbers.Int) (Map)

let invert_map map =
  Map.fold (fun older newer invert_map ->
      Map.add newer older invert_map)
    map
    Map.empty
