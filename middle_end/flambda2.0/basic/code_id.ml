(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Id : Id_types.Id = Id_types.Id (struct end)
module Unit_id = Id_types.UnitId (Id) (Compilation_unit)

type t = Unit_id.t

include Identifiable.Make (struct
  include Unit_id

  let print ppf t =
    Format.fprintf ppf "@<0>%s%a@<0>%s"
      (Flambda_colours.code_id ())
      print t
      (Flambda_colours.normal ())
end)

let create ~name comp_unit = Unit_id.create ~name comp_unit
let get_compilation_unit = Unit_id.unit

let name t =
  (* CR mshinwell: Id_types needs fixing to avoid this *)
  match Unit_id.name t with
  | Some name -> name
  | None -> assert false

let rename = Unit_id.rename

let in_compilation_unit t cu =
  Compilation_unit.equal (get_compilation_unit t) cu

let code_symbol t =
  let linkage_name = Linkage_name.create (Unit_id.unique_name t ^ "_code") in
  Symbol.create (get_compilation_unit t) linkage_name

let invert_map map =
  Map.fold (fun older newer invert_map ->
      Map.add newer older invert_map)
    map
    Map.empty
