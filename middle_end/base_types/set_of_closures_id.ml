(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module Id : Ext_types.Id = Ext_types.Id (struct end)
module T = Ext_types.UnitId (Id) (Compilation_unit)

include T
include Ext_types.Identifiable.Make (T)

let get_compilation_unit t = unit t

let partition_set_by_compilation_unit t_set =
  let add_set set_of_closures_id map =
    let unit = get_compilation_unit set_of_closures_id in
    let internal_set =
      try Compilation_unit.Map.find unit map
      with Not_found -> Set.empty
    in
    Compilation_unit.Map.add unit
      (Set.add set_of_closures_id internal_set) map
  in
  Set.fold add_set t_set Compilation_unit.Map.empty

let partition_map_by_compilation_unit t_map =
  let add_map set_of_closures_id datum map =
    let unit = get_compilation_unit set_of_closures_id in
    let internal_map =
      try Compilation_unit.Map.find unit map
      with Not_found -> Map.empty
    in
    Compilation_unit.Map.add unit
      (Map.add set_of_closures_id datum internal_map) map
  in
  Map.fold add_map t_map Compilation_unit.Map.empty
