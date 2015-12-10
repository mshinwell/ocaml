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

(* CR mshinwell for pchambart: We should add a comment here describing
   exactly what is meant by an "export ID" (in terms independent of the
   asmcomp/ code). *)

module T = struct
  module Inner_id = Ext_types.Id (struct end)
  include Ext_types.UnitId (Inner_id) (Compilation_unit)
end

include T
include Ext_types.Identifiable.Make (T)

let get_compilation_unit t = unit t

(* CR-soon mshinwell: code duplication with Set_of_closures and Variable *)
let partition_map_by_compilation_unit t_map =
  let add_map export_id datum map =
    let unit = get_compilation_unit export_id in
    let internal_map =
      try Compilation_unit.Map.find unit map
      with Not_found -> Map.empty
    in
    Compilation_unit.Map.add unit
      (Map.add export_id datum internal_map) map
  in
  Map.fold add_map t_map Compilation_unit.Map.empty
