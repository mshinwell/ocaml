(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                      Pierre Chambart (OCamlPro)                        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Functions that access export information held within [Compilenv]. *)

open Abstract_identifiers

val find_declaration
   : t
  -> Closure_id.t
  -> unit Flambda.function_declarations

val get_fun_offset : t -> Closure_id.t -> int
val get_fun_offset_from_var : t -> Variable.t -> int
val get_fv_offset : t -> Var_within_closure.t -> int
val get_local_fv_offset_from_var : t -> Variable.t -> int

val fundecls_for_closure_id
   : t
  -> Closure_id.t
  -> unit Flambda.function_declarations

val fundecls_for_set_of_closures_id
   : t
  -> Set_of_closures_id.t
  -> unit Flambda.function_declarations

val is_closure_constant
   : t
  -> Closure_id.t
  -> bool

val is_set_of_closures_constant
   : t
  -> Set_of_closures_id.t
  -> bool result

(** Transform the informations from [exported] to be suitable to
    be reexported as the informations for a pack named [pack]
    containing units [pack_units].
    It mainly change symbols of units [pack_units] to refer to
    [pack] instead. *)
val import_for_pack
   : pack_units:Compilation_unit.Set.t
  -> pack:Compilation_unit.t
  -> exported
  -> exported

(** Drops the state after importing several units in the same pack. *)
val clear_import_state : unit -> unit

val find_description : Export_id.t -> exported -> descr

val nest_eid_map
   : 'a Export_id.Map.t
  -> 'a Export_id.Map.t Compilation_unit.Map.t
