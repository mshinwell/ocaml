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

(** Information about a compilation unit stored in .cmx files. *)

open Symbol
open Abstract_identifiers

module Export_id : Ext_types.UnitId
   with module Compilation_unit := Compilation_unit

type tag = int

type _ boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type descr =
  | Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_string
  | Value_closure of descr_closure
  | Value_set_of_closures of descr_set_of_closures

and descr_closure = {
  fun_id : Closure_id.t;
  closure : value_set_of_closures;
}

and descr_set_of_closures = {
  (* CR mshinwell: rename to [set_of_closures_id] *)
  closure_id : Set_of_closures_id.t;
  bound_var : approx Var_within_closure.Map.t;
  results : approx Closure_id.Map.t;
}

and approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t

type exported = {
  functions : unit Flambda.function_declarations Set_of_closures_id.Map.t;
  (** Code of exported functions indexed by function identifier *)
  functions_off : unit Flambda.function_declarations Closure_id.Map.t;
  (** Code of exported functions indexed by offset identifier *)
  ex_values : descr Export_id.Map.t Compilation_unit.Map.t;
  (** Structure of exported values *)
  globals : approx Ident.Map.t;
  (** Global variables provided by the unit: usually only the top-level
      module identifier, but there may be multiple identifiers in the case
      of packs. *)
  id_symbol : Symbol.t Export_id.Map.t Compilation_unit.Map.t;
  symbol_id : Export_id.t Symbol.Map.t;
  (** Associates symbols and values *)
  offset_fun : int Closure_id.Map.t;
  (** Positions of function pointers in their closures *)
  (* CR mshinwell: update comments, just pasted in from elsewhere *)
  (* [fun_offset_table] associates a function label to its offset inside
     a closure.  One table suffices, since the identifiers used as keys
     are globally unique. *)
  (* [fv_offset_table] is like [fun_offset_table], but for free variables. *)
  offset_fv : int Var_within_closure.Map.t;
  (** Positions of value pointers in their closures *)
  constants : Symbol.Set.t;
  (** Symbols that are effectively constants (the top-level module is not
      always a constant for instance) *)
  constant_closures : Set_of_closures_id.Set.t;
  kept_arguments : Variable.Set.t Set_of_closures_id.Map.t;
}

val empty_export : exported

(** Union of export information.  Verifies that there are no identifier
    clashes. *)
val merge : exported -> exported -> exported

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

val print_approx : Format.formatter -> exported -> unit
val print_symbols : Format.formatter -> exported -> unit
val print_all : Format.formatter -> exported -> unit
