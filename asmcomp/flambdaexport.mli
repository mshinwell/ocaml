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
  closure_id : Closure_id.t;
  set_of_closures : descr_set_of_closures;
}

and descr_set_of_closures = {
  set_of_closures_id : Set_of_closures_id.t;
  (** Approximations to the variables bound by the set of closures, indexed by
      variable-in-closure ID. *)
  bound_var : approx Var_within_closure.Map.t;
  (** Approximations to the results of the functions bound by the set of closures,
      indexed by closure ID. *)
  results : approx Closure_id.Map.t;
}

and approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t

val print_descr : Format.formatter -> descr -> unit

(* Records of type [exported] are saved within .cmx files to provide
   information about the compilation unit.  A single record of this
   type is also used to hold the union of such information for all
   imported units, for speed of access. *)
type exported = private {
  sets_of_closures : unit Flambda.function_declarations Set_of_closures_id.Map.t;
  (** Code of exported functions indexed by function identifier *)
  closures : unit Flambda.function_declarations Closure_id.Map.t;
  (** Code of exported functions indexed by offset identifier *)
  (* XXX the comments must identify why the map is important (one
     unit might export information about other units) *)
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
  (* [fv_offset_table] is like [fun_offset_table], but for free
     variables. *)
  offset_fv : int Var_within_closure.Map.t;
  (** Positions of value pointers in their closures *)
  constants : Symbol.Set.t;
  (** Symbols that are effectively constants (the top-level module is not
      always a constant for instance) *)
  constant_closures : Set_of_closures_id.Set.t;
  kept_arguments : Variable.Set.t Set_of_closures_id.Map.t;
}

(* A partially mutable version of [exported], used when constructing export
   information for the current unit. *)
type exported_mutable = {
  values : descr Export_id.Tbl.t Compilation_unit.Tbl.t;
  globals : approx Ident.Tbl.t;
  symbol_id : Export_id.t Symbol.Tbl.t;
  mutable constants : Symbol.Set.t;
  sets_of_closures :
    unit Flambda.function_declarations Set_of_closures_id.Map.t;
  closures : unit Flambda.function_declarations Closure_id.Map.t;
  constant_closures : Set_of_closures_id.Set.t;
  offset_fun : int Closure_id.Map.t;
  offset_fv : int Var_within_closure.Map.t;
}

val freeze : exported_mutable -> exported




  val empty : t

  val find_declaration
     : t
    -> Closure_id.t
    -> unit Flambda.function_declarations

  val get_fun_offset_exn : t -> Closure_id.t -> int
  val get_fv_offset_exn : t -> Var_within_closure.t -> int

  (* CR mshinwell: bad function name *)
  val closure_declaration_position_exn
     : t
    -> Closure_id.t
    -> unit Flambda.function_declarations

  val set_of_closures_declaration_position_exn
     : t
    -> Set_of_closures_id.t
    -> unit Flambda.function_declarations

  val is_set_of_closures_constant
     : t
    -> Set_of_closures_id.t
    -> bool
end

type exported = Exported.t
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
