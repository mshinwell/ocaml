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

(** Descriptions of compilation units to be saved in .cmx files. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Set_of_closures_id : Hashtbl.With_map (* XXX *)

(** A structure that describes what a single compilation unit exports. *)
type t = private {
  sets_of_closures : Flambda.Function_declarations.t Set_of_closures_id.Map.t;
  (** Code of exported functions indexed by set of closures IDs. *)
  closures : Flambda.Function_declarations.t Closure_id.Map.t;
  (** Code of exported functions indexed by closure IDs. *)
  values : Flambda_type.t Export_id.Map.t Compilation_unit.Map.t;
  (** Types of exported values. *)
  symbol_id : Export_id.t Symbol.Map.t;
  (** Associates symbols and values. *)
  offset_fun : int Closure_id.Map.t;
  (** Positions of function pointers in their closures. *)
  offset_fv : int Var_within_closure.Map.t;
  (** Positions of value pointers in their closures. *)
  constant_sets_of_closures : Set_of_closures_id.Set.t;
  (* CR-soon mshinwell for pchambart: Add comment *)
  invariant_params : Variable.Set.t Variable.Map.t Set_of_closures_id.Map.t;
  (* Function parameters known to be invariant (see [Invariant_params])
     indexed by set of closures ID. *)
}

(** Export information for a compilation unit that exports nothing. *)
val empty : t

(** Create a new export information structure. *)
val create
   : sets_of_closures:Flambda.Function_declarations.t Set_of_closures_id.Map.t
  -> closures:Flambda.Function_declarations.t Closure_id.Map.t
  -> values:Flambda_type.t Export_id.Map.t Compilation_unit.Map.t
  -> symbol_id:Export_id.t Symbol.Map.t
  -> offset_fun:int Closure_id.Map.t
  -> offset_fv:int Var_within_closure.Map.t
  -> constant_sets_of_closures:Set_of_closures_id.Set.t
  -> invariant_params:Variable.Set.t Variable.Map.t Set_of_closures_id.Map.t
  -> t

(* CR-someday pchambart: Should we separate [t] in 2 types: one created by the
   current [create] function, returned by [Build_export_info]. And
   another built using t and offset_informations returned by
   [flambda_to_clambda] ?
   mshinwell: I think we should, but after we've done the first release.
*)
(** Record information about the layout of closures and which sets of
    closures are constant.  These are all worked out during the
    [Flambda_to_clambda] pass. *)
val add_clambda_info
   : t
  -> offset_fun:int Closure_id.Map.t
  -> offset_fv:int Var_within_closure.Map.t
  -> constant_sets_of_closures:Set_of_closures_id.Set.t
  -> t

(** Union of export information.  Verifies that there are no identifier
    clashes. *)
val merge : t -> t -> t

(** Look up the type of an exported value given its export ID. *)
val find_description
   : t
  -> Export_id.t
  -> Flambda_type.t

(** Partition a mapping from export IDs by compilation unit. *)
val nest_eid_map
   : 'a Export_id.Map.t
  -> 'a Export_id.Map.t Compilation_unit.Map.t

(**/**)
(* Debug printing functions. *)
val print_approx : Format.formatter -> t * Symbol.t list -> unit
val print_functions : Format.formatter -> t -> unit
val print_offsets : Format.formatter -> t -> unit
val print_all : Format.formatter -> t * Symbol.t list -> unit
