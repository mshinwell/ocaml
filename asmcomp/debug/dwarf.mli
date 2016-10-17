(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A value of type [t] holds all state necessary to emit DWARF debugging
    information for a single compilation unit. *)
type t

val create
   : source_provenance:Timings.source_provenance
  -> t

(** For dealing with "Let_symbol" bindings. *)
val dwarf_for_toplevel_constants
   : t
  -> Clambda.preallocated_constant list
  -> unit

(** For dealing with "Initialize_symbol" bindings. *)
val dwarf_for_toplevel_inconstants
   : t
  -> Clambda.preallocated_block list
  -> unit

val dwarf_for_function_definition
   : t
  -> fundecl:Linearize.fundecl
  -> available_ranges:Available_ranges.t
  -> emit_info:Emit.fundecl_result
  -> unit

val emit : t -> (module Asm_directives_intf.S) -> unit
