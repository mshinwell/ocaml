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
  -> idents_to_original_idents:Ident.t Ident.tbl
     (** [idents_to_original_idents] gives the mapping from identifiers
         mentioned in the Linearize code (e.g. in register availability sets)
         back to identifiers that occurred in the source code.  (Note that this
         mapping might not be injective due to inlining.) *)
  -> t

val dwarf_for_function_definition
   : t
  -> fundecl:Linearize.fundecl
  -> available_ranges:Available_ranges.t
  -> emit_info:Emit.fundecl_result
  -> unit

val emit : t -> (module Asm_directives.S) -> unit
