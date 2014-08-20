(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Generation of assembly code *)

val fundecl: dwarf:Dwarf.t option -> Linearize.fundecl -> unit
val data: Cmm.data_item list -> unit
val begin_assembly: unit -> string * string  (* beginning and end of code symbols *)
val end_assembly: end_of_code_symbol:string -> unit

val emit_string : string -> unit
val emit_symbol : string -> unit
val emit_label : Linearize.label -> unit
val emit_label_declaration : label_name:Linearize.label -> unit
val emit_section_declaration : section_name:string -> unit
val emit_switch_to_section : section_name:string -> unit
