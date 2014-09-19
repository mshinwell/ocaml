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

(* From lambda to assembly code *)

val compile_implementation :
    ?toplevel:(string -> bool) ->
    source_file_path:string option ->
    string -> Format.formatter
    -> (int * ((Path.t * Ident.t * Types.type_expr * Ident.t * int) list)) * Lambda.lambda
    -> unit
val compile_phrase :
    Format.formatter -> dwarf:Dwarf.t option -> Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: Format.formatter -> error -> unit
