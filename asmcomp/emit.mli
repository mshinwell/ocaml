(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Generation of assembly code *)

type fundecl_result = {
  end_of_function_label : Linearize.label;
}

val fundecl: Linearize.fundecl -> fundecl_result
val data: Cmm.data_item list -> unit

val begin_assembly: unit -> unit

val end_assembly
   : before_code_generation:((module Asm_directives.S) -> unit)
  -> unit
