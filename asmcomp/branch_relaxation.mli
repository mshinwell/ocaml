(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                   Mark Shinwell, Jane Street Europe                 *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Fixup conditional branches that exceed hardware allowed range *)

module Make (T : sig
  type cond_branch

  val instr_size : Linearize.instruction_desc -> int
  val classify_instr : Linearize.instruction -> cond_branch option
  val max_displacement_in_words : cond_branch -> int
  val code_for_far_allocation : num_words:int -> Linearize.instruction_desc
end) : sig
  val fixup_branches : Linearize.instruction -> unit
end
