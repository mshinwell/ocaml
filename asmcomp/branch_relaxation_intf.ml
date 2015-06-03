(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                   Mark Shinwell, Jane Street Europe                 *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

module type S = sig
  (* The distance between two instructions, in arbitrary units (typically
     the natural word size of instructions). *)
  type distance = int

  module Cond_branch : sig
    (* The various types of conditional branches for a given target that
       may require relaxation. *)
    type t

    (* All values of type [t] that the emitter may produce. *)
    val all : t list

    (* If [max_displacement branch] is [n] then [branch] is assumed to
       reach any address in the range [pc - n, pc + n] (inclusive), after
       the [pc] of the branch has been adjusted by [offset_pc_at_branch]
       (see below). *)
    val max_displacement : t -> distance

    (* Which variety of conditional branch may be produced by the emitter for a
       given instruction description.  For the moment we assume that only one
       such variety per instruction description is needed. *)
    val classify_instr : Linearize.instruction_desc -> t option
  end

  module Out_of_line_code : sig
    (* Modelling of blocks of out-of-line code emitted immediately and
       contiguously after a function body itself. *)

    type t = Call_gc | Check_bound

    (* Determine whether a specific operation may branch to an
       out-of-line code block. *)
    val classify_specific_op : Arch.specific_operation -> t option

    (* The maximum size of an out-of-line code block.  This is never called
       if [out_of_line_code_size], below, is [`Constant]. *)
    val max_size : t -> distance
  end

  (* Whether the target has a constant out-of-line code block size or if
     we need to calculate it from the instruction stream. *)
  val out_of_line_code_size : [ `Constant of distance | `Variable ]

  (* The value to be added to the program counter (in [distance] units)
     when it is at a branch instruction, prior to calculating the distance
     to a branch target. *)
  val offset_pc_at_branch : distance

  (* The maximum size of a given instruction. *)
  val instr_size : Linearize.instruction_desc -> distance

  (* Insertion of target-specific code to relax operations that cannot be
     relaxed generically.  For efficiency and simplicity, these relaxation
     operations are not allowed to change the number of call GC or bounds
     check points. *)
  val relax_allocation : num_words:int -> Linearize.instruction_desc
  val relax_intop_checkbound : unit -> Linearize.instruction_desc
  val relax_intop_imm_checkbound : bound:int -> Linearize.instruction_desc
  val relax_specific_op : Arch.specific_operation -> Linearize.instruction_desc
end
