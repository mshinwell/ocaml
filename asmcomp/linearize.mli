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

(* Transformation of Mach code into a list of pseudo-instructions. *)

type label = int
val new_label: unit -> label

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    live: Reg.Set.t;
    available_before: Reg_with_availability.Set.t; }

and instruction_desc =
    Lend
  | Lprologue
  | Lop of Mach.operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of Mach.test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lsetuptrap of label
  | Lpushtrap
  | Lpoptrap
  | Lraise of Lambda.raise_kind
  (* [Lavailable_subrange] denotes the start of an available subrange (cf.
     available_ranges.mli).  The associated register is stored in [arg.(0)] of
     the instruction.  The optional integer must be filled in by the assembly
     emitter in the case where that register is assigned to the stack; the
     integer denotes the byte offset from the stack pointer of the register's
     slot at the start of the subrange (and hence at all points during, since
     the stack pointer may not change during a subrange).  We inline the
     arguments rather than using a record to avoid extra indirection and
     allocation (there are likely to be very many of these nodes). *)
  | Lavailable_subrange of int option ref

val has_fallthrough :  instruction_desc -> bool
val end_instr: instruction
val instr_cons:
  instruction_desc -> Reg.t array -> Reg.t array -> instruction -> instruction
val invert_test: Mach.test -> Mach.test

type fundecl =
  { fun_name: string;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t }

val reset : unit -> unit
val fundecl: Mach.fundecl -> fundecl
