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
    mutable available_before: Reg.Set.t;
    mutable phantom_available_before: Ident.Set.t;
  }

and instruction_desc =
  | Lprologue
  (** [Lprologue] enables instructions (specifically, labels) to be inserted
      before the function prologue.  This is used for emission of DWARF
      information. *)
  | Lend
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
  | Lcapture_stack_offset of int option ref
  (* CR mshinwell: make integer non-optional and update comment *)
  (** [Lcapture_stack_offset] denotes the start of an available subrange (cf.
      available_ranges.mli).  The associated register is stored in [arg.(0)] of
      the instruction.  [Lcapture_stack_offset] only needs to be present when
      that register is assigned to the stack.
      The optional integer must be filled in by the assembly
      emitter in the case where that register is assigned to the stack; the
      integer denotes the byte offset from the stack pointer of the register's
      slot at the start of the subrange (and hence at all points during, since
      the stack pointer may not change during a subrange). *)

val has_fallthrough :  instruction_desc -> bool
val end_instr: instruction
val instr_cons:
  instruction_desc -> Reg.t array -> Reg.t array -> instruction
    -> available_before:Reg.Set.t -> instruction
val invert_test: Mach.test -> Mach.test

type fundecl =
  { fun_name: string;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t;
    fun_human_name : string;
    fun_arity : int;
    fun_module_path : Path.t option;
    fun_phantom_lets :
      (Clambda.ulet_provenance * phantom_defining_expr) Ident.Map.t;
  }

val reset : unit -> unit
val fundecl: Mach.fundecl -> fundecl
