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

(* Representation of machine code by sequences of pseudoinstructions *)

type label = Cmm.label

type integer_comparison =
    Isigned of Cmm.integer_comparison
  | Iunsigned of Cmm.integer_comparison

type integer_operation =
    Iadd | Isub | Imul | Imulh | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Icomp of integer_comparison
  | Icheckbound of { label_after_error : label option;
        spacetime_index : int; }

type float_comparison = Cmm.float_comparison

type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of float_comparison
  | Ioddtest
  | Ieventest

type phantom_defining_expr =
  | Iphantom_const_int of Targetint.t
  | Iphantom_const_symbol of Backend_sym.t
  | Iphantom_var of Backend_var.t
  | Iphantom_offset_var of { var : Backend_var.t; offset_in_words : int; }
  | Iphantom_read_field of { var : Backend_var.t; field : int; }
  | Iphantom_read_symbol_field of { sym : Backend_sym.t; field : int; }
  | Iphantom_block of { tag : int; fields : Backend_var.t option list; }

type call_labels = {
  before : label;
  after : label;
}

type operation =
    Imove
  | Ispill
  | Ireload
  | Iconst_int of nativeint
  | Iconst_float of int64
  | Iconst_symbol of Backend_sym.t
  | Icall_ind of { call_labels : call_labels; }
  | Icall_imm of {
      func : Backend_sym.t;
      callee_dbg : Debuginfo.Function.t option;
      call_labels : call_labels;
    }
  | Itailcall_ind of { call_labels : call_labels; }
  | Itailcall_imm of {
      func : Backend_sym.t;
      callee_dbg : Debuginfo.Function.t option;
      call_labels : call_labels;
    }
  | Iextcall of { func : Backend_sym.t; alloc : bool;
      call_labels : call_labels; }
  | Istackoffset of int
  | Iload of Cmm.memory_chunk * Arch.addressing_mode
  | Istore of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Ialloc of { bytes : int; label_after_call_gc : label option;
        spacetime_index : int; }
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat
  | Ispecific of Arch.specific_operation
  | Iname_for_debugger of { ident : Backend_var.t;
      is_parameter : Is_parameter.t;
      provenance : Backend_var.Provenance.t option; is_assignment : bool; }

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    mutable dbg: Debuginfo.t;
    phantom_available_before: Backend_var.Set.t;
    mutable live: Reg.Set.t;
    mutable available_before: Reg_availability_set.t;
    mutable available_across: Reg_availability_set.t option;
  }

and instruction_desc =
    Iend
  | Iop of operation
  | Ireturn
  | Iifthenelse of test * instruction * instruction
  | Iswitch of int array * instruction array
  | Iloop of instruction
  | Icatch of Cmm.rec_flag * (int * instruction) list * instruction
  | Iexit of int
  | Itrywith of instruction * instruction
  | Iraise of Cmm.raise_kind

type spacetime_part_of_shape =
  | Direct_call_point of { callee : Backend_sym.t; }
  | Indirect_call_point
  | Allocation_point

type spacetime_shape = (spacetime_part_of_shape * Cmm.label) list

type fundecl =
  { fun_name: Backend_sym.t;
    fun_args: Reg.t array;
    fun_body: instruction;
    fun_codegen_options : Cmm.codegen_option list;
    fun_dbg : Debuginfo.Function.t;
    fun_spacetime_shape : spacetime_shape option;
    fun_phantom_lets :
      (Backend_var.Provenance.t option * phantom_defining_expr)
        Backend_var.Map.t;
  }

let rec dummy_instr =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    phantom_available_before = Backend_var.Set.empty;
    live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let end_instr () =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    phantom_available_before = Backend_var.Set.empty;
    live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

type phantom_available_before =
  | Take_from of instruction
  | Exactly of Backend_var.Set.t

let instr_cons_debug ~phantom_available_before d a r dbg n =
  let phantom_available_before =
    match phantom_available_before with
    | Take_from insn -> insn.phantom_available_before
    | Exactly avail -> avail
  in
  { desc = d; next = n; arg = a; res = r; dbg = dbg; live = Reg.Set.empty;
    phantom_available_before;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let rec instr_iter f i =
  match i.desc with
    Iend -> ()
  | _ ->
      f i;
      match i.desc with
        Iend -> ()
      | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _) -> ()
      | Iifthenelse(_tst, ifso, ifnot) ->
          instr_iter f ifso; instr_iter f ifnot; instr_iter f i.next
      | Iswitch(_index, cases) ->
          for i = 0 to Array.length cases - 1 do
            instr_iter f cases.(i)
          done;
          instr_iter f i.next
      | Iloop(body) ->
          instr_iter f body; instr_iter f i.next
      | Icatch(_, handlers, body) ->
          instr_iter f body;
          List.iter (fun (_n, handler) -> instr_iter f handler) handlers;
          instr_iter f i.next
      | Iexit _ -> ()
      | Itrywith(body, handler) ->
          instr_iter f body; instr_iter f handler; instr_iter f i.next
      | Iraise _ -> ()
      | _ ->
          instr_iter f i.next

let spacetime_node_hole_pointer_is_live_before insn =
  match insn.desc with
  | Iop op ->
    begin match op with
    | Icall_ind _ | Icall_imm _ | Itailcall_ind _ | Itailcall_imm _ -> true
    | Iextcall { alloc; } -> alloc
    | Ialloc _ ->
      (* Allocations are special: the call to [caml_call_gc] requires some
         instrumentation code immediately prior, but this is not inserted until
         the emitter (since the call is not visible prior to that in any IR).
         As such, none of the Mach / Linearize analyses will ever see that
         we use the node hole pointer for these, and we do not need to say
         that it is live at such points. *)
      false
    | Iintop op | Iintop_imm (op, _) ->
      begin match op with
      | Icheckbound _
        (* [Icheckbound] doesn't need to return [true] for the same reason as
           [Ialloc]. *)
      | Iadd | Isub | Imul | Imulh | Idiv | Imod
      | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
      | Icomp _ -> false
      end
    | Ispecific specific_op ->
      Arch.spacetime_node_hole_pointer_is_live_before specific_op
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _
    | Iconst_symbol _ | Istackoffset _ | Iload _ | Istore _
    | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
    | Ifloatofint | Iintoffloat
    | Iname_for_debugger _ -> false
    end
  | Iend | Ireturn | Iifthenelse _ | Iswitch _ | Iloop _ | Icatch _
  | Iexit _ | Itrywith _ | Iraise _ -> false

let operation_can_raise op =
  match op with
  | Icall_ind _ | Icall_imm _ | Iextcall _
  | Iintop (Icheckbound _) | Iintop_imm (Icheckbound _, _)
  | Ialloc _ -> true
  | _ -> false
