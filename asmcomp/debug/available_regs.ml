(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* XCR pchambart: I'm still not convinced by the name of the pass.
   unclobered_regs maybe ?
   mshinwell: I think it's the usual name.  It matches what's used in
   CompCert for something similar IIUC.
*)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module M = Mach
module R = Reg
module RD = Reg_with_debug_info

(* XCR pchambart is this only needed for debugging ?
   mshinwell: yes, removed *)

(* CR pchambart I'm not certain that exits numbers cannot be nested:
   every other pass uses a list for that I assume *)
let avail_at_exit_table = Hashtbl.create 42
(* CR pchambart: None here is in some way the same thing as all_regs *)
let avail_at_raise = ref None

(* XCR pchambart: not 'interessting' but contain source level relevant value.
   This may be easier to just filter later (when printing ?).
   Keeping those in gdb might help debug the code generation such
   as corrupted values due to unregistered roots.
   mshinwell: filtering on name now moved to [Available_filtering]
   (although maybe that's still too early?).
*)

(* XCR-pchambart it does not really convey the fact that it's
   unreachable. Having a sum type to represent that may be cleaner.
   I don't think this allocation cost would really matter *)

let operation_can_raise (op : Mach.operation) =
  match op with
  | Icall_ind | Icall_imm _ | Iextcall _
  | Iintop Icheckbound | Iintop_imm (Icheckbound, _)
  | Ialloc _ -> true
  | _ -> false

let augment_availability_at_raise avail =
  match !avail_at_raise with
  | None -> avail_at_raise := Some avail
  | Some avail' -> avail_at_raise := Some (inter avail avail')

(* [available_regs ~instr ~avail_before] calculates, given the registers
   "available before" an instruction [instr], the registers that are available
   after [instr].  This is a forwards dataflow analysis.

   "available before" can be thought of, at the assembly level, as the set of
   registers available when the program counter is equal to the address of the
   particular instruction under consideration (that is to say, immediately
   prior to the instruction being executed).  Inputs to that instruction are
   available at this point even if the instruction will clobber them.  Results
   from the previous instruction are also available at this point.

   The [available_before] field of each instruction is updated by this
   function.
*)
let rec available_regs (instr : M.instruction) ~avail_before =
  let avail_before =
    (* If this instruction might expand into multiple machine instructions
       and clobber registers during that code sequence, make sure any
       such clobbered registers are marked as "unavailable before". *)
    (* CR-someday mshinwell: We could maybe try to improve on this in the
       future, so e.g. when stopped immediately before an allocation,
       registers aren't marked unavailable when not yet clobbered by the
       allocation code.
       Should we be using "available across" instead of "available before"?
       It's not clear that actually solves the problem. *)
    (* XCR pchambart: This test should probably surround everything,
       especialy in the case where it is changed to a sum type. *)
    match avail_before with
    | Unreachable -> Unreachable
    | Ok avail_before ->
      (* Every register that is live across an instruction should also be
          available before the instruction.  (We cannot assert that a register
          should not be an input to an instruction unless it is available due
          to the special case (about multiple instructions) below.) *)
      (* XCR pchambart: The difference between live and avail_before is
        probably due to the 'interesting' filter. *)
      assert (R.Set.subset (instr_live instr)
        (RD.Set.forget_debug_info avail_before));
      match instr.desc with
      | Iop (Ialloc _) ->
        let made_unavailable =
          RD.Set.made_unavailable_by_clobber avail_before
            ~regs_clobbered:(Proc.destroyed_at_oper instr.desc)
        in
        RD.Set.diff avail_before made_unavailable
      (* CR mshinwell: should have a hook for Ispecific cases *)
      | _ -> Ok avail_before
  in
  (* CR pchambart: Given how it's used I would rename it to
     available_across rather than available_before. *)
  instr.available_before <- avail_before;
  let avail_after =
    match avail_before with
    | Unreachable -> Unreachable
    | Ok avail_before ->
      match instr.desc with
      | Iend -> Ok avail_before
      | Ireturn | Iop Itailcall_ind | Iop (Itailcall_imm _) ->
        Unreachable
      | Iop (Iname_for_debugger { ident; which_parameter; }) ->
        (* First forget about any existing debug info to do with [ident]. *)
        let forgetting_ident =
          RD.Set.map (fun reg ->
              if RD.holds_value_of reg ident then RD.clear_debug_info reg
              else reg)
            avail_before
        in
        let avail_after = ref forgetting_ident in
        let num_parts_of_value = Array.length instr.arg in
        (* Add debug info about [ident], but only for registers that are known
           to be available. *)
        for part_of_value = 0 to num_parts_of_value - 1 do
          let reg = instr.arg.(part_of_value) in
          if RD.Set.mem_reg forgetting_ident reg then begin
            let reg =
              RD.create ~reg
                ~holds_value_of:ident
                ~part_of_value
                ~num_parts_of_value
                ~which_parameter
            in
            avail_after := RD.Set.add reg !avail_after
          end
        done;
        !avail_after
      | Iop op ->
        if operation_can_raise op then begin
          augment_availability_at_raise avail_before
        end;
        let avail_after =
          match op with
          | Icall_ind | Icall_imm _ | Iextcall _ ->
            (* Registers become unavailable across a call unless either:
               (a) they hold immediates and are not destroyed by the relevant
                   calling convention; or
               (b) they are on the stack.
               The "not destroyed by the relevant calling convention" part is
               handled by the removal of registers in [destroyed_at_oper],
               below.
            *)
            (* XCR mshinwell: still need to deal with the case where a
               variable is not spilled but is available just before the
               call.  However these don't need to be visible from a callee,
               so not extending their range to the end of the call insn
               should do it.
               mshinwell: fixed in Available_ranges, need a comment here tho *)
            RD.Set.union (RD.Set.filter RD.assigned_to_stack avail_before)
              (* CR mshinwell: deal with this "holds_non_pointer" thing *)
              (RD.Set.filter RD.holds_non_pointer avail_before)
          | _ -> avail_before
        in
        let made_unavailable =
          (* Registers are made unavailable (possibly in addition to the
             above) by:
             1. the operation being marked as one that destroys them;
             2. being clobbered by the instruction writing out results.  The
                special case for moves above keeps the following code
                straightforward. *)
          let regs_clobbered =
            R.Set.union (R.set_of_array (Proc.destroyed_at_oper instr.desc))
              (R.set_of_array instr.res)
          in
          RD.Set.made_unavailable_by_clobber avail_after ~regs_clobbered
        in
        RD.Set.union (RD.Set.without_debug_info instr.res) avail_after
      | Iifthenelse (_, ifso, ifnot) -> join [ifso; ifnot] ~avail_before
      | Iswitch (_, cases) -> join (Array.to_list cases) ~avail_before
      | Iloop body ->
        let avail_after = ref avail_before in
        (* CR pchambart: This should probably be instr.available_before to do a
           single loop in case of nested Iloop.  *)
        begin try
          while true do
            (* CR pchambart: I don't see any situations where a second
               round would remove more registers from the set. Hence
               I'm not convinced that there are situations where a
               fixpoint is required. But this is necessary for marking
               each instruction in the loop with its real
               available_before. An assertion should check that never
               more than 2 rounds are required.  *)
            let avail_after' =
              inter !avail_after
                (available_regs body ~avail_before:!avail_after)
            in
            if RD.Set.equal !avail_after avail_after' then raise Exit;
            avail_after := avail_after'
          done
        with Exit -> ()
        end;
        all_regs
      | Icatch (nfail, body, handler) ->
        let avail_after_body = available_regs body ~avail_before in
        begin match
          try Some (Hashtbl.find avail_at_exit_table nfail)
          with Not_found -> None
        with
        | None -> avail_after_body  (* The handler is unreachable. *)
        | Some avail_at_exit ->
          Hashtbl.remove avail_at_exit_table nfail;
          inter avail_after_body
            (available_regs handler ~avail_before:avail_at_exit)
        end
      | Iexit nfail ->
        begin try
          Hashtbl.replace avail_at_exit_table nfail
            (inter (Hashtbl.find avail_at_exit_table nfail) avail_before)
        with Not_found ->
          Hashtbl.add avail_at_exit_table nfail avail_before
        end;
        Unreachable
      | Itrywith (body, handler) ->
        let saved_avail_at_raise = !avail_at_raise in
        avail_at_raise := None;
        let after_body = available_regs body ~avail_before in
        let avail_before_handler =
          match !avail_at_raise with
          | None -> Unreachable
          | Some avail -> avail
        in
        avail_at_raise := saved_avail_at_raise;
        inter after_body
          (available_regs handler ~avail_before:avail_before_handler)
      | Iraise _ ->
        augment_availability_at_raise avail_before;
        Unreachable
  in
  match instr.desc with
  | Iend -> avail_after
  | _ -> available_regs instr.next ~avail_before:avail_after

and join branches ~avail_before =
  let avails = List.map (available_regs ~avail_before) branches in
  begin match avails with
  | [] -> avail_before
  | avail::avails -> List.fold_left avails avail inter
  end

let fundecl (f : Mach.fundecl) =
  (* XCR pchambart: this should be cleaned by the Icatch instruction.
     It should be replaced by an assertion *)
  assert (Hashtbl.length avail_at_exit_table = 0);
  avail_at_raise := None;
  let fun_args = R.set_of_array f.fun_args in
  let first_instr_arg = R.set_of_array f.fun_body.arg in
  assert (R.Set.subset first_instr_arg fun_args);
  let avail_before = RD.Set.without_debug_info fun_args in
  ignore ((available_regs f.fun_body ~avail_before) : RD.Set.t);
  f
