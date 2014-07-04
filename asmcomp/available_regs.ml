(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*         Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                     *)
(*  Copyright 2013--2014, Jane Street Holding                          *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

module List = ListLabels
module M = Mach

(* [overwrite_union reg_set ~overwrite_with] removes from [reg_set] all
   registers having the same location as [overwrite_with]; and then adds
   [overwrite_with] into the resulting set. *)
let overwrite_union reg_set ~overwrite_with =
  Reg.Set.fold (fun reg acc ->
    match reg.Reg.loc with
    | Reg.Unknown ->
      (* This pass is always run after register allocation. *)
      failwith "simplify_avail_set: register has no location"
    | Reg.Reg _ | Reg.Stack _ ->
      let not_sharing_loc =
        Reg.Set.filter (fun reg' -> reg.Reg.loc <> reg'.Reg.loc) acc
      in
      Reg.Set.add reg not_sharing_loc) overwrite_with (* ~init:*)reg_set

(* [available_regs ~instr ~avail_before] calculates, given the registers
   available before an instruction [instr], the registers that are available
   after [instr].  The [avail_before] fields of instructions are updated with
   the available registers, that have names, before each instruction.

   "available before" can be thought of, at the assembly level, as the set of
   registers available when the program counter is equal to the address of the
   particular instruction under consideration (that is to say, immediately
   prior to the instruction being executed).  Inputs to that instruction are
   available at this point even if the instruction will clobber them.  Results
   from the previous instruction are also available at this point.

   A register that holds a pointer becomes unavailable across any instruction
   that may cause a GC unless said register is also "live across" (cf. mach.mli)
   the instruction.
*)
let rec available_regs instr ~avail_before =
  (* CR mshinwell: think about trying to make this algorithm good enough so
     we can check this.  Maybe we should first measure the "badness" by
     showing those regs that are known to be live but not deduced to be
     available.
  (* A register should not be an input to an instruction unless it is
     available. *)
  assert (Reg.Set.subset (Reg.set_of_array instr.M.arg) avail_before);
  (* Every register that is live across an instruction should also be available
     before the instruction. *)
  assert (Reg.Set.subset instr.M.live instr.M.available_before);
  *)
  instr.M.available_before <-
    Reg.Set.filter (fun reg -> Reg.Raw_name.is_ident reg.Reg.raw_name)
      avail_before;
  (* available-after = (available-before \ clobbers) U results

     "clobbers" has a subtlety: it needs to include registers in
     available-before that have the same location (physical register or
     stack slot) as any registers in "results".  We deal with this by using
     [overwrite_union] instead of [Reg.Set.union] in several places.
  *)
  let without_res =
    let join branches =
      let avails = List.map branches ~f:(available_regs ~avail_before) in
      begin match avails with
      | [] -> avail_before
      | avail::avails -> List.fold_left avails ~init:avail ~f:Reg.Set.inter
      end
    in
    let open Mach in
    match instr.desc with
    | Iend -> avail_before
    | Ireturn | Iop Itailcall_ind | Iop (Itailcall_imm _) ->
      Reg.Set.empty
    | Iop Icall_ind | Iop (Icall_imm _) | Iop (Iextcall _) ->
      (* For registers holding pointers:
         All we can guarantee is that registers live across [instr] (and the
         result, which will be unioned in below) will be available after the
         call.  (The inputs to [instr] may not be, depending on what the
         callee does.)
         For registers not holding pointers:
         A register remains available so long as the procedure call convention
         does not destroy it.
         In both cases:
         The call may raise an exception, but that should not affect the
         registers available before the subsequent instruction (i.e.
         [instr.next]), since we will only arrive at [instr.next] if there
         was no exception. *)
      let non_ptr_avail =
        Reg.Set.diff (Reg.Set.filter Reg.holds_non_pointer avail_before)
          (Reg.set_of_array (Proc.destroyed_at_oper instr.desc))
      in
      (* [union] is ok to use here since the sets of registers are known not
         to have overlapping locations. *)
      (* CR mshinwell: ask Xavier if this is really true.  At the least we
         should add an assertion. *)
      Reg.Set.union instr.M.live non_ptr_avail
    | Iop (Iintop Icheckbound) | Iop (Iintop_imm (Icheckbound, _))
      (* These instructions may branch to the nearest enclosing try-with, but
         the same argument from above about exceptions applies. *)
    | Iop _ -> avail_before
    | Iifthenelse (_, ifso, ifnot) -> join [ifso; ifnot]
    | Iswitch (_, cases) -> join (Array.to_list cases)
    | Iloop body ->
      let avail_after = ref avail_before in
      begin try
        while true do
          let avail_after' = available_regs body ~avail_before:!avail_after in
          if Reg.Set.equal !avail_after avail_after' then raise Exit;
          avail_after := avail_after'
        done
      with Exit -> ()
      end;
      !avail_after
    | Icatch (_nfail, body, handler) ->
      Reg.Set.inter (available_regs body ~avail_before)
        (* CR mshinwell: actually, at the start of the handler we probably do
           know what's available (correlate with Iexit via nfail). *)
        (available_regs handler ~avail_before:Reg.Set.empty)
    | Iexit _nfail -> Reg.Set.empty
    | Itrywith (body, handler) ->
      (* Here, [Proc.loc_exn_bucket] should be available at the start of
         [handler], but it isn't clear it's useful (from the point of view of
         the debugger) to include it since there's no associated identifier
         name.
         We start with [Reg.Set.empty] for the handler since we can't tell
         from where the exception might have been raised. *)
      Reg.Set.inter (available_regs body ~avail_before)
        (available_regs handler ~avail_before:Reg.Set.empty)
    | Iraise _ -> Reg.Set.empty
  in
  let avail_after =
    overwrite_union without_res ~overwrite_with:(Reg.set_of_array instr.M.res)
  in
  match instr.M.desc with
  | M.Iend -> avail_after
  | _ ->
   let next = instr.M.next in
   let avail_before =
     (* CR mshinwell: this seems dubious.  If this algorithm were exact,
        shouldn't this be redundant? *)
     overwrite_union avail_after ~overwrite_with:(Reg.set_of_array next.M.arg)
   in
   available_regs next ~avail_before

let fundecl f =
  let fun_args = Reg.set_of_array f.M.fun_args in
  let first_instr_arg = Reg.set_of_array f.M.fun_body.M.arg in
  assert (Reg.Set.subset first_instr_arg fun_args);
  ignore ((available_regs f.M.fun_body ~avail_before:fun_args) : Reg.Set.t);
  f
