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
module R = Reg
module RA = Reg_with_availability

(* [overwrite_union reg_set ~overwrite_with] removes from [reg_set] all
   registers having the same location as [overwrite_with]; and then adds
   [overwrite_with] into the resulting set. *)
let overwrite_union reg_set ~overwrite_with =
  RA.Set.fold overwrite_with ~init:overwrite_with ~f:(fun ra acc ->
    match (RA.reg ra).R.loc with
    | R.Unknown ->
      (* This pass is always run after register allocation. *)
      failwith "overwrite_union: register has no location"
    | R.Reg _ | R.Stack _ ->
      let not_sharing_loc =
        RA.Set.filter acc
          ~f:(fun ra' -> (RA.reg ra).R.loc <> (RA.reg ra').R.loc)
      in
      RA.Set.add ra not_sharing_loc)

(* Filter out registers that are not tagged with a value identifier name; and
   in the case where multiple registers exist for a given value identifier,
   choose a canonical one (see comment below). *)
let filter_avail_before avail_before =
  RA.Set.fold avail_before ~init:RA.Set.empty ~f:(fun ra acc ->
    let reg = RA.reg ra in
    if not (R.Raw_name.is_ident reg.R.raw_name) then acc
    (* CR-soon mshinwell: handle values split across multiple registers *)
    else if reg.R.part <> None then acc
    else
      let for_same_ident =
        RA.Set.filter acc ~f:(fun ra' ->
          R.Raw_name.(=) reg.R.raw_name (RA.reg ra').R.raw_name)
      in
      match RA.Set.to_list for_same_ident with
      | [] -> RA.Set.add ra acc
      | [ra'] ->
        let priority ra =
          match (RA.reg ra).R.loc with
          | R.Unknown ->
            failwith "filter_avail_before: register has no location"
          | R.Reg _ -> 0
          | R.Stack _ -> 1
        in
        if priority ra' >= priority ra then acc
        else RA.Set.add ra (RA.Set.remove ra' acc)
      | _ -> assert false)

(* [available_regs ~instr ~avail_before] calculates, given the registers
   "available before" an instruction [instr], the registers that are available
   after [instr].

   "available before" can be thought of, at the assembly level, as the set of
   registers available when the program counter is equal to the address of the
   particular instruction under consideration (that is to say, immediately
   prior to the instruction being executed).  Inputs to that instruction are
   available at this point even if the instruction will clobber them.  Results
   from the previous instruction are also available at this point.

   A register that holds a pointer becomes unavailable across any instruction
   that may cause a GC unless said register is also "live across" (cf. mach.mli)
   the instruction.

   The [available_before] field of each instruction is updated by this
   function to the subset of the [avail_before] argument consisting of those
   registers that are tagged with identifier names.  (The rationale is that
   these are the registers we may be interested in referencing by name when
   debugging.)  In any such subset there will be at most one register for any
   given name; preference is given to spilled registers since they are less
   likely to become dead during the lifetime of a function activation.
*)
let rec available_regs instr ~avail_before =
  (* CR mshinwell: think about trying to make this algorithm good enough so
     we can check this.  Maybe we should first measure the "badness" by
     showing those regs that are known to be live but not deduced to be
     available.
  (* A register should not be an input to an instruction unless it is
     available. *)
  assert (R.Set.subset (R.set_of_array instr.M.arg) avail_before);
  (* Every register that is live across an instruction should also be available
     before the instruction. *)
  assert (R.Set.subset instr.M.live instr.M.available_before);
  *)
  instr.M.available_before <- filter_avail_before avail_before;
  (* available-after = (available-before \ clobbers) U results

     "clobbers" has a subtlety: it needs to include registers in
     available-before that have the same location (physical register or
     stack slot) as any registers in "results".  We deal with this by using
     [overwrite_union] instead of [RA.Set.union] in several places.
  *)
  let without_res =
    let join branches =
      let avails = List.map branches ~f:(available_regs ~avail_before) in
      begin match avails with
      | [] -> avail_before
      | avail::avails -> List.fold_left avails ~init:avail ~f:RA.Set.inter
      end
    in
    let open Mach in
    let is_destroyed_at_oper reg =
      R.Set.exists (fun reg' -> reg.R.loc = reg'.R.loc)
        (R.set_of_array (Proc.destroyed_at_oper instr.desc))
    in
    match instr.desc with
    | Iend -> avail_before
    | Ireturn | Iop Itailcall_ind | Iop (Itailcall_imm _) ->
      RA.Set.empty
    | Iop (Iextcall (_, false)) ->
      (* C call that does not allocate.
         - Callee-save hard registers as per the target platform's ABI are
           preserved, irrespective of their contents (since no GC may occur
           during the call).
         - Pseudos assigned to the stack are preserved, again, irrespective
           of their contents. *)
      RA.Set.filter avail_before ~f:(fun ra ->
          match (RA.reg ra).R.loc with
          | R.Stack _ -> true
          | R.Reg _ -> not (is_destroyed_at_oper (RA.reg ra))
          | R.Unknown -> failwith "R.Unknown in Iextcall false case")
    | Iop Icall_ind | Iop (Icall_imm _) | Iop (Iextcall (_, true)) ->
      (* For registers that might hold pointers:
         1. If they are live across the instruction, their availability
            after the call is the same as before the call.
         2. If they are not live across the instruction, then:
            (a) if the register is destroyed by the relevant procedure call
                convention (either OCaml or C), it becomes unavailable after
                the call;
            (b) otherwise (typically something in a stack slot), the
                register has its availability degraded to "maybe available".
                A register that becomes "maybe" can never become "definitely"
                available (see reg_with_availability.mli).  The debugger will
                determine whether the value of a "maybe available" register or
                stack slot is suitable for printing.
         For registers that never hold pointers:
         3. A register's availability remains unchanged so long as the
            relevant procedure call convention (either OCaml or C, depending
            on [instr]) does not destroy it.  If it is destroyed, it becomes
            definitely unavailable.
         Any call may raise an exception, but that should not affect the
         registers available before the subsequent instruction (i.e.
         [instr.next]), since we will only arrive at [instr.next] if there
         was no exception. *)
      let non_ptr, maybe_ptr =
        RA.Set.partition avail_before ~f:RA.definitely_holds_non_ptr
      in
      (* [union] is ok to use here since the sets of registers are known not
         to have overlapping locations. *)
      (* CR mshinwell: check this assumption.  Using [overwrite_union]
         anyway for the moment *)
      overwrite_union
        (RA.Set.filter_and_change_confidence maybe_ptr ~f:(fun ra ->
            let reg = RA.reg ra in
            if R.Set.mem reg instr.M.live then `Unchanged       (* case 1 *)
            else if is_destroyed_at_oper reg then `Remove       (* case 2(a) *)
            else `Degrade                                       (* case 2(b) *)
          ))
        (RA.Set.filter_and_change_confidence non_ptr ~f:(fun ra -> (* case 3 *)
            if is_destroyed_at_oper (RA.reg ra) then `Remove
            else `Unchanged
          ))
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
          if RA.Set.equal !avail_after avail_after' then raise Exit;
          avail_after := avail_after'
        done
      with Exit -> ()
      end;
      !avail_after
    | Icatch (_nfail, body, handler) ->
      RA.Set.inter (available_regs body ~avail_before)
        (* CR mshinwell: actually, at the start of the handler we probably do
           know what's available (correlate with Iexit via nfail). *)
        (available_regs handler ~avail_before:RA.Set.empty)
    | Iexit _nfail -> RA.Set.empty
    | Itrywith (body, handler) ->
      (* Here, [Proc.loc_exn_bucket] should be available at the start of
         [handler], but it isn't clear it's useful (from the point of view of
         the debugger) to include it since there's no associated identifier
         name.
         We start with [RA.Set.empty] for the handler since we can't tell
         from where the exception might have been raised. *)
      (* CR-someday mshinwell: there is one case where we might be able to:
         if the body doesn't contain any calls, but does contain bounds
         checks. *)
      RA.Set.inter (available_regs body ~avail_before)
        (available_regs handler ~avail_before:RA.Set.empty)
    | Iraise _ -> RA.Set.empty
  in
  let avail_after =
    overwrite_union without_res ~overwrite_with:(RA.Set.of_array instr.M.res)
  in
  match instr.M.desc with
  | M.Iend -> avail_after
  | _ ->
   let next = instr.M.next in
   let avail_before =
     (* CR mshinwell: this seems dubious.  If this algorithm were exact,
        shouldn't this be redundant? *)
     overwrite_union avail_after ~overwrite_with:(RA.Set.of_array next.M.arg)
   in
   available_regs next ~avail_before

let fundecl f =
  let fun_args = RA.Set.of_array f.M.fun_args in
  let first_instr_arg = RA.Set.of_array f.M.fun_body.M.arg in
  assert (RA.Set.subset first_instr_arg fun_args);
  let avail_before = filter_avail_before fun_args in
  ignore ((available_regs f.M.fun_body ~avail_before) : RA.Set.t);
  f
