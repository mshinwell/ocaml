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

(* [overwrite_union reg_set ~overwrite_with] removes from [reg_set] all
   registers having the same location as [overwrite_with]; and then adds
   [overwrite_with] into the resulting set. *)
let overwrite_union reg_set ~overwrite_with =
  R.Set.fold (fun reg acc ->
    match reg.R.loc with
    | R.Unknown ->
      (* This pass is always run after register allocation. *)
      failwith "overwrite_union: register has no location"
    | R.Reg _ | R.Stack _ ->
      let not_sharing_loc =
        R.Set.filter (fun reg' -> reg.R.loc <> reg'.R.loc) acc
      in
      R.Set.add reg not_sharing_loc) overwrite_with (* ~init:*)reg_set

(* Filter out registers that are not tagged with a value identifier name; and
   in the case where multiple registers exist for a given value identifier,
   choose a canonical one (see comment below). *)
let filter_avail_before avail_before =
  R.Set.fold (fun reg acc ->
    if not (R.Raw_name.is_ident reg.R.raw_name) then acc
    (* CR-soon mshinwell: handle values split across multiple registers *)
    else if reg.R.part <> None then acc
    else
      let for_same_ident =
        R.Set.filter (fun reg' ->
          R.Raw_name.(=) reg.R.raw_name reg'.R.raw_name) acc
      in
      match R.Set.elements for_same_ident with
      | [] -> R.Set.add reg acc
      | [reg'] ->
        let priority reg =
          match reg.R.loc with
          | R.Unknown ->
            failwith "filter_avail_before: register has no location"
          | R.Reg _ -> 0
          | R.Stack _ -> 1
        in
        if priority reg' >= priority reg then acc
        else R.Set.add reg (R.Set.remove reg' acc)
      | _ -> assert false) avail_before (* ~init:*)R.Set.empty

let destroyed_at_c_call = Array.to_list Proc.destroyed_at_c_call

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
     [overwrite_union] instead of [R.Set.union] in several places.
  *)
  let without_res =
    let join branches =
      let avails = List.map branches ~f:(available_regs ~avail_before) in
      begin match avails with
      | [] -> avail_before
      | avail::avails -> List.fold_left avails ~init:avail ~f:R.Set.inter
      end
    in
    let open Mach in
    match instr.desc with
    | Iend -> avail_before
    | Ireturn | Iop Itailcall_ind | Iop (Itailcall_imm _) ->
      R.Set.empty
    | Iop (Iextcall false) ->
      (* C call that does not allocate.
         - Callee-save hard registers as per the target platform's ABI are
           preserved, irrespective of their contents (since no GC may occur
           during the call).
         - Pseudos assigned to the stack are preserved, again, irrespective
           of their contents.
      *)
      R.Set.filter (fun reg ->
          match reg.R.location with
          | R.Stack -> true
          | R.Reg reg_num -> not (List.mem reg_num destroyed_at_c_call)
          | R.Unknown -> failwith "R.Unknown in Iextcall false case"
        ) avail_before
    | Iop Icall_ind | Iop (Icall_imm _) | Iop (Iextcall true) ->
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
        R.Set.diff (R.Set.filter R.holds_non_pointer avail_before)
          (R.set_of_array (Proc.destroyed_at_oper instr.desc))
      in
      (* [union] is ok to use here since the sets of registers are known not
         to have overlapping locations. *)
      (* CR mshinwell: ask Xavier if this is really true.  At the least we
         should add an assertion. *)
      R.Set.union instr.M.live non_ptr_avail
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
          if R.Set.equal !avail_after avail_after' then raise Exit;
          avail_after := avail_after'
        done
      with Exit -> ()
      end;
      !avail_after
    | Icatch (_nfail, body, handler) ->
      R.Set.inter (available_regs body ~avail_before)
        (* CR mshinwell: actually, at the start of the handler we probably do
           know what's available (correlate with Iexit via nfail). *)
        (available_regs handler ~avail_before:R.Set.empty)
    | Iexit _nfail -> R.Set.empty
    | Itrywith (body, handler) ->
      (* Here, [Proc.loc_exn_bucket] should be available at the start of
         [handler], but it isn't clear it's useful (from the point of view of
         the debugger) to include it since there's no associated identifier
         name.
         We start with [R.Set.empty] for the handler since we can't tell
         from where the exception might have been raised. *)
      (* CR-someday mshinwell: there is one case where we might be able to:
         if the body doesn't contain any calls, but does contain bounds
         checks. *)
      R.Set.inter (available_regs body ~avail_before)
        (available_regs handler ~avail_before:R.Set.empty)
    | Iraise _ -> R.Set.empty
  in
  let avail_after =
    overwrite_union without_res ~overwrite_with:(R.set_of_array instr.M.res)
  in
  match instr.M.desc with
  | M.Iend -> avail_after
  | _ ->
   let next = instr.M.next in
   let avail_before =
     (* CR mshinwell: this seems dubious.  If this algorithm were exact,
        shouldn't this be redundant? *)
     overwrite_union avail_after ~overwrite_with:(R.set_of_array next.M.arg)
   in
   available_regs next ~avail_before

let fundecl f =
  let fun_args = R.set_of_array f.M.fun_args in
  let first_instr_arg = R.set_of_array f.M.fun_body.M.arg in
  assert (R.Set.subset first_instr_arg fun_args);
  let avail_before = filter_avail_before fun_args in
  ignore ((available_regs f.M.fun_body ~avail_before) : R.Set.t);
  f
