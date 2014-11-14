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

let fun_name = ref ""

let avail_at_exit_table = Hashtbl.create 42
let avail_at_raise = ref None

let reg_is_interesting reg =
  (* CR-soon mshinwell: handle values split across multiple registers *)
  if reg.R.part <> None then false
  else
    match R.Raw_name.to_ident reg.R.raw_name with
    | None -> false
    | Some ident -> Ident.name ident <> Closure.env_param_name

let instr_arg i =
  R.Set.filter reg_is_interesting (R.set_of_array i.M.arg)

let instr_res i =
  R.Set.filter reg_is_interesting (R.set_of_array i.M.res)

let instr_live i =
  R.Set.filter reg_is_interesting i.M.live

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
        R.Set.filter (fun reg' ->
          reg.R.loc <> reg'.R.loc
            || (not (R.is_temporary reg')
                  && reg.R.raw_name <> reg'.R.raw_name)) acc
      in
      R.Set.add reg not_sharing_loc) overwrite_with (* ~init:*)reg_set

let should_ignore_reg _reg = false
(*
  match R.Raw_name.to_ident reg.R.raw_name with
  | None -> true
  | Some ident ->
    Ident.name ident = Closure.env_param_name
      (* CR-soon mshinwell: handle values split across multiple registers *)
      || reg.R.part <> None
*)

(* Filter out registers that are not tagged with a value identifier name and
   for identifier names (e.g. for the closure parameters of mutually-recursive
   functions) that should be hidden in the debugger; and in the case where
   multiple registers exist for a given value identifier, choose a canonical
   one (see comment below). *)
(*let filter_avail_before avail_before =
  R.Set.filter (fun reg -> not (should_ignore_reg reg)) avail_before*)
(*
  R.Set.fold (fun reg acc ->
    if should_ignore_reg reg then
      acc
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
*)

(* Form the largest subset of the array [regs] that contains entirely
   registers that we will not ignore during this analysis. *)
(*
let not_ignored_set_of_array regs =
  R.Set.filter (fun reg -> not (should_ignore_reg reg)) (R.set_of_array regs)
*)

(* A special sentinel value meaning "all registers available"---used when a
   point in the code is unreachable. *)
let all_regs = R.set_of_array [| R.dummy |]

(* Intersection, taking into account the special meaning of [all_regs]. *)
let inter regs1 regs2 =
  if regs1 == all_regs then regs2
  else if regs2 == all_regs then regs1
  else R.Set.inter regs1 regs2

let operation_can_raise = function
  | M.Icall_ind | M.Icall_imm _ | M.Iextcall _
  | M.Iintop M.Icheckbound | M.Iintop_imm (M.Icheckbound, _) -> true
  | _ -> false

let augment_availability_at_raise avail =
  match !avail_at_raise with
  | None -> avail_at_raise := Some avail
  | Some avail' -> avail_at_raise := Some (inter avail avail')

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
  instr.M.available_before <- avail_before;
  (* A register should not be an input to an instruction unless it is
     available. *)
  if not (R.Set.subset (instr_arg instr) avail_before) then begin
    Format.fprintf Format.err_formatter "Non-available reg(s) in %s:\n"
      !fun_name;
    Printmach.instr Format.err_formatter instr;
    Format.fprintf Format.err_formatter "\navail_before:\n";
    Printmach.regset Format.err_formatter avail_before;
    Format.fprintf Format.err_formatter "\ninstruction's argument:\n";
    Printmach.regset Format.err_formatter (R.set_of_array instr.M.arg);
    Format.fprintf Format.err_formatter "\n";
(*    assert false*)
  end;
  (* Every register that is live across an instruction should also be available
     before the instruction. *)
  if instr.M.desc <> M.Iend && not (R.Set.subset (R.Set.filter reg_is_interesting instr.M.live) avail_before) then begin
    Format.fprintf Format.err_formatter "Regs live but not available in %s:\n"
      !fun_name;
    Printmach.instr Format.err_formatter instr;
    Format.fprintf Format.err_formatter "\nlive:\n";
    Printmach.regset Format.err_formatter instr.M.live;
    Format.fprintf Format.err_formatter "\navail_before:\n";
    Printmach.regset Format.err_formatter avail_before;
    Format.fprintf Format.err_formatter "\n";
(*    assert false*)
  end;
  let avail_after =
    let join branches =
      let avails = List.map branches ~f:(available_regs ~avail_before) in
      begin match avails with
      | [] -> avail_before
      | avail::avails -> List.fold_left avails ~init:avail ~f:inter
      end
    in
    let open Mach in
    match instr.desc with
    | Iend -> avail_before
    | Ireturn | Iop Itailcall_ind | Iop (Itailcall_imm _) -> all_regs
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
             (b) they are live across the call.
             The "not destroyed by the relevant calling convention" part is
             handled by the removal of registers in [destroyed_at_oper], below.
          *)
          let non_ptr_avail = R.Set.filter R.holds_non_pointer avail_before in
          R.Set.union (instr_live instr) non_ptr_avail
        | _ -> avail_before
      in
      let avail_after =
        (* Make unavailable any registers that the operation destroys. *)
        R.Set.diff avail_after
          (R.set_of_array (Proc.destroyed_at_oper instr.desc))
      in
      assert (R.Set.for_all reg_is_interesting avail_after);
      let results = instr_res instr in
      (* Calculate which registers will have been made unavailable by the
         writing out of the results from the instruction. *)
      let made_unavailable_by_results =
        R.Set.fold (fun reg acc ->
          (* [reg] is a result of the operation holding the value of some
             named identifier.  Any other register in the available-before set
             holding the value of that identifier, in the same location, will
             be replaced in the availability-after set by this register;
             however, any other registers in the same location but identifying
             _different_ identifiers are passed into the availability-after set
             untouched.  This means in particular that moves from one named
             value to another both in the same location (which would seem
             unnecessary but are currently generated) are recorded correctly. *)
          let reg_ident = R.Raw_name.to_ident_exn reg.R.raw_name in
          let made_unavailable =
            R.Set.filter (fun reg' ->
              (* Note that we do not compare the stamps here. *)
              Ident.same reg_ident (R.Raw_name.to_ident_exn reg'.R.raw_name)
                && reg.R.loc = reg'.R.loc)
              avail_after
          in
          R.Set.union made_unavailable acc)
          results (* ~init:*)R.Set.empty
      in
      R.Set.union results (R.Set.diff avail_after made_unavailable_by_results)
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
    | Icatch (nfail, body, handler) ->
      let avail_after_body = available_regs body ~avail_before in
      let avail_at_exit =
        try Hashtbl.find avail_at_exit_table nfail
        with Not_found ->
          Misc.fatal_error (
            Printf.sprintf "Available_regs: avail_at_exit: %s, nfail=%d"
              !fun_name nfail)
      in
      Hashtbl.remove avail_at_exit_table nfail;
      let avail_after_handler =
        assert (not (reg_is_interesting Proc.loc_exn_bucket));
        available_regs handler ~avail_before:avail_at_exit
      in
      inter avail_after_body avail_after_handler
    | Iexit nfail ->
      begin try
        Hashtbl.replace avail_at_exit_table nfail
          (inter (Hashtbl.find avail_at_exit_table nfail) avail_before)
      with Not_found ->
        Hashtbl.add avail_at_exit_table nfail avail_before
      end;
      all_regs
    | Itrywith (body, handler) ->
      let saved_avail_at_raise = !avail_at_raise in
      let after_body = available_regs body ~avail_before in
      let avail_before_handler =
        match !avail_at_raise with
        | None -> R.Set.empty
        | Some avail -> avail
      in
      avail_at_raise := saved_avail_at_raise;
      let after_handler =
        available_regs handler ~avail_before:avail_before_handler
      in
      inter after_body after_handler
    | Iraise _ ->
      augment_availability_at_raise avail_before;
      all_regs
  in
  match instr.M.desc with
  | M.Iend -> avail_after
  | _ -> available_regs instr.M.next ~avail_before:avail_after

let fundecl f =
  Hashtbl.clear avail_at_exit_table;
  avail_at_raise := None;
  fun_name := f.M.fun_name;
  let fun_args = R.set_of_array f.M.fun_args in
  let first_instr_arg = R.set_of_array f.M.fun_body.M.arg in
  assert (R.Set.subset first_instr_arg fun_args);
  let avail_before = R.Set.filter reg_is_interesting fun_args in
  ignore ((available_regs f.M.fun_body ~avail_before) : R.Set.t);
  f
