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

let instr_arg_array i =
  Array.of_list (R.Set.elements (instr_arg i))

let instr_res i =
  R.Set.filter reg_is_interesting (R.set_of_array i.M.res)

let instr_res_array i =
  Array.of_list (R.Set.elements (instr_res i))

let instr_live i =
  R.Set.filter reg_is_interesting i.M.live

(* A special sentinel value meaning "all registers available"---used when a
   point in the code is unreachable. *)
let all_regs = R.set_of_array [| R.dummy |]

(* Intersection, taking into account the special meaning of [all_regs]. *)
let inter regs1 regs2 =
  if regs1 == all_regs then regs2
  else if regs2 == all_regs then regs1
  else R.Set.inter regs1 regs2

let regs_have_same_location reg1 reg2 =
  (* We need to check the register classes too: two locations both saying "stack offset
     N" might actually be different physical locations, for example if one is of
     class "Int" and another "Float" on amd64. *)
  reg1.R.loc = reg2.R.loc
    && Proc.register_class reg1 = Proc.register_class reg2

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
   after [instr].  This is a forwards dataflow analysis.

   "available before" can be thought of, at the assembly level, as the set of
   registers available when the program counter is equal to the address of the
   particular instruction under consideration (that is to say, immediately
   prior to the instruction being executed).  Inputs to that instruction are
   available at this point even if the instruction will clobber them.  Results
   from the previous instruction are also available at this point.

   The [available_before] field of each instruction is updated by this
   function to the subset of the [avail_before] argument consisting of those
   registers that are tagged with identifier names, and are not related to special
   parameters such as environments for mutually-recursive closures.  (The rationale is
   that these are the registers we may be interested in referencing, by name, when
   debugging.)
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
    (* Detect initializing moves between named registers, including when either the
       source or destination is a spill slot or reload target. *)
    | Iop (Imove | Ispill | Ireload)
        when begin match instr_arg_array instr, instr_res_array instr with
          | [| arg |], [| res |] (* when regs_have_same_location arg res *) ->
            let arg_name = arg.R.raw_name in
            let res_name = res.R.raw_name in
            begin match R.Raw_name.to_ident arg_name, R.Raw_name.to_ident res_name with
            | Some _ident1, Some _ident2 (*when Ident.same ident1 ident2*) -> true
            | _ -> false
            end
          | _ -> false
          end ->
      R.Set.union avail_before (instr_res instr)
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
         writing out of the results from the instruction.  Thanks to the special case
         above for moves, this is straightforward. *)
      let made_unavailable_by_results =
        R.Set.fold (fun reg acc ->
          let made_unavailable =
            R.Set.filter (regs_have_same_location reg) avail_after
          in
          R.Set.union made_unavailable acc)
          results (* ~init:*)R.Set.empty
      in
      let avail_after =
        R.Set.union results (R.Set.diff avail_after made_unavailable_by_results)
      in
      avail_after
    | Iifthenelse (_, ifso, ifnot) -> join [ifso; ifnot]
    | Iswitch (_, cases) -> join (Array.to_list cases)
    | Iloop body ->
      let avail_after = ref avail_before in
      begin try
        while true do
          let avail_after' =
            inter !avail_after (available_regs body ~avail_before:!avail_after)
          in
          if R.Set.equal !avail_after avail_after' then raise Exit;
          avail_after := avail_after'
        done
      with Exit -> ()
      end;
      all_regs
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
      avail_at_raise := None;
      let after_body = available_regs body ~avail_before in
      let avail_before_handler =
        match !avail_at_raise with
        | None -> R.Set.empty
        | Some avail -> avail
      in
      avail_at_raise := saved_avail_at_raise;
      let after_handler =
        assert (not (reg_is_interesting Proc.loc_exn_bucket));
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
