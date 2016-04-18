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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module List = ListLabels
module M = Mach
module R = Reg

let fun_name = ref ""

let avail_at_exit_table = Hashtbl.create 42
let avail_at_raise = ref None

let reg_is_interesting reg =
  (* CR-soon mshinwell: handle values split across multiple registers (and
     below) *)
  if reg.R.part <> None then false
  else
    match R.Raw_name.to_ident reg.R.raw_name with
    | None -> false
    | Some _ident -> true

let instr_arg i = R.Set.filter reg_is_interesting (R.set_of_array i.M.arg)
let instr_res i = R.Set.filter reg_is_interesting (R.set_of_array i.M.res)
let instr_live i = R.Set.filter reg_is_interesting i.M.live

(* A special sentinel value meaning "all registers available"---used when a
   point in the code is unreachable. *)
let all_regs = R.set_of_array [| R.dummy |]

(* Intersection, taking into account the special meaning of [all_regs]. *)
let inter regs1 regs2 =
  if regs1 == all_regs then regs2
  else if regs2 == all_regs then regs1
  else R.Set.inter regs1 regs2

let regs_have_same_location reg1 reg2 =
  (* We need to check the register classes too: two locations both saying
     "stack offset N" might actually be different physical locations, for
     example if one is of class "Int" and another "Float" on amd64. *)
  reg1.R.loc = reg2.R.loc
    && Proc.register_class reg1 = Proc.register_class reg2

let operation_can_raise = function
  | M.Icall_ind | M.Icall_imm _ | M.Iextcall _
  | M.Iintop M.Icheckbound | M.Iintop_imm (M.Icheckbound, _)
  | M.Ialloc _ -> true
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
   registers that are tagged with identifier names.
   (The rationale is that these are the registers we may be interested in
   referencing, by name, when debugging.)
*)
let rec available_regs instr ~avail_before =
  instr.M.available_before <- avail_before;
  let avail_after =
    if avail_before == all_regs then
      all_regs  (* This instruction is unreachable. *)
    else begin
      (* A register should not be an input to an instruction unless it is
         available. *)
      assert (R.Set.subset (instr_arg instr) avail_before);
      (* Every register that is live across an instruction should also be
         available before the instruction. *)
      assert (R.Set.subset (instr_live instr) avail_before);
      let open Mach in
      match instr.desc with
      | Iend -> avail_before
      | Ireturn | Iop Itailcall_ind | Iop (Itailcall_imm _) -> all_regs
      (* Detect initializing moves between named registers, including when
         either the source or destination is a spill slot or reload target. *)
      | Iop (Imove | Ispill | Ireload)
          when begin match instr.arg, instr.res with
          | [| arg |], [| res |] ->
            (* We need both [arg] and [res] to be named with identifiers.
               However, this is implied by them satisfying
               [reg_is_interesting]. *)
            reg_is_interesting arg && reg_is_interesting res
          | _ -> false
          end ->
        R.Set.diff (R.Set.union avail_before (R.set_of_array instr.res))
          (R.set_of_array
            (Proc.destroyed_at_oper instr.desc))  (* just in case *)
      | Iop op ->
        if operation_can_raise op then begin
          augment_availability_at_raise avail_before
        end;
        let candidate_avail_after =
          match op with
          | Icall_ind | Icall_imm _ | Iextcall _ ->
            (* Registers become unavailable across a call unless either:
               (a) they hold immediates and are not destroyed by the relevant
                   calling convention; or
               (b) they are live across the call.
               The "not destroyed by the relevant calling convention" part is
               handled by the removal of registers in [destroyed_at_oper],
               below.
            *)
            R.Set.union (instr_live instr)
              (R.Set.filter R.holds_non_pointer avail_before)
          | _ -> avail_before
        in
        let results = instr_res instr in
        let made_unavailable =
          (* Registers are made unavailable (possibly in addition to the
             above) by:
             1. the operation being marked as one that destroys them;
             2. being clobbered by the instruction writing out results.  The
                special case for moves above keeps the following code
                straightforward. *)
          R.Set.fold (fun reg acc ->
            let made_unavailable =
              R.Set.filter (regs_have_same_location reg) candidate_avail_after
            in
            R.Set.union made_unavailable acc)
            results
            (* ~init:*)(R.set_of_array (Proc.destroyed_at_oper instr.desc))
        in
        R.Set.union results (R.Set.diff candidate_avail_after made_unavailable)
      | Iifthenelse (_, ifso, ifnot) -> join [ifso; ifnot] ~avail_before
      | Iswitch (_, cases) -> join (Array.to_list cases) ~avail_before
      | Iloop body ->
        let avail_after = ref avail_before in
        begin try
          while true do
            let avail_after' =
              inter !avail_after
                (available_regs body ~avail_before:!avail_after)
            in
            if R.Set.equal !avail_after avail_after' then raise Exit;
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
        all_regs
      | Itrywith (body, handler) ->
        let saved_avail_at_raise = !avail_at_raise in
        avail_at_raise := None;
        let after_body = available_regs body ~avail_before in
        let avail_before_handler =
          match !avail_at_raise with
          | None -> all_regs  (* The handler is unreachable. *)
          | Some avail -> avail
        in
        avail_at_raise := saved_avail_at_raise;
        assert (not (reg_is_interesting Proc.loc_exn_bucket));
        inter after_body
          (available_regs handler ~avail_before:avail_before_handler)
      | Iraise _ ->
        augment_availability_at_raise avail_before;
        all_regs
      | Iphantom_let_start _ | Iphantom_let_end _ ->
        avail_before
    end
  in
  match instr.M.desc with
  | M.Iend -> avail_after
  | _ -> available_regs instr.M.next ~avail_before:avail_after

and join branches ~avail_before =
  let avails = List.map branches ~f:(available_regs ~avail_before) in
  begin match avails with
  | [] -> avail_before
  | avail::avails -> List.fold_left avails ~init:avail ~f:inter
  end

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
