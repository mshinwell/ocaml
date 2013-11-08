(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*         Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
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

let remove_regs_that_share_locations ~to_remove ~from =
  Reg.Set.filter
    (fun reg ->
      let has_same_location reg' = Reg.same_location reg reg' in
      not (Reg.Set.exists has_same_location to_remove))
    from

(* [available_regs ~instr ~currently_available] returns the registers
   that are available after [instr].  As a side effect the function
   updates the [available_before] members of all instructions in the
   sequence [instr] to indicate the registers available immediately
   prior to each instruction. *)
let rec available_regs ~instr ~currently_available =
(*  assert (Reg.Set.subset (Reg.set_of_array instr.Mach.arg) currently_available); *)
  instr.Mach.available_before <- currently_available;
  let destroyed = Reg.set_of_array (Proc.destroyed_at_oper instr.Mach.desc) in
  let available_after_children =
    (* CR mshinwell: should we remove "destroyed" here? *)
    match instr.Mach.desc with
    | Mach.Iexit _ | Mach.Iraise | Mach.Iend | Mach.Ireturn -> currently_available
    | Mach.Iop _op -> currently_available
    | Mach.Iifthenelse (_test, if_true, if_false) ->
      (* [inter] should be correct here: a register should only appear in the result
         if it appears (with the same stamp, not just the same location) in both of
         the input sets. *)
      Reg.Set.inter
        (available_regs ~instr:if_true ~currently_available)
        (available_regs ~instr:if_false ~currently_available)
    | Mach.Iswitch (_index, cases) ->
      let avail_cases =
        List.map (Array.to_list cases)
          ~f:(fun case -> available_regs ~instr:case ~currently_available)
      in
      begin match avail_cases with
      | [] -> currently_available
      | avail_case1::avail_cases ->
        (* Same comment as above regarding [inter]. *)
        List.fold_left avail_cases ~init:avail_case1 ~f:Reg.Set.inter
      end
    | Mach.Iloop body ->
      (* CR trefis for mshinwell: why [Reg.all_registers_set] instead of
         [currently_available] here? *)
      let available_after = ref (Reg.all_registers_set ()) in
      begin try
        while true do
          let new_available_after =
            available_regs ~instr:body ~currently_available:!available_after
          in
          if Reg.Set.equal !available_after new_available_after then raise Exit;
          available_after := new_available_after
        done
      with Exit -> ()
      end;
      !available_after
    | Mach.Icatch (_, i1, i2)
    | Mach.Itrywith (i1, i2) ->
      (* [inter] works for the same reason as in the [Iifthenelse] case. *)
      Reg.Set.inter
        (available_regs ~instr:i1 ~currently_available)
        (available_regs ~instr:i2 ~currently_available)
  in
  let available_after =
    let without_destroyed =
      remove_regs_that_share_locations ~to_remove:destroyed
        ~from:available_after_children
    in
    let result_regs = Reg.set_of_array instr.Mach.res in
    let without_result_regs_and_destroyed =
      (* This ensures that any existing registers with the same locations as result
         registers of [instr] are removed (before we union in those result registers).
         For example:
           {R/0[%rax] R/1[%rbx]}
           accu/29[%rax] := R/0[%rax]
           {R/1[%rbx] accu/29[%rax]*}   <-- R/0[%rax] has been removed
      *)
      remove_regs_that_share_locations ~to_remove:result_regs
        ~from:without_destroyed
    in
    Reg.Set.union result_regs without_result_regs_and_destroyed
  in
  match instr.Mach.desc with
  | Mach.Iend | Mach.Ireturn | Mach.Iexit _ -> available_after  (* CR mshinwell: check *)
  | _ -> (* CR mshinwell: filter regs out that have no locations? *)
    available_regs ~instr:instr.Mach.next ~currently_available:available_after

let function_declaration fundecl =
  let _available_after = 
    available_regs ~instr:fundecl.Mach.fun_body
      ~currently_available:(Reg.set_of_array fundecl.Mach.fun_args)
  in
  ()
