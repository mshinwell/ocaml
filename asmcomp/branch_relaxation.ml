(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                   Mark Shinwell, Jane Street Europe                 *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Linearize

module Make (T : sig
  type cond_branch

  val instr_size : Linearize.instruction_desc -> int
  val classify_instr : Linearize.instruction -> cond_branch option
  val max_displacement_in_words : cond_branch -> int
  val code_for_far_allocation : num_words:int -> Linearize.instruction_desc
end) = struct
  let label_map code =
    let map = Hashtbl.create 37 in
    let rec fill_map pc instr =
      match instr.desc with
      | Lend -> (pc, map)
      | Llabel lbl -> Hashtbl.add map lbl pc; fill_map pc instr.next
      | op -> fill_map (pc + T.instr_size op) instr.next
    in fill_map 0 code

  let branch_overflows map pc_branch lbl_dest max_branch_offset =
    let pc_dest = Hashtbl.find map lbl_dest in
    let delta = pc_dest - (pc_branch + 1) in
    delta <= -max_branch_offset || delta >= max_branch_offset

  let opt_branch_overflows branch map pc_branch opt_lbl_dest
        max_branch_offset =
    match opt_lbl_dest with
    | None -> false
    | Some lbl_dest ->
      branch_overflows map pc_branch lbl_dest max_branch_offset

  let instr_overflows codesize instr map pc =
    match T.classify_instr instr with
    | None -> false
    | Some branch ->
      let max_branch_offset = T.max_displacement_in_words branch in
      match instr.desc with
      | Lop (Ialloc _)
      | Lop (Iintop Icheckbound)
      | Lop (Iintop_imm (Icheckbound, _))
      | Lop (Ispecific (Ishiftcheckbound _)) ->
        codesize - pc >= max_branch_offset
      | Lcondbranch (_, lbl) ->
        branch_overflows map pc lbl max_branch_offset
      | Lcondbranch3 (lbl0, lbl1, lbl2) ->
        opt_branch_overflows map pc lbl0 max_branch_offset
          || opt_branch_overflows map pc lbl1 max_branch_offset
          || opt_branch_overflows map pc lbl2 max_branch_offset
      | _ ->
        Misc.fatal_error "Unsupported instruction for branch relaxation"

  let fixup_branches codesize map code =
    let expand_optbranch lbl n arg next =
      match lbl with
      | None -> next
      | Some l ->
        instr_cons (Lcondbranch (Iinttest_imm (Isigned Ceq, n), l))
          arg [||] next
    in
    let rec fixup did_fix pc instr =
      if not (instr_overflows codesize instr map codesize pc) then
        fixup did_fix (pc + T.instr_size op) instr.next
      else
        match instr.desc with
        | Lend -> did_fix
        | Lop (Ialloc num_words) ->
          instr.desc <- T.code_for_far_allocation ~num_words;
          fixup true (pc + T.instr_size instr.desc) instr.next
        | Lop (Iintop Icheckbound) ->
          assert false
        | Lop (Iintop_imm (Icheckbound, _)) ->
          assert false
        | Lop (Ispecific (Ishiftcheckbound _)) ->
          assert false
        | Lcondbranch (test, lbl) ->
          let old_size = T.instr_size instr.desc in
          let lbl2 = new_label() in
          let new_desc0 = Lbranch lbl in
          let new_desc1 = Llabel lbl2 in
          let cont =
            instr_cons new_desc0 [||] [||]
              (instr_cons new_desc1 [||] [||] instr.next) in
          instr.desc <- Lcondbranch (invert_test test, lbl2);
          instr.next <- cont;
          let new_size =
            T.instr_size instr.desc + T.instr_size new_desc0
              + T.instr_size new_desc1
          in
          fixup true (pc - old_size + new_size) instr.next
        | Lcondbranch3 (lbl0, lbl1, lbl2) ->
          let cont =
            expand_optbranch lbl0 0 instr.arg
              (expand_optbranch lbl1 1 instr.arg
                (expand_optbranch lbl2 2 instr.arg instr.next)) in
          instr.desc <- cont.desc;
          instr.next <- cont.next;
          fixup true pc instr
        | _ ->
          Misc.fatal_error "Unsupported instruction for branch relaxation"
    in
    fixup false 0 code

  (* Iterate branch expansion till all conditional branches are OK *)

  let rec fixup_branches code =
    let (codesize, map) = label_map code in
    if codesize >= max_branch_offset && fixup_branches codesize map code
    then branch_normalization code
    else ()
end
