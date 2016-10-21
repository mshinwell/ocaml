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

(* Insertion of moves to suggest possible spilling / reloading points
   before register allocation. *)

open Reg
open Mach

(* We say that a register is "destroyed" if it is live across a construct
   that potentially destroys all physical registers: function calls or
   try...with constructs.

   The "destroyed" registers must therefore reside in the stack during
   these instructions.. We will insert spills (stores) just after they
   are defined, and reloads just before their first use following a
   "destroying" construct.

   Instructions with more live registers than actual registers also
   "destroy" registers: we mark as "destroyed" the registers live
   across the instruction that haven't been used for the longest time.
   These registers will be spilled and reloaded as described above. *)

(* Association of spill registers to registers *)

let spill_env = ref (Reg.Map.empty : Reg.t Reg.Map.t)

let spill_reg r =
  try
    Reg.Map.find r !spill_env
  with Not_found ->
    let spill_r = Reg.create r.typ in
    spill_r.spill <- true;
    if not (Reg.anonymous r) then spill_r.raw_name <- r.raw_name;
    spill_env := Reg.Map.add r spill_r !spill_env;
    spill_r

(* Record the position of last use of registers *)

let use_date = ref (Reg.Map.empty : int Reg.Map.t)
let current_date = ref 0

let record_use regv =
  for i = 0 to Array.length regv - 1 do
    let r = regv.(i) in
    let prev_date = try Reg.Map.find r !use_date with Not_found -> 0 in
    if !current_date > prev_date then
      use_date := Reg.Map.add r !current_date !use_date
  done

(* Check if the register pressure overflows the maximum pressure allowed
   at that point. If so, spill enough registers to lower the pressure. *)

let add_superpressure_regs op live_regs res_regs spilled =
  let max_pressure = Proc.max_register_pressure op in
  let regs = Reg.add_set_array live_regs res_regs in
  (* Compute the pressure in each register class *)
  let pressure = Array.make Proc.num_register_classes 0 in
  Reg.Set.iter
    (fun r ->
      if Reg.Set.mem r spilled then () else begin
        match r.loc with
          Stack _ -> ()
        | _ -> let c = Proc.register_class r in
               pressure.(c) <- pressure.(c) + 1
      end)
    regs;
  (* Check if pressure is exceeded for each class. *)
  let rec check_pressure cl spilled =
    if cl >= Proc.num_register_classes then
      spilled
    else if pressure.(cl) <= max_pressure.(cl) then
      check_pressure (cl+1) spilled
    else begin
      (* Find the least recently used, unspilled, unallocated, live register
         in the class *)
      let lru_date = ref 1000000 and lru_reg = ref Reg.dummy in
      Reg.Set.iter
        (fun r ->
          if Proc.register_class r = cl &&
             not (Reg.Set.mem r spilled) &&
             r.loc = Unknown
          then begin
            try
              let d = Reg.Map.find r !use_date in
              if d < !lru_date then begin
                lru_date := d;
                lru_reg := r
              end
            with Not_found ->                 (* Should not happen *)
              ()
          end)
        live_regs;
      if !lru_reg != Reg.dummy then begin
        pressure.(cl) <- pressure.(cl) - 1;
        check_pressure cl (Reg.Set.add !lru_reg spilled)
      end else
        (* Couldn't find any spillable register, give up for this class *)
        check_pressure (cl+1) spilled
    end in
  check_pressure 0 spilled

(* A-list recording what is destroyed at if-then-else points. *)

let destroyed_at_fork = ref ([] : (instruction * Reg.Set.t) list)

(* First pass: insert reload instructions based on an approximation of
   what is destroyed at pressure points. *)

let add_reloads regset i =
  Reg.Set.fold
    (fun r i -> instr_cons (Iop Ireload) [|spill_reg r|] [|r|] i)
    regset i

let reload_at_exit = ref []

let find_reload_at_exit k =
  try
    List.assoc k !reload_at_exit
  with
  | Not_found -> Misc.fatal_error "Spill.find_reload_at_exit"

let rec reload i before =
  incr current_date;
  record_use i.arg;
  record_use i.res;
  match i.desc with
    Iend ->
      (i, before)
  | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _) ->
      (add_reloads (Reg.inter_set_array before i.arg) i,
       Reg.Set.empty)
  | Iop(Icall_ind _ | Icall_imm _ | Iextcall { alloc = true; }) ->
      (* All regs live across must be spilled *)
      let (new_next, finally) = reload i.next i.live in
      (add_reloads (Reg.inter_set_array before i.arg)
                   (instr_cons_debug i.desc i.arg i.res i.dbg new_next),
       finally)
  | Iop op ->
      let new_before =
        (* Quick check to see if the register pressure is below the maximum *)
        if Reg.Set.cardinal i.live + Array.length i.res <=
           Proc.safe_register_pressure op
        then before
        else add_superpressure_regs op i.live i.res before in
      let after =
        Reg.diff_set_array (Reg.diff_set_array new_before i.arg) i.res in
      let (new_next, finally) = reload i.next after in
      (add_reloads (Reg.inter_set_array new_before i.arg)
                   (instr_cons_debug i.desc i.arg i.res i.dbg new_next),
       finally)
  | Iifthenelse(test, ifso, ifnot) ->
      let at_fork = Reg.diff_set_array before i.arg in
      let date_fork = !current_date in
      let (new_ifso, after_ifso) = reload ifso at_fork in
      let date_ifso = !current_date in
      current_date := date_fork;
      let (new_ifnot, after_ifnot) = reload ifnot at_fork in
      current_date := max date_ifso !current_date;
      let (new_next, finally) =
        reload i.next (Reg.Set.union after_ifso after_ifnot) in
      let new_i =
        instr_cons (Iifthenelse(test, new_ifso, new_ifnot))
        i.arg i.res new_next in
      destroyed_at_fork := (new_i, at_fork) :: !destroyed_at_fork;
      (add_reloads (Reg.inter_set_array before i.arg) new_i,
       finally)
  | Iswitch(index, cases) ->
      let at_fork = Reg.diff_set_array before i.arg in
      let date_fork = !current_date in
      let date_join = ref 0 in
      let after_cases = ref Reg.Set.empty in
      let new_cases =
        Array.map
          (fun c ->
            current_date := date_fork;
            let (new_c, after_c) = reload c at_fork in
            after_cases := Reg.Set.union !after_cases after_c;
            date_join := max !date_join !current_date;
            new_c)
          cases in
      current_date := !date_join;
      let (new_next, finally) = reload i.next !after_cases in
      (add_reloads (Reg.inter_set_array before i.arg)
                   (instr_cons (Iswitch(index, new_cases))
                               i.arg i.res new_next),
       finally)
  | Iloop(body) ->
      let date_start = !current_date in
      let destroyed_at_fork_start = !destroyed_at_fork in
      let at_head = ref before in
      let final_body = ref body in
      begin try
        while true do
          current_date := date_start;
          destroyed_at_fork := destroyed_at_fork_start;
          let (new_body, new_at_head) = reload body !at_head in
          let merged_at_head = Reg.Set.union !at_head new_at_head in
          if Reg.Set.equal merged_at_head !at_head then begin
            final_body := new_body;
            raise Exit
          end;
          at_head := merged_at_head
        done
      with Exit -> ()
      end;
      let (new_next, finally) = reload i.next Reg.Set.empty in
      (instr_cons (Iloop(!final_body)) i.arg i.res new_next,
       finally)
  | Icatch(nfail, body, handler) ->
      let new_set = ref Reg.Set.empty in
      reload_at_exit := (nfail, new_set) :: !reload_at_exit ;
      let (new_body, after_body) = reload body before in
      let at_exit = !new_set in
      reload_at_exit := List.tl !reload_at_exit ;
      let (new_handler, after_handler) = reload handler at_exit in
      let (new_next, finally) =
        reload i.next (Reg.Set.union after_body after_handler) in
      (instr_cons (Icatch(nfail, new_body, new_handler)) i.arg i.res new_next,
       finally)
  | Iexit nfail ->
      let set = find_reload_at_exit nfail in
      set := Reg.Set.union !set before;
      (i, Reg.Set.empty)
  | Iraise _ ->
      (add_reloads (Reg.inter_set_array before i.arg) i, Reg.Set.empty)

(* Second pass: add spill instructions based on what we've decided to reload.
   That is, any register that may be reloaded in the future must be spilled
   just after its definition. *)

(*
   As an optimization, if a register needs to be spilled in one branch of
   a conditional but not in the other, then we spill it late on entrance
   in the branch that needs it spilled.
   NB: This strategy is turned off in loops, as it may prevent a spill from
   being lifted up all the way out of the loop.
   NB again: This strategy is also off in switch arms
   as it generates many useless spills inside switch arms
   NB ter: is it the same thing for catch bodies ?
*)


let spill_at_exit = ref []
let find_spill_at_exit k ~for_exception_raise =
  try
    let spill = List.assoc k !spill_at_exit in
    if not for_exception_raise then
      spill
    else
      Reg.Set.remove Proc.loc_exn_bucket spill
  with
  | Not_found ->
    if for_exception_raise then
      Reg.Set.empty
    else
      Misc.fatal_error "Spill.find_spill_at_exit"

let inside_loop = ref false
and inside_arm = ref false
and inside_catch = ref false

let add_spills regset i =
  Reg.Set.fold
    (fun r i -> instr_cons (Iop Ispill) [|r|] [|spill_reg r|] i)
    regset i

let rec spill i finally exn_stack_finally =
  match i.desc with
    Iend ->
      (i, finally, exn_stack_finally)
  | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _) ->
      (i, Reg.Set.empty, exn_stack_finally)
  | Iop Ireload ->
      let (new_next, after, exn_stack) =
        spill i.next finally exn_stack_finally
      in
      let before1 = Reg.diff_set_array after i.res in
      (instr_cons i.desc i.arg i.res new_next,
       Reg.add_set_array before1 i.res,
       exn_stack)
  | Iop op ->
      let (new_next, after, exn_stack) =
        spill i.next finally exn_stack_finally
      in
      let before1 = Reg.diff_set_array after i.res in
      let before =
        match i.desc with
          Iop Icall_ind _ | Iop(Icall_imm _) | Iop(Iextcall _)
        | Iop(Iintop (Icheckbound _)) | Iop(Iintop_imm((Icheckbound _), _)) ->
            let spill_at_raise =
              match exn_stack with
              | static_exn :: _ ->
                find_spill_at_exit static_exn ~for_exception_raise:true
              | _ -> Reg.Set.empty
            in
            Reg.Set.union before1 spill_at_raise
        | _ ->
            before1 in
      let exn_stack =
        match op with
        | Ipoptrap { static_exn; } -> static_exn :: exn_stack
        | Ipushtrap { static_exn; } ->
          begin match exn_stack with
          | static_exn'::exn_stack when static_exn = static_exn' -> exn_stack
          | _ -> Misc.fatal_error "Spill exn stack is wrong"
          end
        | _ -> exn_stack
      in
      (instr_cons_debug i.desc i.arg i.res i.dbg
                  (add_spills (Reg.inter_set_array after i.res) new_next),
       before,
       exn_stack)
  | Iifthenelse(test, ifso, ifnot) ->
      let (new_next, at_join, exn_stack) =
        spill i.next finally exn_stack_finally
      in
      let (new_ifso, before_ifso, ifso_exn_stack) =
        spill ifso at_join exn_stack
      in
      let (new_ifnot, before_ifnot, ifnot_exn_stack) =
        spill ifnot at_join exn_stack in
      assert (exn_stack = ifso_exn_stack);
      assert (exn_stack = ifnot_exn_stack);
      if
        !inside_loop || !inside_arm
      then
        (instr_cons (Iifthenelse(test, new_ifso, new_ifnot))
                     i.arg i.res new_next,
         Reg.Set.union before_ifso before_ifnot,
         exn_stack)
      else begin
        let destroyed = List.assq i !destroyed_at_fork in
        let spill_ifso_branch =
          Reg.Set.diff (Reg.Set.diff before_ifso before_ifnot) destroyed
        and spill_ifnot_branch =
          Reg.Set.diff (Reg.Set.diff before_ifnot before_ifso) destroyed in
        (instr_cons
            (Iifthenelse(test, add_spills spill_ifso_branch new_ifso,
                               add_spills spill_ifnot_branch new_ifnot))
            i.arg i.res new_next,
         Reg.Set.diff (Reg.Set.diff (Reg.Set.union before_ifso before_ifnot)
                                    spill_ifso_branch)
                       spill_ifnot_branch,
         exn_stack)
      end
  | Iswitch(index, cases) ->
      let (new_next, at_join, exn_stack) =
        spill i.next finally exn_stack_finally
      in
      let saved_inside_arm = !inside_arm in
      inside_arm := true ;
      let before = ref Reg.Set.empty in
      let new_cases =
        Array.map
          (fun c ->
            let (new_c, before_c, exn_stack_c) = spill c at_join exn_stack in
            assert (exn_stack = exn_stack_c);
            before := Reg.Set.union !before before_c;
            new_c)
          cases in
      inside_arm := saved_inside_arm ;
      (instr_cons (Iswitch(index, new_cases)) i.arg i.res new_next,
       !before,
       exn_stack)
  | Iloop(body) ->
      let (new_next, _, exn_stack') = spill i.next finally exn_stack_finally in
      let saved_inside_loop = !inside_loop in
      inside_loop := true;
      let at_head = ref Reg.Set.empty in
      let final_body = ref body in
      let exn_stack_before = ref [] in
      begin try
        while true do
          let (new_body, before_body, exn_stack) =
            spill body !at_head exn_stack'
          in
          exn_stack_before := exn_stack;
          let new_at_head = Reg.Set.union !at_head before_body in
          if Reg.Set.equal new_at_head !at_head then begin
            final_body := new_body; raise Exit
          end;
          at_head := new_at_head
        done
      with Exit -> ()
      end;
      inside_loop := saved_inside_loop;
      (instr_cons (Iloop(!final_body)) i.arg i.res new_next,
       !at_head,
       !exn_stack_before)
  | Icatch(nfail, body, handler) ->
      let (new_next, at_join, exn_stack) =
        spill i.next finally exn_stack_finally
      in
      let (new_handler, at_exit, exn_stack') =
        spill handler at_join exn_stack
      in
      assert (exn_stack = exn_stack');
      let saved_inside_catch = !inside_catch in
      inside_catch := true ;
      spill_at_exit := (nfail, at_exit) :: !spill_at_exit ;
      let (new_body, before, exn_stack') = spill body at_join exn_stack in
      assert (exn_stack = exn_stack');
      spill_at_exit := List.tl !spill_at_exit;
      inside_catch := saved_inside_catch ;
      (instr_cons (Icatch(nfail, new_body, new_handler)) i.arg i.res new_next,
       before,
       exn_stack)
  | Iexit nfail ->
      (i, find_spill_at_exit nfail ~for_exception_raise:false,
        exn_stack_finally)
  | Iraise _ ->
      let spill_at_raise =
        match exn_stack_finally with
        | static_exn::_ ->
          find_spill_at_exit static_exn ~for_exception_raise:true
        | [] -> Reg.Set.empty
      in
      (i, spill_at_raise, exn_stack_finally)

(* Entry point *)

let reset () =
  spill_env := Reg.Map.empty;
  use_date := Reg.Map.empty;
  current_date := 0;
  destroyed_at_fork := []

let fundecl f =
  reset ();

  let (body1, _) = reload f.fun_body Reg.Set.empty in
  let (body2, tospill_at_entry, _) = spill body1 Reg.Set.empty [] in
  let new_body =
    add_spills (Reg.inter_set_array tospill_at_entry f.fun_args) body2 in
  spill_env := Reg.Map.empty;
  use_date := Reg.Map.empty;
  destroyed_at_fork := [];
  { fun_name = f.fun_name;
    fun_args = f.fun_args;
    fun_body = new_body;
    fun_fast = f.fun_fast;
    fun_dbg  = f.fun_dbg;
    fun_spacetime_shape = f.fun_spacetime_shape;
  }
