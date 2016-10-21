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

(* Liveness analysis.
   Annotate mach code with the set of regs live at each point. *)

open Mach

let live_at_exit = ref []

let find_live_at_exit k ~for_exception_raise =
  try
    let live = List.assoc k !live_at_exit in
    if not for_exception_raise then
      live
    else
      Reg.Set.remove Proc.loc_exn_bucket live
  with
  | Not_found ->
    if for_exception_raise then
      Reg.Set.empty
    else
      Misc.fatal_error "Liveness.find_live_at_exit"

let rec live i finally exn_stack_finally =
  (* finally is the set of registers live after execution of the
     instruction sequence.
     exn_stack_finally is the stack of installed (dynamic) exception traps
     (referenced by static exception IDs) after the instruction sequence.
     The result of the function is the set of registers live just
     before the instruction sequence together with the exception stack at
     that point.
     The instruction i is annotated by the set of registers live across
     the instruction. *)
  let arg =
    if Config.spacetime
      && Mach.spacetime_node_hole_pointer_is_live_before i
    then Array.append i.arg [| Proc.loc_spacetime_node_hole |]
    else i.arg
  in
  match i.desc with
    Iend ->
      i.live <- finally;
      finally, exn_stack_finally
  | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _) ->
      let _, exn_stack = live i.next finally exn_stack_finally in
      i.live <- Reg.Set.empty; (* no regs are live across *)
      Reg.set_of_array arg, exn_stack
  | Iop op ->
      let after, exn_stack = live i.next finally exn_stack_finally in
      if Proc.op_is_pure op                    (* no side effects *)
      && Reg.disjoint_set_array after i.res    (* results are not used after *)
      && not (Proc.regs_are_volatile arg)      (* no stack-like hard reg *)
      && not (Proc.regs_are_volatile i.res)    (*            is involved *)
      then begin
        (* This operation is dead code.  Ignore its arguments. *)
        i.live <- after;
        after, exn_stack
      end else begin
        let across_after = Reg.diff_set_array after i.res in
        let across =
          match op with
          | Icall_ind _ | Icall_imm _ | Iextcall _
          | Iintop (Icheckbound _) | Iintop_imm(Icheckbound _, _) ->
              (* The function call may raise an exception, branching to the
                 nearest enclosing try ... with. Similarly for bounds checks.
                 Hence, everything that must be live at the beginning of
                 the exception handler (minus the exception bucket register)
                 must also be live across this instr. *)
              let live_at_raise =
                match exn_stack with
                | static_exn :: _ ->
                  find_live_at_exit static_exn ~for_exception_raise:true
                | _ -> Reg.Set.empty
              in
              Reg.Set.union across_after live_at_raise
           | _ ->
               across_after in
        i.live <- across;
        let exn_stack =
          match op with
          | Ipoptrap { static_exn; } ->
            static_exn :: exn_stack
          | Ipushtrap { static_exn; } ->
            begin match exn_stack with
            | static_exn'::exn_stack when static_exn = static_exn' -> exn_stack
            | static_exn'::_ ->
              Misc.fatal_errorf "Liveness analysis exn stack is wrong (\
                  expected static exn %d on the top but we have %d"
                static_exn static_exn'
            | [] ->
              Misc.fatal_errorf "Liveness analysis exn stack is empty (\
                  expected static exn %d on the top)"
                static_exn
            end
          | _ -> exn_stack
        in
        Reg.add_set_array across arg, exn_stack
      end
  | Iifthenelse(_test, ifso, ifnot) ->
      let at_join, exn_stack = live i.next finally exn_stack_finally in
      let ifso_live, ifso_exn_stack = live ifso at_join [] in
      let ifnot_live, ifnot_exn_stack = live ifnot at_join [] in
      assert (ifso_exn_stack = []);
      assert (ifnot_exn_stack = []);
      let at_fork = Reg.Set.union ifso_live ifnot_live in
      i.live <- at_fork;
      Reg.add_set_array at_fork arg, exn_stack
  | Iswitch(_index, cases) ->
      let at_join, exn_stack = live i.next finally exn_stack_finally in
      let at_fork = ref Reg.Set.empty in
      for i = 0 to Array.length cases - 1 do
        let case_live, case_exn_stack = live cases.(i) at_join [] in
        assert (case_exn_stack = []);
        at_fork := Reg.Set.union !at_fork case_live
      done;
      i.live <- !at_fork;
      Reg.add_set_array !at_fork arg, exn_stack
  | Iloop(body) ->
      let _, exn_stack = live i.next finally exn_stack_finally in
      let at_top = ref Reg.Set.empty in
      (* Yes, there are better algorithms, but we'll just iterate till
         reaching a fixpoint. *)
      begin try
        while true do
          let live, exn_stack = live body !at_top [] in
          assert (exn_stack = []);
          let new_at_top = Reg.Set.union !at_top live in
          if Reg.Set.equal !at_top new_at_top then raise Exit;
          at_top := new_at_top;
        done
      with Exit -> ()
      end;
      i.live <- !at_top;
      !at_top, exn_stack
  | Icatch(nfail, body, handler) ->
      let at_join, exn_stack = live i.next finally exn_stack_finally in
      let before_handler, exn_stack' = live handler at_join [] in
      assert (exn_stack' = []);
      let before_body =
          live_at_exit := (nfail,before_handler) :: !live_at_exit ;
          let before_body, exn_stack' = live body at_join [] in
          assert (exn_stack' = []);
          live_at_exit := List.tl !live_at_exit ;
          before_body in
      i.live <- before_body;
      before_body, exn_stack
  | Iexit nfail ->
      let _, exn_stack = live i.next finally exn_stack_finally in
      let this_live = find_live_at_exit nfail ~for_exception_raise:false in
      i.live <- this_live ;
      this_live, exn_stack
  | Iraise _ ->
      let _, exn_stack = live i.next finally exn_stack_finally in
      let this_live =
        match exn_stack with
        | static_exn::_ ->
          find_live_at_exit static_exn ~for_exception_raise:true
        | [] -> Reg.Set.empty
      in
      i.live <- this_live;
      Reg.add_set_array this_live arg, exn_stack

let reset () =
  live_at_exit := []

let fundecl ppf f =
  let initially_live, initial_exn_stack = live f.fun_body Reg.Set.empty [] in
  (* Sanity check: only function parameters (and the Spacetime node hole
     register, if profiling) can be live at entrypoint *)
  let wrong_live = Reg.Set.diff initially_live (Reg.set_of_array f.fun_args) in
  let wrong_live =
    if not Config.spacetime then wrong_live
    else Reg.Set.remove Proc.loc_spacetime_node_hole wrong_live
  in
  if not (Reg.Set.is_empty wrong_live) then begin
    Format.fprintf ppf "%a@." Printmach.regset wrong_live;
    Misc.fatal_error "Liveness.fundecl"
  end;
  if initial_exn_stack <> [] then begin
    Misc.fatal_error "Mismatched trap handler operations during liveness pass"
  end
