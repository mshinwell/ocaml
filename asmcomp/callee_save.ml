(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Reg_by_location = struct
  module T = struct
    type t = Reg.t
    let compare (t1 : t) (t2 : t) = Pervasives.compare t1.loc t2.loc
  end

  include T
  module Set = Set.Make (T)

  let of_set (set : Reg.Set.t) =
    Reg.Set.fold (fun reg t ->
        Set.add reg t)
      set
      Set.empty

  let set_of_array (set : Reg.t array) : Set.t =
    Array.fold_left (fun t reg ->
        Set.add reg t)
      Set.empty
      set
end

module RL = Reg_by_location

let place_moves regs ~around =
  RL.Set.fold (fun reg (next : Mach.instruction) : Mach.instruction ->
      { desc = Iop (Iintop Ixor);
        next;
        arg = [| reg; reg |];
        res = [| reg |];
        dbg = next.dbg;
        live = next.live;
      })
    regs
    around

let rec insert_moves (insn : Mach.instruction) =
  match insn.desc with
  | Iend -> insn, RL.Set.empty
  | Iop op ->
    let next, pending = insert_moves insn.next in
    let destroyed =
      (* [under_try] is always [false] here because, even if we are under a
         [try], we still need to initialise callee-save registers as that's
         what the callee expects.  The difference is that we will have forced
         those registers to spill in the caller. *)
      RL.set_of_array (Proc.destroyed_at_oper ~under_try:false insn.desc)
    in
    let must_place_here = RL.Set.union destroyed (RL.set_of_array insn.res) in
    let place_here = RL.Set.inter pending must_place_here in
    let pending = RL.Set.diff pending place_here in
    let next = place_moves place_here ~around:next in
    let pending =
      match op with
      | Icall_ind | Icall_imm _ ->
        let live_before =
          RL.Set.union (RL.set_of_array insn.arg) (RL.of_set insn.live)
        in
        let callee_saves = RL.set_of_array Proc.loc_callee_saves in
        let need_initialising = RL.Set.diff callee_saves live_before in
        RL.Set.union pending need_initialising
      | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _
      | Iconst_symbol _ | Iconst_blockheader _ | Itailcall_ind
      | Itailcall_imm _ | Iextcall _ | Istackoffset _ | Iload _ | Istore _
      | Ialloc _ | Iintop _ | Iintop_imm _ | Inegf | Iabsf | Iaddf | Isubf
      | Imulf | Idivf | Ifloatofint | Iintoffloat
      | Ispecific _ -> pending
    in
    let pending = RL.Set.diff pending (RL.set_of_array insn.arg) in
    { insn with next; }, pending
  | Ireturn | Iexit _ | Iraise _ ->
    let next = insert_moves_starting_over insn.next in
    { insn with next; }, RL.Set.empty
  | Iifthenelse (cond, ifso, ifnot) ->
    let next = insert_moves_starting_over insn.next in
    let ifso, ifso_pending = insert_moves ifso in
    let ifnot, ifnot_pending = insert_moves ifnot in
    let pending = RL.Set.inter ifso_pending ifnot_pending in
    let place_at_top_of_ifso = RL.Set.diff ifso_pending pending in
    let place_at_top_of_ifnot = RL.Set.diff ifnot_pending pending in
    let ifso = place_moves place_at_top_of_ifso ~around:ifso in
    let ifnot = place_moves place_at_top_of_ifnot ~around:ifnot in
    let insn =
      { insn with
        desc = Iifthenelse (cond, ifso, ifnot);
        next;
      }
    in
    insn, pending
  | Iswitch (consts, arms) ->
    let next = insert_moves_starting_over insn.next in
    let arms = Array.map insert_moves arms in
    let pending =
      Array.fold_left (fun pending (_arm, arm_pending) ->
          match pending with
          | None -> Some arm_pending
          | Some pending -> Some (RL.Set.inter arm_pending pending))
        None
        arms
    in
    let pending =
      match pending with
      | None -> RL.Set.empty
      | Some pending -> pending
    in
    let arms =
      Array.map (fun (arm, arm_pending) ->
          let pending = RL.Set.diff arm_pending pending in
          place_moves pending ~around:arm)
        arms
    in
    let insn =
      { insn with
        desc = Iswitch (consts, arms);
        next;
      }
    in
    insn, pending
  | Iloop body ->
    let next = insert_moves_starting_over insn.next in
    let insn =
      { insn with
        desc = Iloop (insert_moves_starting_over body);
        next;
      }
    in
    insn, RL.Set.empty
  | Icatch (cont, body, handler) ->
    let next = insert_moves_starting_over insn.next in
    let body = insert_moves_starting_over body in
    let handler = insert_moves_starting_over handler in
    let insn =
      { insn with
        desc = Icatch (cont, body, handler);
        next;
      }
    in
    insn, RL.Set.empty
  | Itrywith (body, handler) ->
    let next = insert_moves_starting_over insn.next in
    let body = insert_moves_starting_over body in
    let handler = insert_moves_starting_over handler in
    let insn =
      { insn with
        desc = Itrywith (body, handler);
        next;
      }
    in
    insn, RL.Set.empty

and insert_moves_starting_over insn =
  let insn, pending = insert_moves insn in
  place_moves pending ~around:insn

let fundecl (fundecl : Mach.fundecl) =
  { fundecl with
    fun_body = insert_moves_starting_over fundecl.fun_body;
  }
