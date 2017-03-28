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

let place_initialisers regs ~around =
  Reg.Set.fold (fun reg (next : Mach.instruction) : Mach.instruction ->
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
  | Iend -> insn, Reg.Set.empty
  | Iop op ->
    let next, pending = insert_moves insn.next in
    let pending =
      match op with
      | Icall_ind | Icall_imm _ ->
        let live_locations =
          List.map Reg.location (Reg.Set.elements insn.live)
        in
        Array.fold_left (fun pending callee_save ->
            let same_location loc =
              Pervasives.compare loc (Reg.location callee_save) = 0
            in
            if List.exists same_location live_locations then pending
            else Reg.Set.add callee_save pending)
          pending
          Proc.loc_callee_saves
      | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _
      | Iconst_symbol _ | Iconst_blockheader _ | Itailcall_ind
      | Itailcall_imm _ | Iextcall _ | Istackoffset _ | Iload _ | Istore _
      | Ialloc _ | Iintop _ | Iintop_imm _ | Inegf | Iabsf | Iaddf | Isubf
      | Imulf | Idivf | Ifloatofint | Iintoffloat
      | Ispecific _ -> pending
    in
    let destroyed = Reg.set_of_array (Proc.destroyed_at_oper insn.desc) in
    assert (Reg.Set.is_empty (
      Reg.Set.inter destroyed (Reg.set_of_array Proc.loc_callee_saves)));
    let must_place_here =
      Reg.Set.union (Reg.set_of_array insn.arg) (Reg.set_of_array insn.res)
    in
    let place_here = Reg.Set.inter pending must_place_here in
    let insn = place_initialisers place_here ~around:insn in
    let pending = Reg.Set.diff pending place_here in
    { insn with next; }, pending
  | Ireturn | Iexit _ | Iraise _ ->
    let next = insert_moves_starting_over insn.next in
    { insn with next; }, Reg.Set.empty
  | Iifthenelse (cond, ifso, ifnot) ->
    let next = insert_moves_starting_over insn.next in
    let ifso, ifso_pending = insert_moves ifso in
    let ifnot, ifnot_pending = insert_moves ifnot in
    let pending = Reg.Set.inter ifso_pending ifnot_pending in
    let place_at_top_of_ifso = Reg.Set.diff ifso_pending pending in
    let place_at_top_of_ifnot = Reg.Set.diff ifnot_pending pending in
    let ifso = place_initialisers place_at_top_of_ifso ~around:ifso in
    let ifnot = place_initialisers place_at_top_of_ifnot ~around:ifnot in
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
          | Some pending -> Some (Reg.Set.inter arm_pending pending))
        None
        arms
    in
    let pending =
      match pending with
      | None -> Reg.Set.empty
      | Some pending -> pending
    in
    let arms =
      Array.map (fun (arm, arm_pending) ->
          let pending = Reg.Set.diff arm_pending pending in
          place_initialisers pending ~around:arm)
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
    insn, Reg.Set.empty
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
    insn, Reg.Set.empty
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
    insn, Reg.Set.empty

and insert_moves_starting_over insn =
  let insn, pending = insert_moves insn in
  place_initialisers pending ~around:insn

let fundecl (fundecl : Mach.fundecl) =
  let fun_body, pending = insert_moves fundecl.fun_body in
  { fundecl with
    fun_body = place_initialisers pending ~around:fun_body;
  }
