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

let rec insert_moves (insn : Mach.instruction) =
  match insn.desc with
  | Iend -> insn
  | Iop op ->
    let next = insert_moves insn.next in
    begin match op with
    | Icall_ind | Icall_imm _ ->
      let live_locations =
        List.map Reg.location (Reg.Set.elements insn.live)
      in
      let live =
        Reg.Set.union insn.live (Reg.set_of_array Proc.loc_callee_saves);
      in
      let call =
        { insn with
          next;
          live;
        }
      in
      Array.fold_left (fun next callee_save : Mach.instruction ->
          let same_location loc =
            Pervasives.compare loc (Reg.location callee_save) = 0
          in
          if List.exists same_location live_locations then
            next
          else
            { desc = Iop (Iintop Ixor);
              next;
              arg = [| callee_save |];
              res = [| callee_save |];
              dbg = call.dbg;
              live = insn.live;
            })
        call
        Proc.loc_callee_saves
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _ | Iconst_symbol _
    | Iconst_blockheader _ | Itailcall_ind | Itailcall_imm _ | Iextcall _
    | Istackoffset _ | Iload _ | Istore _ | Ialloc _ | Iintop _ | Iintop_imm _
    | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf | Ifloatofint | Iintoffloat
    | Ispecific _ -> { insn with next; }
    end
  | Ireturn | Iexit _ | Iraise _ ->
    let next = insert_moves insn.next in
    { insn with next; }
  | Iifthenelse (cond, ifso, ifnot) ->
    let next = insert_moves insn.next in
    { insn with
      desc = Iifthenelse (cond, insert_moves ifso, insert_moves ifnot);
      next;
    }
  | Iswitch (consts, arms) ->
    let next = insert_moves insn.next in
    { insn with
      desc = Iswitch (consts, Array.map insert_moves arms);
      next;
    }
  | Iloop body ->
    let next = insert_moves insn.next in
    { insn with
      desc = Iloop (insert_moves body);
      next;
    }
  | Icatch (cont, body, handler) ->
    let next = insert_moves insn.next in
    { insn with
      desc = Icatch (cont, insert_moves body, insert_moves handler);
      next;
    }
  | Itrywith (body, handler) ->
    let next = insert_moves insn.next in
    { insn with
      desc = Itrywith (insert_moves body, insert_moves handler);
      next;
    }

let fundecl (fundecl : Mach.fundecl) =
  { fundecl with
    fun_body = insert_moves fundecl.fun_body;
  }
