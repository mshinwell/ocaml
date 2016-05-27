(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module L = Linearize

let canonical_available_before insn =
  let regs_by_ident = Ident.Tbl.create 42 in
  Reg.Set.iter (fun (reg : Reg.t) ->
      match reg.name with
      | None -> ()  (* ignore registers without source-level names *)
      | Some name ->
        match Ident.Tbl.find regs_by_ident name with
        | exception Not_found -> Ident.Tbl.add regs_by_ident name reg
        | (reg' : Reg.t) ->
          (* We prefer registers that are assigned to the stack to
             preserve availability across function calls.  Other than
             that, any register is as good as any other register; likewise
             for stack slots. *)
          match reg.shared.loc, reg'.shared.loc with
          | Reg _, Stack _
          | Reg _, Reg _
          | Stack _, Stack _
          | _, Unknown
          | Unknown, _ -> ()
          | Stack _, Reg _ ->
            Ident.Tbl.remove regs_by_ident name;
            Ident.Tbl.add regs_by_ident name reg')
    insn.L.available_before;
  Ident.Tbl.fold (fun _ident reg available_before ->
      Reg.Set.add reg available_before)
    regs_by_ident
    Reg.Set.empty

let rec filter (insn : L.instruction) =
  match insn.desc with
  | Lend -> insn
  | _ ->
    insn.available_before <- canonical_available_before insn;
    filter insn.next

let fundecl (decl : L.fundecl) =
  { decl with fun_body = filter decl.fun_body; }
