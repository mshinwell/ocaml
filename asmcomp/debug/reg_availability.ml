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

module RD = Reg_with_debug_info

type t =
  | Ok of RD.Set.t
  | Unreachable

let inter regs1 regs2 =
  match regs1, regs2 with
  | Unreachable, _ -> regs2
  | _, Unreachable -> regs1
  | Ok avail1, Ok avail2 ->
    Ok (RD.Set.inter avail1 avail2)

let canonicalise availability =
  match availability with
  | Unreachable -> Unreachable
  | Ok availability ->
    let regs_by_ident = Ident.Tbl.create 42 in
    RD.Set.iter (fun reg ->
        match RD.debug_info reg with
        | None -> ()
        | Some debug_info ->
          let name = RD.Debug_info.holds_value_of debug_info in
          if not (Ident.persistent name) then begin
            match Ident.Tbl.find regs_by_ident name with
            | exception Not_found -> Ident.Tbl.add regs_by_ident name reg
            | (reg' : RD.t) ->
              (* We prefer registers that are assigned to the stack to
                preserve availability across function calls.  Other than
                that, any register is as good as any other register; likewise
                for stack slots. *)
              (* CR mshinwell: think about this again.  Does it matter what's
                chosen here?  May be better to choose "Reg" as the dwarf is
                probably smaller *)
              match RD.location reg, RD.location reg' with
              | Reg _, Stack _
              | Reg _, Reg _
              | Stack _, Stack _
              | _, Unknown
              | Unknown, _ -> ()
              | Stack _, Reg _ ->
                Ident.Tbl.remove regs_by_ident name;
                Ident.Tbl.add regs_by_ident name reg
          end)
      availability;
    let result =
      Ident.Tbl.fold (fun _ident reg availability ->
          RD.Set.add reg availability)
        regs_by_ident
        RD.Set.empty
    in
    Ok result
