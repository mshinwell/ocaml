(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module RD = Reg_with_debug_info
module V = Backend_var

type t =
  | Ok of RD.Set.t
  | Unreachable

let map t ~f =
  match t with
  | Ok t -> Ok (f t)
  | Unreachable -> Unreachable

let inter regs1 regs2 =
  match regs1, regs2 with
  | Unreachable, _ -> regs2
  | _, Unreachable -> regs1
  | Ok avail1, Ok avail2 ->
    (* CR mshinwell: Is this function not symmetric? *)
    let result =
      RD.Set.fold (fun reg1 result ->
          match RD.Set.find_reg_exn avail2 (RD.reg reg1) with
          | exception Not_found -> result
          | reg2 ->
            let debug_info1 = RD.debug_info reg1 in
            let debug_info2 = RD.debug_info reg2 in
            let debug_info =
              match debug_info1, debug_info2 with
              | None, None -> None
              (* Example for this next case: the value of a mutable variable x
                 is copied into another variable y; then there is a conditional
                 where on one branch x is assigned and on the other branch it
                 is not.  This means that on the former branch we have
                 forgotten about y holding the value of x; but we have not on
                 the latter.  At the join point we must have forgotten the
                 information. *)
              | None, Some _ | Some _, None -> None
              | Some debug_info1, Some debug_info2 ->
                if RD.Debug_info.compare debug_info1 debug_info2 = 0 then
                  Some debug_info1
                else
                  None
            in
            let reg =
              RD.create_with_debug_info ~reg:(RD.reg reg1)
                ~debug_info
            in
            RD.Set.add reg result)
        avail1
        RD.Set.empty
    in
    Ok result

let equal t1 t2 =
  match t1, t2 with
  | Unreachable, Unreachable -> true
  | Unreachable, Ok _ | Ok _, Unreachable -> false
  | Ok regs1, Ok regs2 -> RD.Set.equal regs1 regs2

let subset t1 t2 =
  equal (inter t1 t2) t1

let find_reg_opt t reg =
  match t with
  | Unreachable -> None
  | Ok rd_set ->
    match RD.Set.find_reg_exn rd_set reg with
    | exception Not_found -> None
    | rd -> Some rd

let find_all_holding_value_of t holds_value_of =
  match t with
  | Unreachable -> []
  | Ok rd_set ->
    let rd_set =
      RD.Set.filter (fun rd ->
          match RD.debug_info rd with
          | None -> false
          | Some debug_info ->
            RD.Holds_value_of.equal (RD.Debug_info.holds_value_of debug_info)
              holds_value_of)
        rd_set
    in
    RD.Set.elements rd_set

let canonicalise availability =
  match availability with
  | Unreachable -> Unreachable
  | Ok availability ->
    let regs_by_var = V.Tbl.create 42 in
    RD.Set.iter (fun reg ->
        match RD.debug_info reg with
        | None -> ()
        | Some debug_info ->
          match RD.Debug_info.holds_value_of debug_info with
          | Var name ->
            begin match V.Tbl.find regs_by_var name with
            | exception Not_found -> V.Tbl.add regs_by_var name reg
            | (reg' : RD.t) ->
              (* We prefer registers that are assigned to the stack since
                 they probably give longer available ranges (less likely to
                 be clobbered). *)
              match RD.location reg, RD.location reg' with
              | Reg _, Stack _
              | Reg _, Reg _
              | Stack _, Stack _
              | _, Unknown
              | Unknown, _ -> ()
              | Stack _, Reg _ ->
                V.Tbl.remove regs_by_var name;
                V.Tbl.add regs_by_var name reg
            end
          | Const_int _ | Const_naked_float _ | Const_symbol _ -> ())
      availability;
    let result =
      V.Tbl.fold (fun _var reg availability ->
          RD.Set.add reg availability)
        regs_by_var
        RD.Set.empty
    in
    if !Clflags.dwarf_invariant_checks then begin
      assert (RD.Set.subset result availability)
    end;
    Ok result

let print ~print_reg ppf = function
  | Unreachable -> Format.fprintf ppf "<unreachable>"
  | Ok availability ->
    Format.fprintf ppf "{%a}"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
        (Reg_with_debug_info.print ~print_reg))
      (RD.Set.elements availability)
