(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module V = Backend_var

module Debug_info = struct
  type t = {
    holds_value_of : V.t;
    part_of_value : int;
    num_parts_of_value : int;
    which_parameter : int option;
    provenance : Backend_var.Provenance.t option;
  }

  let compare t1 t2 =
    let c = V.compare t1.holds_value_of t2.holds_value_of in
    if c <> 0 then c
    else
      Stdlib.compare
        (t1.part_of_value, t1.num_parts_of_value, t1.which_parameter)
        (t2.part_of_value, t2.num_parts_of_value, t2.which_parameter)

  let holds_value_of t = t.holds_value_of
  let part_of_value t = t.part_of_value
  let num_parts_of_value t = t.num_parts_of_value
  let which_parameter t = t.which_parameter
  let provenance t = t.provenance

  let print ppf t =
    Format.fprintf ppf "%a" V.print t.holds_value_of;
    begin match t.provenance with
    | None -> ()
    | Some provenance ->
      let dbg = Backend_var.Provenance.debuginfo provenance in
      let block =
        Debuginfo.Current_block.to_block (Debuginfo.innermost_block dbg)
      in
      match block with
      | Toplevel -> Format.fprintf ppf "[toplevel]"
      | Block block -> Format.fprintf ppf "[%a]" Debuginfo.Block.print_id block
    end;
    if not (t.part_of_value = 0 && t.num_parts_of_value = 1) then begin
      Format.fprintf ppf "(%d/%d)" t.part_of_value t.num_parts_of_value
    end;
    begin match t.which_parameter with
    | None -> ()
    | Some index -> Format.fprintf ppf "[P%d]" index
    end
end

module type T = sig
  type t
  type reg_with_debug_info = t

  val create
     : reg:Reg.t
    -> holds_value_of:Backend_var.t
    -> part_of_value:int
    -> num_parts_of_value:int
    -> which_parameter:int option
    -> provenance:Backend_var.Provenance.t option
    -> t
  val create_with_debug_info : reg:Reg.t -> debug_info:Debug_info.t option -> t
  val create_without_debug_info : reg:Reg.t -> t
  val create_copying_debug_info : reg:Reg.t -> debug_info_from:t -> t
  val reg : t -> Reg.t
  val location : t -> Reg.location
  val debug_info : t -> Debug_info.t option
  val at_same_location : t -> Reg.t -> register_class:(Reg.t -> int) -> bool
  val holds_pointer : t -> bool
  val holds_non_pointer : t -> bool
  val assigned_to_stack : t -> bool
  val clear_debug_info : t -> t
end

module T = struct
  type t = {
    reg : Reg.t;
    debug_info : Debug_info.t option;
  }

  type reg_with_debug_info = t

  module Order = struct
    type t = Reg.t
    let compare (t1 : t) (t2 : t) = t1.stamp - t2.stamp
  end

  let compare t1 t2 =
    Order.compare t1.reg t2.reg

  let create ~reg ~holds_value_of ~part_of_value ~num_parts_of_value
        ~which_parameter ~provenance =
    assert (num_parts_of_value >= 1);
    assert (part_of_value >= 0 && part_of_value < num_parts_of_value);
    assert (match which_parameter with None -> true | Some index -> index >= 0);
    let debug_info : Debug_info.t =
      { holds_value_of;
        part_of_value;
        num_parts_of_value;
        which_parameter;
        provenance;
      }
    in
    { reg;
      debug_info = Some debug_info;
    }

  let create_with_debug_info ~reg ~debug_info =
    { reg;
      debug_info;
    }

  let create_without_debug_info ~reg =
    { reg;
      debug_info = None;
    }

  let create_copying_debug_info ~reg ~debug_info_from =
    { reg;
      debug_info = debug_info_from.debug_info;
    }

  let reg t = t.reg
  let location t = t.reg.loc

  let holds_pointer t =
    match t.reg.typ with
    | Addr | Val -> true
    | Int | Float -> false

  let holds_non_pointer t = not (holds_pointer t)

  let assigned_to_stack t =
    match t.reg.loc with
    | Stack _ -> true
    | Reg _ | Unknown -> false

  let regs_at_same_location (reg1 : Reg.t) (reg2 : Reg.t) ~register_class =
    (* We need to check the register classes too: two locations both saying
       "stack offset N" might actually be different physical locations, for
       example if one is of class "Int" and another "Float" on amd64.
       [register_class] will be [Proc.register_class], but cannot be here,
       due to a circular dependency. *)
    reg1.loc = reg2.loc
      && register_class reg1 = register_class reg2

  let at_same_location t (reg : Reg.t) ~register_class =
    regs_at_same_location t.reg reg ~register_class

  let debug_info t = t.debug_info

  let clear_debug_info t =
    { t with debug_info = None; }
end

include T

module Order_distinguishing_names_and_locations = struct
  type nonrec t = t

  let compare t1 t2 =
    match t1.debug_info, t2.debug_info with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some di1, Some di2 ->
      let c = V.compare di1.holds_value_of di2.holds_value_of in
      if c <> 0 then c
      else Stdlib.compare t1.reg.loc t2.reg.loc
end

module Distinguishing_names_and_locations = struct
  include T

  module Set = Set.Make (Order_distinguishing_names_and_locations)
  module Map = Map.Make (Order_distinguishing_names_and_locations)
end

module Set = struct
  include Set.Make (T)

  let of_array elts =
    of_list (Array.to_list elts)

  let forget_debug_info t =
    fold (fun t acc -> Reg.Set.add (reg t) acc) t Reg.Set.empty

  let without_debug_info regs =
    Reg.Set.fold (fun reg acc -> add (create_without_debug_info ~reg) acc)
      regs
      empty

  let made_unavailable_by_clobber t ~regs_clobbered ~register_class =
    Reg.Set.fold (fun reg acc ->
        let made_unavailable =
          filter (fun reg' ->
              regs_at_same_location reg'.reg reg ~register_class)
            t
        in
        union made_unavailable acc)
      (Reg.set_of_array regs_clobbered)
      (* ~init:*)empty

  let mem_reg t (reg : Reg.t) =
    exists (fun t -> t.reg.stamp = reg.stamp) t

  let filter_reg t (reg : Reg.t) =
    filter (fun t -> t.reg.stamp <> reg.stamp) t

  (* CR-someday mshinwell: Well, it looks like we should have used a map.
     mshinwell: Also see @chambart's suggestion on GPR#856. *)
  let find_reg_exn t (reg : Reg.t) =
    match elements (filter (fun t -> t.reg.stamp = reg.stamp) t) with
    | [] -> raise Not_found
    | [reg] -> reg
    | _ -> assert false
end

let print ~print_reg ppf t =
  match t.debug_info with
  | None -> Format.fprintf ppf "%a" print_reg t.reg
  | Some debug_info ->
    Format.fprintf ppf "%a(%a)" print_reg t.reg Debug_info.print debug_info
