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

module Debug_info = struct
  type t = {
    holds_value_of : Ident.t;
    part_of_value : int;
    num_parts_of_value : int;
    which_parameter : int option;
  }

  let compare t1 t2 =
    let c = Ident.compare t1.holds_value_of t2.holds_value_of in
    if c <> 0 then c
    else
      Pervasives.compare
        (t1.part_of_value, t1.num_parts_of_value, t1.which_parameter)
        (t2.part_of_value, t2.num_parts_of_value, t2.which_parameter)

  let holds_value_of t = t.holds_value_of
  let part_of_value t = t.part_of_value
  let num_parts_of_value t = t.num_parts_of_value
  let which_parameter t = t.which_parameter
end

module T = struct
  type t = {
    reg : Reg.t;
    debug_info : Debug_info.t option;
  }

  module Order = struct
    type t = Reg.t
    let compare r1 r2 = r1.stamp - r2.stamp
  end

  let compare t1 t2 =
    let c = Order.compare t1.reg t2.reg in
    if c <> 0 then c
    else
      match t1.debug_info, t2.debug_info with
      | None, None -> 0
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some di1, Some di2 -> Debug_info.compare di1 di2
end

include T

let create ~reg ~holds_value_of ~part_of_value ~num_parts_of_value
      ~which_parameter =
  assert (num_parts_of_value >= 1);
  assert (part_of_value >= 0 && part_of_value < num_parts_of_value);
  assert (match which_parameter with None -> true | Some index -> index >= 0);
  let debug_info : Debug_info.t =
    { holds_value_of;
      part_of_value;
      num_parts_of_value;
      which_parameter;
    }
  in
  { reg;
    debug_info = Some debug_info;
  }

let create_without_debug_info ~reg =
  { reg;
    debug_info = None;
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

module Set = struct
  include Set.Make (T)

  let forget_debug_info t =
    map (fun t -> reg t) t

  let without_debug_info regs =
    Set.map (fun reg -> create_without_debug_info ~reg) regs

  let made_unavailable_by_clobber t ~regs_clobbered =
    Set.fold (fun reg acc ->
        let made_unavailable =
          filter (fun reg' -> at_same_location reg' reg)
            avail_before
        in
        union made_unavailable acc)
      (set_of_array regs_clobbered)
      (* ~init:*)empty
end

module Set_distinguishing_names_and_locations = struct
  module T = struct
    type t = t

    let compare t1 t2 =
      let c = Ident.compare t1.holds_value_of t2.holds_value_of in
      if c <> 0 then c
      else Pervasives.compare t1.reg.loc t2.reg.loc
  end

  include Set.Make (T)
end
