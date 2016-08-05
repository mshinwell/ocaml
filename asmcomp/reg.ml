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

open Cmm

type t = {
  mutability : Cmm.mutability;
  stamp: int;
  mutable typ: Cmm.machtype_component;
  mutable loc: location;
  mutable spill: bool;
  mutable part: int option;
  mutable is_parameter: int option;
  mutable interf: shared list;
  mutable prefer: (shared * int) list;
  mutable degree: int;
  mutable spill_cost: int;
  mutable visited: bool;
}

and location =
    Unknown
  | Reg of int
  | Stack of stack_location

and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int

type reg = t

let dummy =
  let shared =
    { mutability = Cmm.Immutable; is_parameter = None;
      stamp = 0; typ = Int; loc = Unknown;
      spill = false; interf = []; prefer = []; degree = 0; spill_cost = 0;
      visited = false; part = None;
    }
  in
  { name = None;
    propagate_name_to = None;
    shared;
    dummy = (fun () -> ());
  }

let currstamp = ref 0
let reg_list = ref([] : shared list)

let create ?(mutability = Cmm.Immutable) ty =
  let shared =
    { mutability; stamp = !currstamp; typ = ty; is_parameter = None;
      loc = Unknown; spill = false; interf = []; prefer = []; degree = 0;
      spill_cost = 0; visited = false; part = None; } in
  reg_list := shared :: !reg_list;
  incr currstamp;
  { name = None;
    propagate_name_to = None;
    shared;
    dummy = (fun () -> ());
  }

let createv tyv =
  let n = Array.length tyv in
  let rv = Array.make n dummy in
  for i = 0 to n-1 do rv.(i) <- create tyv.(i) done;
  rv

let createv_like ?mutability rv =
  let n = Array.length rv in
  let rv' = Array.make n dummy in
  for i = 0 to n-1 do rv'.(i) <- create ?mutability rv.(i).shared.typ done;
  rv'

let clone r =
  let nr = create r.shared.typ ~mutability:r.shared.mutability in
  nr.name <- r.name;
  (* CR mshinwell: It doesn't look like this is taking effect.  Also, are
     reloads going through here? *)
  nr.shared.is_parameter <- r.shared.is_parameter;
  nr

(* The name of registers created in [Proc]. *)
let proc_reg_name = Ident.create_persistent "R"

let at_location ty loc =
  let shared = { mutability = Cmm.Immutable; is_parameter = None;
            stamp = !currstamp; typ = ty; loc;
            spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; part = None; } in
  incr currstamp;
  { name = Some proc_reg_name;
    propagate_name_to = None;
    shared;
    dummy = (fun () -> ());
  }

let immutable t =
  match t.shared.mutability with
  | Immutable -> true
  | Mutable -> false

let name t =
  match t.name with
  | None -> ""
  | Some ident ->
    let name = Ident.unique_name ident in
    let with_spilled =
      if t.shared.spill then
        "spilled-" ^ name
      else
        name
    in
    match t.shared.part with
    | None -> with_spilled
    | Some part -> with_spilled ^ "#" ^ string_of_int part

let first_virtual_reg_stamp = ref (-1)

let reset() =
  (* When reset() is called for the first time, the current stamp reflects
     all hard pseudo-registers that have been allocated by Proc, so
     remember it and use it as the base stamp for allocating
     soft pseudo-registers *)
  if !first_virtual_reg_stamp = -1 then first_virtual_reg_stamp := !currstamp;
  currstamp := !first_virtual_reg_stamp;
  reg_list := []

let all_registers() = !reg_list
let num_registers() = !currstamp

let reinit_reg shared =
  shared.loc <- Unknown;
  shared.interf <- [];
  shared.prefer <- [];
  shared.degree <- 0;
  (* Preserve the very high spill costs introduced by the reloading pass *)
  if shared.spill_cost >= 100000
  then shared.spill_cost <- 100000
  else shared.spill_cost <- 0

let reinit() =
  List.iter reinit_reg !reg_list

module RegOrder =
  struct
    type t = reg
    let compare r1 r2 = r1.shared.stamp - r2.shared.stamp
  end

module Set = Set.Make(RegOrder)
module Map = Map.Make(RegOrder)

let add_set_array s v =
  match Array.length v with
    0 -> s
  | 1 -> Set.add v.(0) s
  | n -> let rec add_all i =
           if i >= n then s else Set.add v.(i) (add_all(i+1))
         in add_all 0

let diff_set_array s v =
  match Array.length v with
    0 -> s
  | 1 -> Set.remove v.(0) s
  | n -> let rec remove_all i =
           if i >= n then s else Set.remove v.(i) (remove_all(i+1))
         in remove_all 0

let inter_set_array s v =
  match Array.length v with
    0 -> Set.empty
  | 1 -> if Set.mem v.(0) s
         then Set.add v.(0) Set.empty
         else Set.empty
  | n -> let rec inter_all i =
           if i >= n then Set.empty
           else if Set.mem v.(i) s then Set.add v.(i) (inter_all(i+1))
           else inter_all(i+1)
         in inter_all 0

let disjoint_set_array s v =
  match Array.length v with
    0 -> true
  | 1 -> not (Set.mem v.(0) s)
  | n -> let rec disjoint_all i =
           if i >= n then true
           else if Set.mem v.(i) s then false
           else disjoint_all (i+1)
         in disjoint_all 0

let set_of_array v =
  match Array.length v with
    0 -> Set.empty
  | 1 -> Set.add v.(0) Set.empty
  | n -> let rec add_all i =
           if i >= n then Set.empty else Set.add v.(i) (add_all(i+1))
         in add_all 0

let at_same_location reg1 reg2 =
  (* We need to check the register classes too: two locations both saying
     "stack offset N" might actually be different physical locations, for
     example if one is of class "Int" and another "Float" on amd64. *)
  reg1.shared.loc = reg2.shared.loc
    && Proc.register_class reg1 = Proc.register_class reg2

module With_debug_info = struct
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
      reg : reg;
      debug_info : Debug_info.t option;
    }

    let compare t1 t2 =
      let c = RegOrder.compare t1.reg t2.reg in
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
end
