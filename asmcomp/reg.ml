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

type shared = {
  mutability : Cmm.mutability;
  stamp: int;
  mutable typ: Cmm.machtype_component;
  mutable loc: location;
  mutable spill: bool;
  mutable part: int option;
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

type t = {
  mutable name : Ident.t option;
  shared : shared;
}

type reg = t

let dummy =
  let shared =
    { mutability = Cmm.Immutable;
      stamp = 0; typ = Int; loc = Unknown;
      spill = false; interf = []; prefer = []; degree = 0; spill_cost = 0;
      visited = false; part = None;
    }
  in
  { name = None;
    shared;
  }

let currstamp = ref 0
let reg_list = ref([] : shared list)

let create ?(mutability = Cmm.Immutable) ty =
  let shared = { mutability; stamp = !currstamp; typ = ty;
            loc = Unknown; spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; part = None; } in
  reg_list := shared :: !reg_list;
  incr currstamp;
  { name = None;
    shared;
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
  let nr = create r.shared.typ in
  nr.name <- r.name;
  nr

(* The name of registers created in [Proc]. *)
let proc_reg_name = Ident.create "R"

let at_location ty loc =
  (* CR mshinwell: check mutability *)
  let shared = { mutability = Cmm.Mutable;
            stamp = !currstamp; typ = ty; loc;
            spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; part = None; } in
  incr currstamp;
  { name = Some proc_reg_name;
    shared;
  }

let immutable t =
  match t.shared.mutability with
  | Immutable -> true
  | Mutable -> false

let name t =
  match t.name with
  | None -> ""
  | Some ident ->
    let name = Ident.name ident in
    let with_spilled =
      if t.shared.spill then
        "spilled-" ^ name
      else
        name
    in
    match t.shared.part with
    | None -> with_spilled
    | Some part -> with_spilled ^ "#" ^ string_of_int part

let anonymous t =
  match t.name with
  | None -> true
  | Some _ident -> false

let anonymise t = { t with name = None; }

let rename t name = { t with name; }

let identical_except_in_name r ~take_name_from =
  match take_name_from.name with
  | None -> r
  | Some name -> { r with name = Some name; }

let identical_except_in_namev rs ~take_names_from =
  if Array.length rs <> Array.length take_names_from then
    failwith "Reg.identical_except_in_namev with different length arrays";
  Array.init (Array.length rs) (fun index ->
    identical_except_in_name rs.(index)
      ~take_name_from:take_names_from.(index))

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

let holds_pointer t =
  match t.typ with
  | Addr | Val -> true
  | Int | Float -> false

let holds_non_pointer t = not (holds_pointer t)
