(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Cmm

type t =
  { mutable name: string;
    stamp: int;
    typ: Cmm.machtype_component;
    mutable loc: location;
    mutable spill: bool;
    mutable interf: t list;
    mutable prefer: (t * int) list;
    mutable degree: int;
    mutable spill_cost: int;
    mutable visited: bool;
    mutable is_parameter: int option;
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
  { name = ""; stamp = 0; typ = Int; loc = Unknown; spill = false;
    interf = []; prefer = []; degree = 0; spill_cost = 0; visited = false;
    is_parameter = None; }

let currstamp = ref 0
let reg_list = ref([] : t list)

let create ty =
  let r = { name = ""; stamp = !currstamp; typ = ty; loc = Unknown;
            spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; is_parameter = None; } in
  reg_list := r :: !reg_list;
  incr currstamp;
  r

let createv tyv =
  let n = Array.length tyv in
  let rv = Array.create n dummy in
  for i = 0 to n-1 do rv.(i) <- create tyv.(i) done;
  rv

let createv_like rv =
  let n = Array.length rv in
  let rv' = Array.create n dummy in
  for i = 0 to n-1 do rv'.(i) <- create rv.(i).typ done;
  rv'

let clone r =
  let nr = create r.typ in
  nr.name <- r.name;
  nr

let at_location ty loc =
  let r = { name = "R"; stamp = !currstamp; typ = ty; loc = loc; spill = false;
            interf = []; prefer = []; degree = 0; spill_cost = 0;
            visited = false; is_parameter = None; } in
  incr currstamp;
  r

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

let reinit_reg r =
  r.loc <- Unknown;
  r.interf <- [];
  r.prefer <- [];
  r.degree <- 0;
  (* Preserve the very high spill costs introduced by the reloading pass *)
  if r.spill_cost >= 100000
  then r.spill_cost <- 100000
  else r.spill_cost <- 0

let reinit() =
  List.iter reinit_reg !reg_list

module RegOrder =
  struct
    type t = reg
    let compare r1 r2 = r1.stamp - r2.stamp
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

let set_of_array v =
  match Array.length v with
    0 -> Set.empty
  | 1 -> Set.add v.(0) Set.empty
  | n -> let rec add_all i =
           if i >= n then Set.empty else Set.add v.(i) (add_all(i+1))
         in add_all 0

let name t =
  match t.is_parameter with
  | None -> t.name
  | Some index -> Printf.sprintf "%s-%d" t.name index

(* CR mshinwell: think about a cleaner way to do this.  just a flag? *)
let name_strip_spilled t =
  let name = name t in
  let prefix = "spilled-" in
  let name =
    if String.length name > String.length prefix
       && String.sub name 0 (String.length prefix) = prefix
    then
      String.sub name (String.length prefix) (String.length name - String.length prefix)
    else
      name
  in
  (* CR mshinwell: work out why spilled- ones don't have the "which parameter" suffix. *)
  match (try Some (String.rindex name '-') with Not_found -> None) with
  | None -> name
  | Some index -> String.sub name 0 index

let location t =
  t.loc

let set_is_parameter t ~parameter_index =
  t.is_parameter <- Some parameter_index

let is_parameter t =
  t.is_parameter

let all_registers_set () =
  ListLabels.fold_left (all_registers ())
    ~init:Set.empty
    ~f:(fun set reg -> Set.add reg set)

let same_location t t' =
  t.loc = t'.loc

let with_name t ~name =
  { t with name; }

let with_name_from t ~from =
  { t with name = from.name; }

let with_name_fromv ts ~from =
  if Array.length ts <> Array.length from then
    failwith "Reg.with_name_fromv: arrays of regs are of different lengths";
  Array.mapi (fun index reg -> with_name_from reg ~from:from.(index)) ts
