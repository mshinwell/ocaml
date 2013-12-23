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

type raw_name =
  | Anon
  | R
  | Named of string

type t =
  { mutable raw_name: raw_name;
    stamp: int;
    typ: Cmm.machtype_component;
    mutable loc: location;
    mutable spill: bool;
    mutable interf: t list;
    mutable prefer: (t * int) list;
    mutable degree: int;
    mutable spill_cost: int;
    mutable visited: bool;
    mutable partial_value : int option;
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
  { raw_name = Anon; stamp = 0; typ = Int; loc = Unknown; spill = false;
    interf = []; prefer = []; degree = 0; spill_cost = 0; visited = false;
    partial_value = None; is_parameter = None; }

let currstamp = ref 0
let reg_list = ref([] : t list)

let create ty =
  let r = { raw_name = Anon; stamp = !currstamp; typ = ty; loc = Unknown;
            spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; partial_value = None;
            is_parameter = None; } in
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
  nr.raw_name <- r.raw_name;
  nr

let at_location ty loc =
  let r = { raw_name = R; stamp = !currstamp; typ = ty; loc = loc; spill = false;
            interf = []; prefer = []; degree = 0; spill_cost = 0;
            visited = false; partial_value = None; is_parameter = None; } in
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

let anonymous t =
  match t.raw_name with
  | Anon | Named "" -> true
  | R | Named _ -> false

let has_name_suitable_for_debugger t =
  match t.raw_name with
  | Anon | Named "" | R -> false
  | Named reg_name ->
    let is_internal =
      let internal_prefix = "__ocaml" in
      String.length reg_name > String.length internal_prefix
        && String.sub reg_name 0 (String.length internal_prefix) = internal_prefix
    in
    not is_internal

let name_for_debugger_exn t =
  if not (has_name_suitable_for_debugger t) then
    failwith "Reg.name_for_debugger_exn on register that is not for the debugger";
  match t.raw_name with
  | Named name when String.length name > 0 -> name
  | _ -> assert false

let name_for_printing t =
  let raw_name =
    match t.raw_name with
    | Anon | Named "" -> None
    | R -> Some "R"
    | Named name -> Some name
  in
  match raw_name with
  | None -> ""
  | Some raw_name ->
    Printf.sprintf "%s%s%s"
      (if t.spill then "spilled-" else "")
      raw_name
      (match t.partial_value with None -> "" | Some part -> string_of_int part)

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
  { t with raw_name = name; }

let with_name_from t ~from =
  { t with raw_name = from.raw_name; }

let with_name_fromv ts ~from =
  if Array.length ts <> Array.length from then
    failwith "Reg.with_name_fromv: arrays of regs are of different lengths";
  Array.mapi (fun index reg -> with_name_from reg ~from:from.(index)) ts

let stamp t = t.stamp
