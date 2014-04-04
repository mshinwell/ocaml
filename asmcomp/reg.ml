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

module Raw_name = struct
  type t =
    | Anon
    | R
    | Ident of Ident.t
    | Symbol of string
    | Block_header of nativeint
    | Uninitialized_block
    | With_displacement of t * int

  let create_from_ident ident = Ident ident
  let create_from_symbol sym = Symbol sym
  let create_from_blockheader hdr = Block_header hdr
  let pointer_to_uninitialized_block = Uninitialized_block

  let do_not_propagate _t = false
(*
  let rec do_not_propagate = function
    | Symbol _
    | Block_header _
    | Uninitialized_block -> true
    | Anon
    | R
    | Ident _ -> false
    | With_displacement (t, _displ) -> do_not_propagate t
*)

  let rec to_string t =
    match t with
    | Anon -> None
    | R -> Some "R"
    | Ident ident ->
      let name = Ident.unique_name ident in
      if String.length name <= 0 then None else Some name
    | Symbol name -> Some (Printf.sprintf "symbol(%s)" name)
    | Block_header hdr ->
      (* CR mshinwell: fix for 32 bits (and large 64 bit blocks) *)
      let hdr = Nativeint.to_int hdr in
      let raw_tag = hdr land 0xff in
      let raw_colour = (hdr lsr 8) land 0x3 in
      let raw_size = hdr lsr 10 in
      let tag =
        if raw_tag = Obj.lazy_tag then "Lazy_tag"
        else if raw_tag = Obj.lazy_tag then "Lazy_tag"
        else if raw_tag = Obj.closure_tag then "Closure_tag"
        else if raw_tag = Obj.object_tag then "Object_tag"
        else if raw_tag = Obj.infix_tag then "Infix_tag"
        else if raw_tag = Obj.forward_tag then "Forward_tag"
        else if raw_tag = Obj.no_scan_tag then "No_scan_tag"
        else if raw_tag = Obj.abstract_tag then "Abstract_tag"
        else if raw_tag = Obj.string_tag then "String_tag"
        else if raw_tag = Obj.double_tag then "Double_tag"
        else if raw_tag = Obj.double_array_tag then "Double_array_tag"
        else if raw_tag = Obj.custom_tag then "Custom_tag"
        else Printf.sprintf "tag=%d" raw_tag
      in
      let colour =
        match raw_colour with  (* see byterun/gc.h *)
        | 0 -> "white"
        | 1 -> "grey"
        | 2 -> "blue"
        | 3 -> "black"
        | _ -> assert false
      in
      let size = Printf.sprintf "size=%d" raw_size in
      Some (Printf.sprintf "hdr(%s,%s,%s)" tag colour size)
    | Uninitialized_block -> Some "uninited-block"
    | With_displacement (t, displ) ->
      match to_string t with
      | None -> None
      | Some t_str ->
        Some (Printf.sprintf "%s[%d]" t_str (displ / Arch.size_int))

  let augmented_with_displacement t displ =
    (* CR mshinwell: must check that we don't try to do this on a mutable one *)
    With_displacement (t, displ)
end

type t =
  { mutable raw_name: Raw_name.t;
    stamp: int;
    typ: Cmm.machtype_component;
    mutable loc: location;
    mutable spill: bool;
    mutable part: int option;
    mutable interf: t list;
    mutable prefer: (t * int) list;
    mutable degree: int;
    mutable spill_cost: int;
    mutable visited: bool }

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
  { raw_name = Raw_name.Anon; stamp = 0; typ = Int; loc = Unknown;
    spill = false; interf = []; prefer = []; degree = 0; spill_cost = 0;
    visited = false; part = None;
  }

let currstamp = ref 0
let reg_list = ref([] : t list)

let create ty =
  let r = { raw_name = Raw_name.Anon; stamp = !currstamp; typ = ty;
            loc = Unknown; spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; part = None; } in
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

let identical_except_in_name r ~from =
  { r with raw_name = from.raw_name; }

let identical_except_in_namev rs ~from =
  if Array.length rs <> Array.length from then
    failwith "Reg.identical_except_in_namev with different length arrays";
  Array.init (Array.length rs)
    (fun index -> identical_except_in_name rs.(index) ~from:from.(index))

let at_location ty loc =
  let r = { raw_name = Raw_name.R; stamp = !currstamp; typ = ty; loc;
            spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; part = None; } in
  incr currstamp;
  r

let anonymous t =
  match t.raw_name with
  | Raw_name.Anon
  | Raw_name.R -> true
  | Raw_name.Ident _
  | Raw_name.Symbol _
  | Raw_name.Block_header _
  | Raw_name.Uninitialized_block
  | Raw_name.With_displacement _ -> false

let immutable t =
  match t.raw_name with
  | Raw_name.Ident ident -> not (Ident.is_mutable ident)
  | Raw_name.Anon
  | Raw_name.R
  | Raw_name.Symbol _
  | Raw_name.Block_header _
  | Raw_name.Uninitialized_block
  | Raw_name.With_displacement _ -> true  (* CR mshinwell: see note above re. this case *)

let name t =
  match Raw_name.to_string t.raw_name with
  | None -> ""
  | Some raw_name ->
    let with_spilled =
      if t.spill then
        "spilled-" ^ raw_name
      else
        raw_name
    in
    match t.part with
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
