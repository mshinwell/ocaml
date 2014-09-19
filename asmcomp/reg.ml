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

module Raw_name : sig
  type t

  val create_temporary : unit -> t
  val create_procedure_call_convention : unit -> t
  val create_ident : Ident.t -> t

  val to_string : t -> string option
  val is_temporary : t -> bool
  val is_procedure_call_convention : t -> bool
  val is_ident : t -> bool
  val to_ident : t -> Ident.t option

  val (=) : t -> t -> bool
end = struct
  type t =
    | Temporary
    | Procedure_call_convention
    | Ident of Ident.t

  (* The reader may wonder why we don't allow multiple identifier names to
     be associated with a given register.  The reason is that for situations
     where this might arise, e.g.:

        let x = y in
        ...

     we will always emit a move at the moment, since it is assumed [y] might
     be mutable (in the [Cassign] sense).  See [bind_let] in selectgen.ml.
  *)
  (* CR-soon mshinwell: see if we can do better here *)

  let create_temporary () = Temporary
  let create_procedure_call_convention () = Procedure_call_convention
  let create_ident ident = Ident ident

  let to_string t =
    match t with
    | Temporary -> None
    | Procedure_call_convention -> Some "R"
    | Ident ident ->
      let name = Ident.name ident in
      if String.length name <= 0 then None else Some name

  let is_temporary = function
    | Temporary -> true
    | Procedure_call_convention | Ident _ -> false

  let is_procedure_call_convention = function
    | Temporary | Ident _ -> false
    | Procedure_call_convention -> true

  let is_ident = function
    | Ident _ -> true
    | Temporary | Procedure_call_convention -> false

  let to_ident = function
    | Ident ident -> Some ident
    | Temporary | Procedure_call_convention -> None

  let (=) = Pervasives.(=)
end

type t =
  { mutable raw_name: Raw_name.t;
    stamp: int;
    typ: Cmm.machtype_component;
    mutable loc: location;
    mutable spill: bool;
    mutable part: int option;
    mutable is_parameter: bool;
    mutable interf: t list;
    mutable prefer: (t * int) list;
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
  { raw_name = Raw_name.create_temporary ();
    stamp = 0; typ = Int; loc = Unknown;
    spill = false; interf = []; prefer = []; degree = 0; spill_cost = 0;
    visited = false; part = None; is_parameter = false; }

let currstamp = ref 0
let reg_list = ref([] : t list)

let create ty =
  let r = { raw_name = Raw_name.create_temporary ();
            stamp = !currstamp; typ = ty;
            loc = Unknown; spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; part = None;
            is_parameter = false; } in
  reg_list := r :: !reg_list;
  incr currstamp;
  r

let createv tyv =
  let n = Array.length tyv in
  let rv = Array.make n dummy in
  for i = 0 to n-1 do rv.(i) <- create tyv.(i) done;
  rv

let createv_like rv =
  let n = Array.length rv in
  let rv' = Array.make n dummy in
  for i = 0 to n-1 do rv'.(i) <- create rv.(i).typ done;
  rv'

let clone r =
  let nr = create r.typ in
  nr.raw_name <- r.raw_name;
  nr

let create_procedure_call_convention ty loc =
  let r = { raw_name = Raw_name.create_procedure_call_convention ();
            stamp = !currstamp; typ = ty; loc;
            spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; part = None;
            is_parameter = false; } in
  incr currstamp;
  r

let identical_except_in_name r ~from =
  { r with raw_name = from.raw_name; }

let identical_except_in_namev rs ~from =
  if Array.length rs <> Array.length from then
    failwith "Reg.identical_except_in_namev with different length arrays";
  Array.init (Array.length rs)
    (fun index -> identical_except_in_name rs.(index) ~from:from.(index))

let name t =
  let raw_name =
    match Raw_name.to_string t.raw_name with
    | Some raw_name -> raw_name
    | None ->
      match t.typ with
      | Addr -> "A"
      | Int -> "I"
      | Float -> "F"
  in
  let with_spilled =
    if t.spill then
      "spilled-" ^ raw_name
    else
      raw_name
  in
  match t.part with
  | None -> with_spilled
  | Some part -> with_spilled ^ "#" ^ string_of_int part

let is_temporary t = Raw_name.is_temporary t.raw_name
let is_procedure_call_convention t =
  Raw_name.is_procedure_call_convention t.raw_name

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

let compare_stamps r1 r2 = r1.stamp - r2.stamp

module RegOrder =
  struct
    type t = reg
    let compare = compare_stamps
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
  | Addr -> true
  | Int | Float -> false

let holds_non_pointer t = not (holds_pointer t)
