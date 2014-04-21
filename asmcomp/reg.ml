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
  val create_anon : unit -> t
  val create_procedure_call_convention : unit -> t
  val create_from_ident : Ident.t -> t
  val augment : t -> new_name:t -> t
  val to_string : t -> typ:Cmm.machtype_component -> string
  val immutable : t -> bool
  val immutable_and_anonymous : t -> bool
end = struct
  module One = struct
    type t =
      | Anon
      | Procedure_call_convention
      | Immutable_ident of Ident.t

    let compare = Pervasives.compare

    let to_string t ~typ =
      match t with
      | Anon ->
        begin match typ with
        | Addr -> "A"
        | Int -> "I"
        | Float -> "F"
        end
      | Procedure_call_convention -> "R"
      | Immutable_ident ident ->
        let name = Ident.unique_name ident in
        assert (String.length name > 0);
        name
  end

  module O = One
  module Set = Set.Make (O)

  type t =
    | Immutable of Set.t
    | Mutable_ident of Ident.t

  let create_anon () =
    Immutable (Set.singleton O.Anon)

  let create_procedure_call_convention () =
    Immutable (Set.singleton O.Procedure_call_convention)

  let create_from_ident ident =
    if Ident.is_mutable ident then
      Mutable_ident ident
    else
      Immutable (Set.singleton (O.Immutable_ident ident))

  let augment t ~new_name =
    match t, new_name with
    | (Mutable_ident _ | Immutable _), Mutable_ident ident ->
      Mutable_ident ident  (* A mutable name always clobbers any other name. *)
    | Mutable_ident ident, Immutable _immset ->
      Mutable_ident ident  (* To cope with updates of regs holding mutable values. *)
    | Immutable immset, Immutable immset' ->
      (* CR mshinwell: "R" should not overwrite "Anon" probably *)
      match Set.elements immset with
      | [O.Anon] | [O.Procedure_call_convention] -> Immutable immset'
      | _ ->
        match Set.elements immset' with
        | [O.Anon] | [O.Procedure_call_convention] -> Immutable immset
        | _ -> Immutable (Set.union immset immset')

  let immutable = function
    | Immutable _ -> true
    | Mutable_ident _ -> false

  let immutable_and_anonymous = function
    | Immutable immset ->
      begin match Set.elements immset with
      | [O.Anon] -> true
      | _ -> false
      end
    | Mutable_ident _ -> false

  let to_string t ~typ =
    match t with
    | Immutable immset ->
      Set.fold (fun one result ->
        let one = O.to_string one ~typ in
        if result = "" then one
        else one ^ "|" ^ result) immset ""
    | Mutable_ident ident ->
      let name = Ident.unique_name ident in
      assert (String.length name > 0);
      name ^ "M"
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
  { raw_name = Raw_name.create_anon (); stamp = 0; typ = Int; loc = Unknown;
    spill = false; interf = []; prefer = []; degree = 0; spill_cost = 0;
    visited = false; part = None;
  }

let currstamp = ref 0
let reg_list = ref([] : t list)

let create ty =
  let r = { raw_name = Raw_name.create_anon (); stamp = !currstamp; typ = ty;
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

let create_procedure_call_convention ty loc =
  (* [Raw_name.Procedure_call_convention] doesn't take an argument because we
     should always take a copy of a value of type [t] used to represent a hard
     register / stack slot (as allocated by the various proc.ml files) and name
     it accordingly, for example when moving from function argument registers.
     See selectgen.ml. *)
  let r = { raw_name = Raw_name.create_procedure_call_convention ();
            stamp = !currstamp; typ = ty; loc;
            spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; part = None; } in
  incr currstamp;
  r

let immutable t = Raw_name.immutable t.raw_name
let immutable_and_anonymous t = Raw_name.immutable_and_anonymous t.raw_name

let name t =
  let raw_name = Raw_name.to_string t.raw_name ~typ:t.typ in
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
