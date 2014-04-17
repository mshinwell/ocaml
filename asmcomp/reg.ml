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
  val create_hard_reg : unit -> t
  val create_from_ident : Ident.t -> t
  val create_from_symbol : string -> t
  val create_from_blockheader : nativeint -> t
  val create_pointer_to_uninitialized_block : unit -> t
  val augmented_with_displacement : t -> words:int -> t
  val do_not_propagate : t -> bool
  val to_string : t -> typ:Cmm.machtype_component -> string
  val references_possibly_mutable_identifier : t -> bool
end = struct
  type t =
    | Anon
    | Hard_reg
    | Ident of Ident.t
    | Symbol of string
    | Block_header of nativeint
    | Uninitialized_block
    | With_displacement of t * int

  let create_anon () = Anon
  let create_hard_reg () = Hard_reg
  let create_from_ident ident = Ident ident
  let create_from_symbol sym = Symbol sym
  let create_from_blockheader hdr = Block_header hdr
  let create_pointer_to_uninitialized_block () = Uninitialized_block

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

  let references_possibly_mutable_identifier t =
    match t with
    | Ident ident -> Ident.is_mutable ident
    | Anon
    | Hard_reg
    | Symbol _
    | Block_header _
    | Uninitialized_block
    | With_displacement _ -> false  (* CR mshinwell: see note below re. this case *)

  let rec to_string t ~typ =
    match t with
    | Anon ->
      begin match typ with
      | Addr -> "A"
      | Int -> "I"
      | Float -> "F"
      end
    | Hard_reg -> "R"
    | Ident ident ->
      let name = Ident.unique_name ident in
      assert (String.length name > 0);
      if Ident.is_mutable ident then
        name ^ "M"
      else
        name
    | Symbol name -> Printf.sprintf "symbol(%s)" name
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
      Printf.sprintf "hdr(%s,%s,%s)" tag colour size
    | Uninitialized_block -> "uninited-block"
    | With_displacement (t, displ) ->
      Printf.sprintf "%s[%d]" (to_string t ~typ) (displ / Arch.size_int)

  let augmented_with_displacement t ~words:displ =
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

let create_hard_reg ty loc =
  (* [Raw_name.Hard_reg] doesn't take an argument because we should always
     take a copy of a value of type [t] used to represent a hard register (as
     allocated by the various proc.ml files) and name it accordingly, for
     example when moving from function argument registers.  See
     selectgen.ml. *)
  let r = { raw_name = Raw_name.create_hard_reg ();
            stamp = !currstamp; typ = ty; loc;
            spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false; part = None; } in
  incr currstamp;
  r

let immutable t =
  not (Raw_name.references_possibly_mutable_identifier t.raw_name)

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
