(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module L = Linearize

(* CR mshinwell: We're getting ranges that should be concatenated in the
   output.  For example:
    00041726 ffffffffffffffff 0000000000433ce0 (base address)
    00041736 0000000000434113 000000000043418d (DW_OP_bregx: 5 (rdi) 0)
    0004174b 000000000043418e 0000000000434193 (DW_OP_bregx: 5 (rdi) 0)
*)

type phantom_defining_expr =
  | Symbol of Symbol.t
  | Int of int

let rewrite_label env label =
  match Numbers.Int.Map.find label env with
  | exception Not_found -> label
  | label -> label

module Available_subrange : sig
  type t

  type location =
    | Reg of Reg.t
    | Phantom of phantom_defining_expr

  val create
     : start_insn:L.instruction  (* must be [Lavailable_subrange] *)
    -> start_pos:L.label
    -> end_pos:L.label
    -> t

  val create_phantom
     : ident:Ident.t
    -> provenance:Clambda.ulet_provenance
    -> defining_expr:phantom_defining_expr
    -> start_pos:Linearize.label
    -> end_pos:Linearize.label
    -> t

  val start_pos : t -> L.label
  val end_pos : t -> L.label
  val location : t -> location
  val offset_from_stack_ptr : t -> int option
  val ident : t -> Ident.t

  val rewrite_labels : t -> env:int Numbers.Int.Map.t -> t
end = struct
  type start_insn_or_symbol =
    | Start_insn of L.instruction
    | Phantom of Ident.t * Clambda.ulet_provenance * phantom_defining_expr

  type t = {
    (* CR-soon mshinwell: find a better name for [start_insn] *)
    start_insn : start_insn_or_symbol;
    start_pos : L.label;
    (* CR mshinwell: we need to check exactly what happens with function
       epilogues, including returns in the middle of functions. *)
    end_pos : L.label;
  }

  type location =
    | Reg of Reg.t
    | Phantom of phantom_defining_expr

  let create ~start_insn ~start_pos ~end_pos =
    match start_insn.L.desc with
    | L.Lavailable_subrange _ ->
      { start_insn = Start_insn start_insn;
        start_pos;
        end_pos;
      }
    | _ -> failwith "Available_subrange.create"

  let create_phantom ~ident ~provenance ~defining_expr ~start_pos ~end_pos =
    { start_insn = Phantom (ident, provenance, defining_expr);
      start_pos;
      end_pos;
    }

  let start_pos t = t.start_pos
  let end_pos t = t.end_pos

  let location t : location =
    match t.start_insn with
    | Start_insn insn ->
      assert (Array.length insn.L.arg = 1);
      Reg (insn.L.arg.(0))
    | Phantom (_ident, _provenance, defining_expr) ->
      Phantom defining_expr

  let offset_from_stack_ptr t =
    match t.start_insn with
    | Start_insn insn ->
      begin match insn.L.desc with
      | L.Lavailable_subrange offset -> !offset
      | _ -> assert false
      end
    | Phantom _ -> None

  let ident t =
    match t.start_insn with
    | Start_insn insn ->
      let reg = insn.L.arg.(0) in
      begin match reg.Reg.name with
      | Some ident -> ident
      | None -> assert false  (* most likely a bug in available_regs.ml *)
      end
    | Phantom (ident, _, _) -> ident

  let rewrite_labels t ~env =
    { t with
      start_pos = rewrite_label env t.start_pos;
      end_pos = rewrite_label env t.end_pos;
    }
end

module Available_range : sig
  type t

  val create : unit -> t

  val create_phantom
     : ident:Ident.t
    -> provenance:Clambda.ulet_provenance
    -> defining_expr:phantom_defining_expr
    -> range_start:Linearize.label
    -> range_end:Linearize.label
    -> t

  val is_parameter : t -> int option
  val add_subrange : t -> subrange:Available_subrange.t -> unit
  val extremities : t -> L.label * L.label

  val iter
     : t
    -> f:(available_subrange:Available_subrange.t -> unit)
    -> unit

  val fold
     : t
    -> init:'a
    -> f:('a -> available_subrange:Available_subrange.t -> 'a)
    -> 'a

  val rewrite_labels : t -> env:int Numbers.Int.Map.t -> t
end = struct
  type t = {
    mutable subranges : Available_subrange.t list;
    mutable min_pos : L.label option;
    mutable max_pos : L.label option;
  }

  let create () = { subranges = []; min_pos = None; max_pos = None; } 

  let create_phantom ~ident ~provenance ~defining_expr ~range_start
        ~range_end =
    let subrange =
      Available_subrange.create_phantom ~defining_expr ~ident ~provenance
        ~start_pos:range_start ~end_pos:range_end
    in
    { subranges = [subrange];
      min_pos = Some range_start;
      max_pos = Some range_end;
    }

  let add_subrange t ~subrange =
    let start_pos = Available_subrange.start_pos subrange in
    let end_pos = Available_subrange.end_pos subrange in
    (* CR-someday mshinwell: consider if there is a way of addressing the
       label ordering problem *)
    (* This is dubious, but should be correct by virtue of the way label
       counters are allocated (see linearize.ml) and the fact that, below,
       we go through the code from lowest (code) address to highest.  As
       such the label with the highest integer value should be the one with
       the highest address, and vice-versa.  (Note that we also exploit the
       ordering when constructing location lists, to ensure that they are
       sorted in increasing program counter order by start address.) *)
    assert (compare start_pos end_pos <= 0);
    begin
      match t.min_pos with
      | None -> t.min_pos <- Some start_pos
      | Some min_pos ->
        if compare start_pos min_pos < 0 then t.min_pos <- Some start_pos
    end;
    begin
      match t.max_pos with
      | None -> t.max_pos <- Some end_pos
      | Some max_pos ->
        if compare (`At_label end_pos) (`At_label max_pos) > 0 then
          t.max_pos <- Some end_pos
    end;
    t.subranges <- subrange::t.subranges

  let is_parameter t =
    match t.subranges with
    | [] -> assert false
    | subrange::_ ->
      match Available_subrange.location subrange with
      | Reg reg -> reg.Reg.shared.Reg.is_parameter
      | Phantom _ -> None

  let extremities t =
    match t.min_pos, t.max_pos with
    | Some min, Some max -> min, max
    | Some _, None | None, Some _ -> assert false
    | None, None -> failwith "Available_ranges.extremities on empty range"

  let iter t ~f =
    List.iter (fun available_subrange -> f ~available_subrange)
      t.subranges

  let fold t ~init ~f =
    List.fold_left (fun acc available_subrange -> f acc ~available_subrange)
      init
      t.subranges

  let rewrite_labels t ~env =
    let subranges =
      List.map (fun subrange ->
          Available_subrange.rewrite_labels subrange ~env)
        t.subranges
    in
    let min_pos = Misc.Stdlib.Option.map (rewrite_label env) t.min_pos in
    let max_pos = Misc.Stdlib.Option.map (rewrite_label env) t.max_pos in
    { subranges; min_pos; max_pos; }
end

type t = {
  mutable ranges : Available_range.t Ident.tbl;
}

let find t ~ident =
  match Ident.find_same ident t.ranges with
  | exception Not_found -> None
  | range -> Some range

let fold ?exclude t ~init ~f =
  Ident.fold_all (fun ident range acc ->
      let is_unique =
        List.length (Ident.find_all (Ident.name ident) t.ranges) <= 1
      in
      let call_f =
        match exclude with
        | None -> true
        | Some exclude -> not (Ident.same ident exclude)
      in
      if call_f then f acc ~ident ~is_unique ~range
      else acc)
    t.ranges
    init

let add_subrange t ~subrange =
  let ident = Available_subrange.ident subrange in
  let range =
    try Ident.find_same ident t.ranges
    with Not_found -> begin
      let range = Available_range.create () in
      t.ranges <- Ident.add ident range t.ranges;
      range
    end
  in
  Available_range.add_subrange range ~subrange

let available_before insn =
  let available_before, _idents_seen =
    Reg.Set.fold (fun reg ((available_before, idents_seen) as acc) ->
      (* CR-soon mshinwell: handle values split across multiple registers *)
      if reg.Reg.shared.Reg.part <> None then
        acc
      else
        match reg.Reg.name with
        | None -> acc  (* ignore registers without proper names *)
        | Some ident ->
          try
            let () = Ident.find_same ident idents_seen in
            (* We don't need more than one reg location for a given name. *)
            acc
          with Not_found -> begin
            let available_before = Reg.Set.add reg available_before in
            let idents_seen = Ident.add ident () idents_seen in
            available_before, idents_seen
          end)
      insn.L.available_before (Reg.Set.empty, Ident.empty)
  in
  available_before

(* Imagine that the program counter is exactly at the start of [insn]; it has
   not yet been executed.  This function calculates which available subranges
   are to start at that point, and which are to stop.  [prev_insn] is the
   instruction immediately prior to [insn], if such exists. *)
let births_and_deaths ~insn ~prev_insn =
  (* Available subranges must not cross points at which the stack pointer
     changes.  (This is because we assign a single stack offset for each
     available subrange, cf. [Lavailable_subrange].)  Thus, if the previous
     instruction adjusted the stack pointer, then as soon as the program
     counter reaches the first address immediately after that instruction
     we must "restart" all continuing available subranges. *)
  let adjusts_sp =
    match prev_insn with
    | None -> false
    | Some prev_insn ->
      match prev_insn.L.desc with
      | L.Lop (Mach.Istackoffset _) -> true
      | _ -> false
  in
  let births =
    match prev_insn with
    | None -> available_before insn
    | Some prev_insn ->
      if not adjusts_sp then
        Reg.Set.diff (available_before insn) (available_before prev_insn)
      else
        available_before insn
  in
  let deaths =
    match prev_insn with
    | None -> Reg.Set.empty
    | Some prev_insn ->
      if not adjusts_sp then
        Reg.Set.diff (available_before prev_insn) (available_before insn)
      else
        available_before prev_insn
  in
  births, deaths

let rec process_instruction t ~first_insn ~insn ~prev_insn
      ~open_subrange_start_insns =
  let births, deaths = births_and_deaths ~insn ~prev_insn in
  let first_insn = ref first_insn in
  let prev_insn = ref prev_insn in
  let insert_insn ~new_insn =
    assert (new_insn.L.next == insn);
    (* (Note that by virtue of [Lprologue], we can insert labels prior
       to the first assembly instruction of the function.) *)
    begin match !prev_insn with
    | None -> first_insn := new_insn
    | Some prev_insn ->
      assert (prev_insn.L.next == insn);
      prev_insn.L.next <- new_insn
    end;
    prev_insn := Some new_insn
  in
  (* Note that we can't reuse an existing label in the code since we rely
     on the ordering of range-related labels. *)
  let label = lazy (L.new_label ()) in
  (* As a result of the code above to restart subranges where a stack
     adjustment is involved, we may have a register occurring in both
     [births] and [deaths]; and we would like the register to have an open
     subrange from this point.  It follows that we should process deaths
     before births. *)
  Reg.Set.fold (fun reg () ->
      let start_pos, start_insn =
        try Reg.Map.find reg open_subrange_start_insns
        with Not_found -> assert false
      in
      let end_pos = Lazy.force label in
      let subrange =
        Available_subrange.create ~start_pos ~start_insn ~end_pos
      in
      add_subrange t ~subrange)
    deaths
    ();
  let open_subrange_start_insns =
    let open_subrange_start_insns =
      (Reg.Map.filter (fun reg _start_insn -> not (Reg.Set.mem reg deaths))
        open_subrange_start_insns)
    in
    Reg.Set.fold (fun reg open_subrange_start_insns ->
        let new_insn =
          { L.
            desc = L.Lavailable_subrange (ref None);
            next = insn;
            arg = [| reg |];
            res = [| |];
            dbg = Debuginfo.none;
            live = Reg.Set.empty;
            available_before = Reg.Set.empty;
          }
        in
        insert_insn ~new_insn;
        Reg.Map.add reg (Lazy.force label, new_insn) open_subrange_start_insns)
      births
      open_subrange_start_insns
  in
  begin if Lazy.is_val label then
    let new_insn =
      { L.
        desc = L.Llabel (Lazy.force label);
        next = insn;
        arg = [| |];
        res = [| |];
        dbg = Debuginfo.none;
        live = Reg.Set.empty;
        available_before = Reg.Set.empty;
      }
    in
    insert_insn ~new_insn
  end;
  let first_insn = !first_insn in
  match insn.L.desc with
  | L.Lend -> first_insn
  | L.Lprologue | L.Lop _ | L.Lreloadretaddr | L.Lreturn | L.Llabel _
  | L.Lbranch _ | L.Lcondbranch _ | L.Lcondbranch3 _ | L.Lswitch _
  | L.Lsetuptrap _ | L.Lpushtrap | L.Lpoptrap | L.Lraise _
  | L.Lavailable_subrange _ ->
    process_instruction t ~first_insn ~insn:insn.L.next ~prev_insn:(Some insn)
      ~open_subrange_start_insns

let create ~fundecl ~phantom_ranges =
  let phantom_ranges =
    Ident.fold_all (fun ident (range : Linearize.phantom_let_range) ranges ->
        (* CR mshinwell: Not quite right.  The symbol is actually in the
           defining expression, so it probably shouldn't be an argument to
           [Available_range.create_phantom]. *)
        let defining_expr : phantom_defining_expr option =
          match range.defining_expr with
          | Uphantom_const (Uconst_ref (symbol, _defining_expr)) ->
            (* It's not actually a "fun_name", but the mangling is the same.
               This should go away if we switch to [Symbol.t] everywhere. *)
            Some (Symbol (Name_laundry.fun_name_to_symbol symbol))
          | Uphantom_const (Uconst_int i) ->
            Some (Int i)
          | Uphantom_const _ -> None
        in
        match defining_expr with
        | None -> ranges
        | Some defining_expr ->
          let range =
            Available_range.create_phantom ~ident
              ~provenance:range.provenance
              ~defining_expr
              ~range_start:range.starting_label
              ~range_end:range.ending_label
          in
          Ident.add ident range ranges)
      phantom_ranges
      Ident.empty
  in
  let t =
    { ranges = phantom_ranges;
    }
  in
  let first_insn =
    let first_insn = fundecl.L.fun_body in
    process_instruction t ~first_insn ~insn:first_insn ~prev_insn:None
      ~open_subrange_start_insns:Reg.Map.empty
  in
(*
  Printf.printf "Available ranges for function: %s\n%!" fundecl.L.fun_name;
  fold t ~init:()
    ~f:(fun () ~ident ~is_unique ~range ->
        Printf.printf "  Identifier: %s (is unique? %s)\n%!"
          (Ident.unique_name ident) (if is_unique then "yes" else "no");
        Available_range.fold range ~init:()
          ~f:(fun () ~available_subrange ->
            Printf.printf "    Label range: %d -> %d\n%!"
              (Available_subrange.start_pos available_subrange)    
              (Available_subrange.end_pos available_subrange)));
*)
  t, { fundecl with L.fun_body = first_insn; }

type label_classification =
  | Start of {
      end_pos : Linearize.label;
      location : Available_subrange.location;
    }
  | End

exception Found_label of label_classification * Available_subrange.t

let classify_label t label =
  try
    Ident.iter (fun _ident range ->
        Available_range.iter range ~f:(fun ~available_subrange:subrange ->
          if Available_subrange.start_pos subrange = label then
            let location = Available_subrange.location subrange in
            let end_pos = Available_subrange.end_pos subrange in
            raise (Found_label (Start { end_pos; location; }, subrange))
          else if Available_subrange.end_pos subrange = label then
            raise (Found_label (End, subrange))))
      t.ranges;
    None
  with Found_label (start_or_end, subrange) ->
    Some (start_or_end, subrange)

let rewrite_labels t ~env =
  let ranges =
    Ident.fold_all (fun ident range ranges ->
        let range = Available_range.rewrite_labels range ~env in
        Ident.add ident range ranges)
      t.ranges
      Ident.empty
  in
  { ranges;
  }
