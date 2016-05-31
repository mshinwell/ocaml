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
module RM = Reg.Map_distinguishing_names
module RS = Reg.Set_distinguishing_names

(* CR mshinwell: We're getting ranges that should be concatenated in the
   output.  For example:
    00041726 ffffffffffffffff 0000000000433ce0 (base address)
    00041736 0000000000434113 000000000043418d (DW_OP_bregx: 5 (rdi) 0)
    0004174b 000000000043418e 0000000000434193 (DW_OP_bregx: 5 (rdi) 0)
   (may be fixed now)
*)

type phantom_defining_expr =
  | Int of int
  | Symbol of Symbol.t
  | Read_symbol_field of { symbol : Symbol.t; field : int; }

let rewrite_label env label =
  match Numbers.Int.Map.find label env with
  | exception Not_found -> label
  | label -> label

module Available_subrange : sig
  type t

  type location =
    | Reg of Reg.t
    | Phantom of phantom_defining_expr
    | Read_field of { address : location; field : int; }
    | Offset_pointer of { address : location; offset_in_words : int; }

  val create
     : reg:Reg.t
    -> start_insn:L.instruction
    -> start_pos:L.label
    -> end_pos:L.label
    -> t

  val create_phantom
     : provenance:Clambda.ulet_provenance
    -> defining_expr:phantom_defining_expr
    -> start_pos:Linearize.label
    -> end_pos:Linearize.label
    -> t

  val create_read_field : t -> field:int -> t
  val create_offset_pointer : t -> offset_in_words:int -> t

  val start_pos : t -> L.label
  val end_pos : t -> L.label
  val location : t -> location
  val offset_from_stack_ptr_in_bytes : t -> int option

  val rewrite_labels : t -> env:int Numbers.Int.Map.t -> t
end = struct
  type start_insn_or_symbol =
    | Start_insn of Reg.t * L.instruction
    | Phantom of Clambda.ulet_provenance * phantom_defining_expr
    | Read_field of
        { address : start_insn_or_symbol; field : int; }
    | Offset_pointer of
        { address : start_insn_or_symbol; offset_in_words : int; }

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
    | Read_field of { address : location; field : int; }
    | Offset_pointer of { address : location; offset_in_words : int; }

  let create ~reg ~start_insn ~start_pos ~end_pos =
    assert (reg.Reg.name <> None);
    match start_insn.L.desc with
    | L.Lcapture_stack_offset _ | L.Llabel _ ->
      begin match start_insn.L.desc with
      | L.Lcapture_stack_offset _ ->
        assert (Array.length start_insn.L.arg = 1);
        assert (reg == start_insn.L.arg.(0))
      | _ -> ()
      end;
      { start_insn = Start_insn (reg, start_insn);
        start_pos;
        end_pos;
      }
    | _ -> failwith "Available_subrange.create"

  let create_phantom ~provenance ~defining_expr ~start_pos ~end_pos =
    { start_insn = Phantom (provenance, defining_expr);
      start_pos;
      end_pos;
    }

  let create_read_field t ~field =
    { start_insn = Read_field { address = t.start_insn; field; };
      start_pos = t.start_pos;
      end_pos = t.end_pos;
    }

  let create_offset_pointer t ~offset_in_words =
    { start_insn =
        Offset_pointer { address = t.start_insn; offset_in_words; };
      start_pos = t.start_pos;
      end_pos = t.end_pos;
    }

  let start_pos t = t.start_pos
  let end_pos t = t.end_pos

  let location t : location =
    let rec convert_location (start_insn : start_insn_or_symbol) =
      match start_insn with
      | Start_insn (reg, _insn) -> Reg reg
      | Phantom (_provenance, defining_expr) -> Phantom defining_expr
      | Read_field { address; field; } ->
        let address = convert_location address in
        Read_field { address; field; }
      | Offset_pointer { address; offset_in_words; } ->
        let address = convert_location address in
        Offset_pointer { address; offset_in_words; }
    in
    convert_location t.start_insn

  let offset_from_stack_ptr_in_bytes t =
    let rec offset (start_insn : start_insn_or_symbol) =
      match start_insn with
      | Start_insn (reg, insn) ->
        begin match insn.L.desc with
        | L.Lcapture_stack_offset offset -> !offset
        | L.Llabel _ ->
          assert (not (Reg.assigned_to_stack reg));
          None
        | _ -> assert false
        end
      | Phantom _ -> None
      | Read_field { address; _ } -> offset address
      | Offset_pointer { address; _ } -> offset address
    in
    offset t.start_insn

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
     : provenance:Clambda.ulet_provenance
    -> defining_expr:phantom_defining_expr
    -> range_start:Linearize.label
    -> range_end:Linearize.label
    -> t

  val create_read_field : t -> field:int -> t
  val create_offset_pointer : t -> offset_in_words:int -> t

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

  let create_phantom ~provenance ~defining_expr ~range_start
        ~range_end =
    let subrange =
      Available_subrange.create_phantom ~defining_expr ~provenance
        ~start_pos:range_start ~end_pos:range_end
    in
    { subranges = [subrange];
      min_pos = Some range_start;
      max_pos = Some range_end;
    }

  let create_read_field t ~field =
    let subranges =
      List.map (fun subrange ->
          Available_subrange.create_read_field subrange ~field)
        t.subranges
    in
    { t with subranges; }

  let create_offset_pointer t ~offset_in_words =
    let subranges =
      List.map (fun subrange ->
          Available_subrange.create_offset_pointer subrange
            ~offset_in_words)
        t.subranges
    in
    { t with subranges; }

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
      | Phantom _ | Read_field _ | Offset_pointer _ -> None

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
  ranges : Available_range.t Ident.Tbl.t;
}

let find t ~ident =
  match Ident.Tbl.find t.ranges ident with
  | exception Not_found -> None
  | range -> Some range

let fold t ~init ~f =
  Ident.Tbl.fold (fun ident range acc ->
      let is_unique = true in
(* CR mshinwell: fix this
        List.length (Ident.find_all (Ident.name ident) t.ranges) <= 1
      in
*)
      f acc ~ident ~is_unique ~range)
    t.ranges
    init

let add_subrange t ~ident ~subrange =
  let range =
    try Ident.Tbl.find t.ranges ident
    with Not_found -> begin
      let range = Available_range.create () in
      Ident.Tbl.add t.ranges ident range;
      range
    end
  in
  Available_range.add_subrange range ~subrange

(* CR mshinwell: improve efficiency *)
let available_before (insn : L.instruction) =
  RS.of_list (Reg.Set.elements insn.available_before)

(* Imagine that the program counter is exactly at the start of [insn]; it has
   not yet been executed.  This function calculates which available subranges
   are to start at that point, and which are to stop.  [prev_insn] is the
   instruction immediately prior to [insn], if such exists. *)
let births_and_deaths ~(insn : L.instruction)
      ~(prev_insn : L.instruction option) =
  (* Available subranges must not cross points at which the stack pointer
     changes.  (This is because we assign a single stack offset for each
     available subrange, cf. [Lcapture_stack_offset].)  Thus, if the previous
     instruction adjusted the stack pointer, then as soon as the program
     counter reaches the first address immediately after that instruction
     we must "restart" all continuing available subranges. *)
  let adjusts_sp =
    match prev_insn with
    | None -> false
    | Some prev_insn ->
      match prev_insn.L.desc with
      (* CR mshinwell: should this have a hook into [Proc]? *)
      | L.Lop (Mach.Istackoffset _) -> true
      | _ -> false
  in
  let births =
    match prev_insn with
    | None -> (available_before insn)
    | Some prev_insn ->
      if not adjusts_sp then
        RS.diff (available_before insn) (available_before prev_insn)
      else
        (available_before insn)
  in
  let deaths =
    match prev_insn with
    | None -> RS.empty
    | Some prev_insn ->
      if not adjusts_sp then
        RS.diff (available_before prev_insn) (available_before insn)
      else
        (available_before prev_insn)
  in
  births, deaths

let rec process_instruction t ~first_insn ~(insn : L.instruction) ~prev_insn
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
  RS.fold (fun (reg : Reg.t) () ->
if reg.Reg.name = None then begin
  Format.eprintf "IGNORING REG %a\n%!" Printmach.reg reg;
  assert false
end else
      let start_pos, start_insn =
        try RM.find reg open_subrange_start_insns
        with Not_found -> assert false
      in
      let end_pos = Lazy.force label in
      let subrange =
        Available_subrange.create ~reg ~start_pos ~start_insn ~end_pos
      in
      let ident =
        match reg.name with
        | Some name -> name
        | None -> assert false
      in
      add_subrange t ~ident ~subrange)
    deaths
    ();
  let label_insn =
    lazy ({ L.
      desc = L.Llabel (Lazy.force label);
      next = insn;
      arg = [| |];
      res = [| |];
      dbg = Debuginfo.none;
      live = Reg.Set.empty;
      available_before = insn.available_before;
    })
  in
  let open_subrange_start_insns =
    let open_subrange_start_insns =
      (RM.filter (fun reg _start_insn -> not (RS.mem reg deaths))
        open_subrange_start_insns)
    in
    RS.fold (fun reg open_subrange_start_insns ->
        (* We only need [Lcapture_stack_offset] in the case where the register
           is assigned to the stack.  (It enables us to determine what the
           stack offset will be at that point.) *)
        let new_insn =
          if not (Reg.assigned_to_stack reg) then begin
            Lazy.force label_insn
          end else begin
            let new_insn =
              { L.
                desc = L.Lcapture_stack_offset (ref None);
                next = insn;
                arg = [| reg |];
                res = [| |];
                dbg = Debuginfo.none;
                live = Reg.Set.empty;
                available_before = insn.available_before;
              }
            in
            insert_insn ~new_insn;
            new_insn
          end
        in
        RM.add reg (Lazy.force label, new_insn) open_subrange_start_insns)
      births
      open_subrange_start_insns
  in
  begin if Lazy.is_val label then
    insert_insn ~new_insn:(Lazy.force label_insn)
  end;
  let first_insn = !first_insn in
  match insn.L.desc with
  | L.Lend -> first_insn
  | L.Lprologue | L.Lop _ | L.Lreloadretaddr | L.Lreturn | L.Llabel _
  | L.Lbranch _ | L.Lcondbranch _ | L.Lcondbranch3 _ | L.Lswitch _
  | L.Lsetuptrap _ | L.Lpushtrap | L.Lpoptrap | L.Lraise _
  | L.Lcapture_stack_offset _ ->
    process_instruction t ~first_insn ~insn:insn.L.next ~prev_insn:(Some insn)
      ~open_subrange_start_insns

let create ~fundecl ~phantom_ranges =
  (* Calculate ranges for non-phantom identifiers. *)
  let t = { ranges = Ident.Tbl.create 42; } in
  let first_insn =
    let first_insn = fundecl.L.fun_body in
    process_instruction t ~first_insn ~insn:first_insn ~prev_insn:None
      ~open_subrange_start_insns:RM.empty
  in
  (* Resolve ranges for phantom identifiers.  Some of these may be derived
     from the non-phantom ranges, possibly via some transitive chain of
     aliases or field accesses. *)
  let rec resolve_range target_ident =
    match Ident.Tbl.find t.ranges target_ident with
    | range -> Some range
    | exception Not_found ->
      match Ident.find_same target_ident phantom_ranges with
      | exception Not_found -> None
      | (range : Linearize.phantom_let_range) ->
        let create_new_range ~defining_expr =
          let range =
            Available_range.create_phantom ~provenance:range.provenance
              ~defining_expr
              ~range_start:range.starting_label
              ~range_end:range.ending_label
          in
          Ident.Tbl.add t.ranges target_ident range;
          Some range
        in
        (* CR mshinwell: Not quite right.  The symbol is actually in the
           defining expression, so it probably shouldn't be an argument to
           [Available_range.create_phantom]. *)
        (* CR-someday mshinwell: For the moment we ignore the scope of
           "alias" phantom ranges (Iphantom_var).  We should probably clip
           their ranges to the defining variable's range. *)
        match range.defining_expr with
        | Iphantom_const (Uconst_ref (symbol, _defining_expr)) ->
          (* It's not actually a "fun_name", but the mangling is the same.
             This should go away if we switch to [Symbol.t] everywhere. *)
          let symbol = Name_laundry.fun_name_to_symbol symbol in
          create_new_range ~defining_expr:(Symbol symbol)
        | Iphantom_read_symbol_field (
            Uconst_ref (symbol, _defining_expr), field) ->
          let defining_expr : phantom_defining_expr =
            Read_symbol_field {
              symbol = Name_laundry.fun_name_to_symbol symbol;
              field;
            }
          in
          create_new_range ~defining_expr
        | Iphantom_read_symbol_field _ ->
          Misc.fatal_errorf "Available_ranges: unknown Clambda constant \
            pattern for Iphantom_read_symbol_field"
        | Iphantom_const (Uconst_int i)
        | Iphantom_const (Uconst_ptr i) ->
          create_new_range ~defining_expr:(Int i)
        | Iphantom_var defining_ident ->
          begin match resolve_range defining_ident with
          | None -> None
          | Some range ->
            Ident.Tbl.add t.ranges target_ident range;
            Some range
          end
        | Iphantom_read_var_field (defining_ident, field) ->
          begin match resolve_range defining_ident with
          | None -> None
          | Some range ->
            let range = Available_range.create_read_field range ~field in
            Ident.Tbl.add t.ranges target_ident range;
            Some range
          end
        | Iphantom_offset_var (defining_ident, offset_in_words) ->
          begin match resolve_range defining_ident with
          | None -> None
          | Some range ->
            let range =
              Available_range.create_offset_pointer range
                ~offset_in_words
            in
            Ident.Tbl.add t.ranges target_ident range;
            Some range
          end
  in
  Ident.iter (fun target_ident _range ->
      ignore ((resolve_range target_ident) : Available_range.t option))
    phantom_ranges;
  t, { fundecl with L.fun_body = first_insn; }

let create ~fundecl ~phantom_ranges =
  if not !Clflags.debug then
    let t =
      { ranges = Ident.Tbl.create 1;
      }
    in
    t, fundecl
  else
    create ~fundecl ~phantom_ranges

type label_classification =
  | Start of {
      end_pos : Linearize.label;
      location : Available_subrange.location;
    }
  | End

let classify_label t label =
  Ident.Tbl.fold (fun ident range result ->
      Available_range.fold range ~init:result
        ~f:(fun result ~available_subrange:subrange ->
          if Available_subrange.start_pos subrange = label then
            let location = Available_subrange.location subrange in
            let end_pos = Available_subrange.end_pos subrange in
            (Start { end_pos; location; }, ident, subrange) :: result
          else if Available_subrange.end_pos subrange = label then
            (End, ident, subrange) :: result
          else
            result))
    t.ranges
    []

let rewrite_labels t ~env =
  let ranges =
    Ident.Tbl.map t.ranges (fun range ->
      Available_range.rewrite_labels range ~env)
  in
  { ranges; }
