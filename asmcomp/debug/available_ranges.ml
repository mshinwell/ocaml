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
[@@@ocaml.warning "-60"]  (* CR mshinwell: remove once warning fixed *)

module L = Linearize

let rewrite_label env label =
  match Numbers.Int.Map.find label env with
  | exception Not_found -> label
  | label -> label

(* CR-soon mshinwell: pull this type forward so other passes can use it *)
type is_parameter =
  | Local
  | Parameter of { index : int; }

module Available_subrange : sig
  type t

  type 'a location =
    | Reg of Reg.t * 'a
    | Phantom

  val create
     : reg:Reg.t
    -> start_insn:L.instruction
    -> start_pos:L.label
    -> end_pos:L.label
    -> end_pos_offset:int option
    -> t

  val create_phantom
     : start_pos:Linearize.label
    -> end_pos:Linearize.label
    -> t

  val start_pos : t -> L.label
  val end_pos : t -> L.label
  val end_pos_offset : t -> int option
  val location : t -> unit location
  val offset_from_stack_ptr_in_bytes : t -> int option

  val rewrite_labels : t -> env:int Numbers.Int.Map.t -> t
end = struct
  type 'a location =
    | Reg of Reg.t * 'a
    | Phantom

  type start_insn_or_phantom = L.instruction location

  type t = {
    (* CR-soon mshinwell: find a better name for [start_insn] *)
    start_insn : start_insn_or_phantom;
    start_pos : L.label;
    (* CR mshinwell: we need to check exactly what happens with function
       epilogues, including returns in the middle of functions. *)
    end_pos : L.label;
    end_pos_offset : int option;
  }

  let create ~(reg : Reg.t) ~(start_insn : Linearize.instruction)
        ~start_pos ~end_pos ~end_pos_offset =
    match start_insn.desc with
    | L.Lcapture_stack_offset _ | L.Llabel _ ->
      begin match start_insn.desc with
      | L.Lcapture_stack_offset _ ->
        assert (Array.length start_insn.arg = 1);
        (* CR mshinwell: review assertions, maybe less useful now *)
        (*assert (reg.name = start_insn.arg.(0).Reg.name);*)
        (* CR mshinwell: Why bother storing the start instruction when it's
           a label? *)
        assert (reg.loc = start_insn.arg.(0).Reg.loc)
      | _ -> ()
      end;
      { start_insn = Reg (reg, start_insn);
        start_pos;
        end_pos;
        end_pos_offset;
      }
    | _ -> failwith "Available_subrange.create"

  let create_phantom ~start_pos ~end_pos =
    { start_insn = Phantom;
      start_pos;
      end_pos;
      end_pos_offset = None;
    }

  let start_pos t = t.start_pos
  let end_pos t = t.end_pos
  let end_pos_offset t = t.end_pos_offset

  let location t : unit location =
    let convert_location (start_insn : start_insn_or_phantom) : unit location =
      match start_insn with
      | Reg (reg, _insn) -> Reg (reg, is_parameter)
      | Phantom -> Phantom
    in
    convert_location t.start_insn

  let offset_from_stack_ptr_in_bytes t =
    let offset (start_insn : start_insn_or_phantom) =
      match start_insn with
      | Reg (_reg, insn) ->
        begin match insn.L.desc with
        | L.Lcapture_stack_offset offset -> !offset
        | L.Llabel _ ->
(* CR mshinwell: resurrect assertion *)
(*          assert (not (Reg.assigned_to_stack reg)); *)
          None
        | _ -> assert false
        end
      | Phantom -> None
    in
    offset t.start_insn

  let rewrite_labels t ~env =
    { t with
      start_pos = rewrite_label env t.start_pos;
      end_pos = rewrite_label env t.end_pos;
    }
end

type type_info =
  | From_cmt_file
  | Phantom of Clambda.ulet_provenance option * Mach.phantom_defining_expr

module Available_range : sig
  type t

  val create
     : type_info:type_info
    -> is_parameter:is_parameter
    -> t

  val type_info : t -> type_info
  val is_parameter : t -> is_parameter
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
    type_info : type_info;
    is_parameter : is_parameter;
  }

  let create () = { subranges = []; min_pos = None; max_pos = None; }

  let type_info t = t.type_info
  let is_parameter t = t.is_parameter

  let add_subrange t ~subrange =
    let start_pos = Available_subrange.start_pos subrange in
    let end_pos = Available_subrange.end_pos subrange in
    (* This is dubious, but should be correct by virtue of the way label
       counters are allocated (see linearize.ml) and the fact that, below,
       we go through the code from lowest (code) address to highest.  As
       such the label with the highest integer value should be the one with
       the highest address, and vice-versa.  (Note that we also exploit the
       ordering when constructing location lists, to ensure that they are
       sorted in increasing program counter order by start address.
       However by that stage [Coalesce_labels] has run.) *)
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
    (* Look at the first subrange, since a later subrange for the same
       variable might not be marked as a parameter. *)
    match List.rev t.subranges with
    | [] -> assert false
    | subrange::_ -> Available_subrange.is_parameter subrange

  let extremities t =
    (* We ignore any [end_pos_offsets] here; should be ok. *)
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

module Make (S : sig
  module Key : sig
    type t

    module Map : Map.S with type key := t
    module Set : Set.S with type elt := t

    val assert_valid : t -> unit
    val needs_stack_offset_capture : t -> Reg.t option
  end

  val available_before : L.instruction -> Key.Set.t

  val create_range
     : fundecl:L.fundecl
    -> key:Key.t
    -> start_insn:L.instruction
    -> (type_info * is_parameter) option

  val create_subrange
     : fundecl:L.fundecl
    -> key:Key.t
    -> start_pos:L.label
    -> start_insn:L.instruction
    -> end_pos:L.label
    -> end_pos_offset:int option
    -> (Available_subrange.t * Ident.t) option

  val end_pos_offset
     : prev_insn:L.instruction option
    -> key:Key.t
    -> int option
end) = struct
  module KM = S.Key.Map
  module KS = S.Key.Set

  (* Imagine that the program counter is exactly at the start of [insn]; it has
     not yet been executed.  This function calculates which available subranges
     are to start at that point, and which are to stop.  [prev_insn] is the
     instruction immediately prior to [insn], if such exists. *)
  let births_and_deaths ~(insn : L.instruction)
        ~(prev_insn : L.instruction option) =
    (* Available subranges are allowed to cross points at which the stack
       pointer changes, since we reference the stack slots as an offset from
       the CFA, not from the stack pointer.

       We avoid generating ranges that overlap, since this confuses lldb.
       This pass may generate ranges that are the same as other ranges,
       but those are deduped in [Dwarf].
    *)
    let proto_births =
      match prev_insn with
      | None -> S.available_before insn
      | Some prev_insn ->
        KS.diff (S.available_before insn) (S.available_before prev_insn)
    in
    let proto_deaths =
      match prev_insn with
      | None -> KS.empty
      | Some prev_insn ->
        KS.diff (S.available_before prev_insn) (S.available_before insn)
    in
    let restart_ranges =
      KS.cardinal proto_births <> 0 || KS.cardinal proto_deaths <> 0
    in
    let births =
      match prev_insn with
      | None -> S.available_before insn
      | Some _prev_insn ->
        if not restart_ranges then
          proto_births
        else
          S.available_before insn
    in
    let deaths =
      match prev_insn with
      | None -> KS.empty
      | Some prev_insn ->
        if not restart_ranges then
          proto_deaths
        else
          S.available_before prev_insn
    in
    births, deaths

  let rec process_instruction t ~fundecl ~first_insn ~(insn : L.instruction)
        ~prev_insn ~open_subrange_start_insns =
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
    let label = Cmm.new_label () in
    let used_label = ref false in
    (* As a result of the code above to restart subranges, we may have
       a register occurring in both [births] and [deaths]; and we would
       like the register to have an open subrange from this point.  It
       follows that we should process deaths before births. *)
    KS.fold (fun (key : S.Key.t) () ->
        S.Key.assert_valid key;
        let start_pos, start_insn =
          try KM.find key open_subrange_start_insns
          with Not_found -> assert false
        in
        let end_pos = label in
        used_label := true;
        let end_pos_offset = S.end_pos_offset ~prev_insn:!prev_insn ~key in
        let range =
          match Ident.Tbl.find t.ranges ident with
          | range -> Some range
          | exception Not_found ->
            match S.create_range ~fundecl ~key ~start_insn with
            | None -> None
            | Some (type_info, is_parameter) ->
              let range = Available_range.create ~type_info ~is_parameter in
              Ident.Tbl.add t.ranges ident range;
              Some range
        in
        match range with
        | None -> ()
        | Some range ->
          match
            S.create_subrange ~fundecl ~key ~start_pos ~start_insn ~end_pos
              ~end_pos_offset
          with
          | None -> ()
          | Some (subrange, ident) ->
            Available_range.add_subrange range ~subrange)
      deaths
      ();
    let label_insn =
      { L.
        desc = L.Llabel label;
        next = insn;
        arg = [| |];
        res = [| |];
        dbg = Debuginfo.none;
        live = Reg.Set.empty;
        available_before = insn.available_before;
        phantom_available_before = insn.phantom_available_before;
      }
    in
    let open_subrange_start_insns =
      let open_subrange_start_insns =
        (KM.filter (fun reg _start_insn -> not (KS.mem reg deaths))
          open_subrange_start_insns)
      in
      KS.fold (fun key open_subrange_start_insns ->
          (* We only need [Lcapture_stack_offset] in the case where the register
             is assigned to the stack.  (It enables us to determine what the
             stack offset will be at that point.) *)
          let new_insn =
            match S.Key.needs_stack_offset_capture key with
            | None -> label_insn
            | Some reg ->
              let new_insn =
                { L.
                  desc = L.Lcapture_stack_offset (ref None);
                  next = insn;
                  arg = [| reg |];
                  res = [| |];
                  dbg = Debuginfo.none;
                  live = Reg.Set.empty;
                  available_before = insn.available_before;
                  phantom_available_before = insn.phantom_available_before;
                }
              in
              insert_insn ~new_insn;
              new_insn
          in
          used_label := true;
          KM.add key (label, new_insn) open_subrange_start_insns)
        births
        open_subrange_start_insns
    in
    begin if !used_label then
      insert_insn ~new_insn:label_insn
    end;
    let first_insn = !first_insn in
    match insn.L.desc with
    | L.Lend -> first_insn
    | L.Lprologue | L.Lop _ | L.Lreloadretaddr | L.Lreturn | L.Llabel _
    | L.Lbranch _ | L.Lcondbranch _ | L.Lcondbranch3 _ | L.Lswitch _
    | L.Lsetuptrap _ | L.Lpushtrap | L.Lpoptrap | L.Lraise _
    | L.Lcapture_stack_offset _ ->
      process_instruction t ~fundecl ~first_insn ~insn:insn.L.next
        ~prev_insn:(Some insn) ~open_subrange_start_insns

  let process_instructions t ~fundecl ~first_insn =
    process_instruction t ~fundecl ~first_insn ~insn:first_insn
      ~prev_insn:None ~open_subrange_start_insns:KM.empty
end

module Make_ranges = Make (struct
  module RD = Reg_with_debug_info

  (* By the time this pass has run, register stamps are irrelevant; indeed,
     there may be multiple registers with different stamps assigned to the
     same location.  As such, we quotient register sets by the equivalence
     relation that identifies two registers iff they have the same name and
     location. *)
  module Key = struct
    type t = RD.t

    (* CR mshinwell: check this *)
    let assert_valid (_t : t) = ()
     (*  assert (t.name <> None);  (* cf. [Available_filtering] *) *)

    module Map = RD.Map_distinguishing_names_and_locations
    module Set = RD.Set_distinguishing_names_and_locations

    let needs_stack_offset_capture t =
      if RD.assigned_to_stack t then Some (RD.reg t) else None
  end

  (* CR mshinwell: improve efficiency *)
  let available_before (insn : L.instruction) =
    match insn.available_before with
    | Unreachable -> Key.Set.empty
    | Ok available_before -> Key.Set.of_list (RD.Set.elements available_before)

  let end_pos_offset ~prev_insn ~key:reg =
    (* If the range is for a register destroyed by a call (which for
       calls to OCaml functions means any non-spilled register) and which
       ends immediately after a call instruction, move the end of the
       range back very slightly.  The effect is that the register is seen
       in the debugger as available when standing on the call instruction
       but unavailable when we are in the callee (and move to the previous
       frame). *)
    (* CR-someday mshinwell: I wonder if this should be more
       conservative for Iextcall.  If the C callee is compiled with
       debug info then it should describe where any callee-save
       registers have been saved, so when we step back to the OCaml frame
       in the debugger, the unwinding procedure should get register
       values correct.  (I think.)  However if it weren't compiled with
       debug info, this wouldn't happen, and when looking back up into
       the OCaml frame I suspect registers would be wrong.  This may
       not be a great problem once libmonda is hardened, although it
       is possible for this to be subtle and misleading (e.g. an integer
       value being 1 instead of 2 or something.) *)
    match prev_insn with
    | None -> None
    | Some prev_insn ->
      match prev_insn.L.desc with
      | Lop ((Icall_ind _ | Icall_imm _ | Iextcall _) as op) ->
        let destroyed_locations =
          Array.map (fun (reg : Reg.t) -> reg.loc)
            (Proc.destroyed_at_oper (Mach.Iop op))
        in
        if Array.mem (RD.location reg) destroyed_locations then
          Some (-1)
        else
          None
      | _ -> None

  let create_range ~fundecl:_ ~key:reg ~start_insn =
    match RD.debug_info reg with
    | None -> None
    | Some debug_info ->
      let is_parameter = RD.Debug_info.which_parameter debug_info in
      Some (From_cmt_file, is_parameter)

  let create_subrange ~fundecl:_ ~key:reg ~start_pos ~start_insn ~end_pos
        ~end_pos_offset =
    match RD.debug_info reg with
    | None -> None
    | Some debug_info ->
      let subrange =
        Available_subrange.create ~reg:(RD.reg reg)
          ~start_pos ~start_insn
          ~end_pos ~end_pos_offset
      in
      let ident = RD.Debug_info.holds_value_of debug_info in
      Some (subrange, ident)
end)

module Make_phantom_ranges = Make (struct
  module Key = struct
    include Ident

    let assert_valid _t = ()
    let needs_stack_offset_capture _ = None
  end

  let available_before (insn : L.instruction) = insn.phantom_available_before

  let end_pos_offset ~prev_insn:_ ~key:_ = None

  let create_range ~fundecl ~key ~start_insn:_ =
    match Ident.Map.find key fundecl.L.fun_phantom_lets with
    | exception Not_found ->
      Misc.fatal_errorf "Available_ranges.Make_phantom_ranges.create_range: \
          phantom identifier occurs in [phantom_available_before] but not in \
          [fun_phantom_lets]: %a"
        Key.print key
    | provenance, defining_expr ->
      (* CR-someday mshinwell: Presumably the "Local" only changes to indicate
         a parameter when we can represent the inlined frames properly in DWARF.
         (Otherwise the function into which an inlining occurs ends up having
         more parameters than it should in the debugger.)
      *)
      Some (Phantom (provenance, defining_expr), Local)

  let create_subrange ~fundecl:_ ~key:_ ~start_pos ~start_insn:_ ~end_pos
        ~end_pos_offset:_ =
    (* Ranges for phantom identifiers are emitted as contiguous blocks
        which are designed to approximately indicate their scope.
        Some such phantom identifiers' values may ultimately be derived
        from the values of normal identifiers (e.g. "Read_var_field") and
        thus will be unavailable when those normal identifiers are
        unavailable.  This effective intersecting of available ranges
        is handled automatically in the debugger since we emit DWARF that
        explains properly how the phantom identifiers relate to other
        (normal or phantom) ones. *)
    Available_subrange.create_phantom ~start_pos ~end_pos
end)

let create ~fundecl =
  let t = { ranges = Ident.Tbl.create 42; } in
  let first_insn =
    let first_insn = fundecl.L.fun_body in
    Make_ranges.process_instructions t ~fundecl ~first_insn
  in
  let first_insn =
    Make_phantom_ranges.process_instructions t ~fundecl ~first_insn
  in
  t, { fundecl with L.fun_body = first_insn; }

let create ~fundecl =
  if not !Clflags.debug then
    let t =
      { ranges = Ident.Tbl.create 1;
      }
    in
    t, fundecl
  else
    create ~fundecl

type label_classification =
  | Start of {
      end_pos : Linearize.label;
      location : unit Available_subrange.location;
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
