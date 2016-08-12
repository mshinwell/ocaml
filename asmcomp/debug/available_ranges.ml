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
    | Reg of Reg.t * is_parameter * 'a
    | Phantom of Clambda.ulet_provenance * is_parameter * 'a phantom

  and 'a phantom =
    | Const_int of int
    | Const_symbol of Symbol.t
    | Read_symbol_field of { symbol : Symbol.t; field : int; }
    | Read_field of { address : 'a location; field : int; }
    | Offset_pointer of { address : 'a location; offset_in_words : int; }

  val create
     : reg:Reg.t
    -> is_parameter:int option
    -> start_insn:L.instruction
    -> start_pos:L.label
    -> end_pos:L.label
    -> end_pos_offset:int option
    -> t

  val create_phantom
     : provenance:Clambda.ulet_provenance
    -> defining_expr:L.instruction phantom
    -> start_pos:Linearize.label
    -> end_pos:Linearize.label
    -> t

  val create_from_existing
     : t
    -> location_map:(L.instruction location -> L.instruction location)
    -> t

  val start_pos : t -> L.label
  val end_pos : t -> L.label
  val end_pos_offset : t -> int option
  val location : t -> unit location
  val is_parameter : t -> is_parameter
  val offset_from_stack_ptr_in_bytes : t -> int option

  val rewrite_labels : t -> env:int Numbers.Int.Map.t -> t
end = struct
  type 'a location =
    | Reg of Reg.t * is_parameter * 'a
    | Phantom of Clambda.ulet_provenance * is_parameter * 'a phantom

  and 'a phantom =
    | Const_int of int
    | Const_symbol of Symbol.t
    | Read_symbol_field of { symbol : Symbol.t; field : int; }
    | Read_field of { address : 'a location; field : int; }
    | Offset_pointer of { address : 'a location; offset_in_words : int; }

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

  let create ~(reg : Reg.t) ~is_parameter ~(start_insn : Linearize.instruction)
        ~start_pos ~end_pos ~end_pos_offset =
    match start_insn.desc with
    | L.Lcapture_stack_offset _ | L.Llabel _ ->
      begin match start_insn.desc with
      | L.Lcapture_stack_offset _ ->
        assert (Array.length start_insn.arg = 1);
        (* CR mshinwell: review assertions, maybe less useful now *)
        (*assert (reg.name = start_insn.arg.(0).Reg.name);*)
        assert (reg.loc = start_insn.arg.(0).Reg.loc)
      | _ -> ()
      end;
      let is_parameter =
        match is_parameter with
        | None -> Local
        | Some index -> Parameter { index; }
      in
      { start_insn = Reg (reg, is_parameter, start_insn);
        start_pos;
        end_pos;
        end_pos_offset;
      }
    | _ -> failwith "Available_subrange.create"

  let create_phantom ~provenance ~defining_expr ~start_pos ~end_pos =
    (* CR-someday mshinwell: when inlining a function, mark [Let]s binding
       function parameters so that they turn into "parameter" phantom lets. *)
    { start_insn = Phantom (provenance, Local, defining_expr);
      start_pos;
      end_pos;
      end_pos_offset = None;
    }

  let create_from_existing t ~location_map =
    { t with start_insn = location_map t.start_insn; }

  let start_pos t = t.start_pos
  let end_pos t = t.end_pos
  let end_pos_offset t = t.end_pos_offset

  let location t : unit location =
    let rec convert_location (start_insn : start_insn_or_phantom)
          : unit location =
      match start_insn with
      | Reg (reg, is_parameter, _insn) -> Reg (reg, is_parameter, ())
      | Phantom (provenance, is_parameter, defining_expr) ->
        Phantom (provenance, is_parameter, convert_phantom defining_expr)
    and convert_phantom (phantom : _ phantom) : unit phantom =
      match phantom with
      | Read_field { address; field; } ->
        let address = convert_location address in
        Read_field { address; field; }
      | Offset_pointer { address; offset_in_words; } ->
        let address = convert_location address in
        Offset_pointer { address; offset_in_words; }
      | Const_int i -> Const_int i
      | Const_symbol symbol -> Const_symbol symbol
      | Read_symbol_field { symbol; field; } ->
        Read_symbol_field { symbol; field; }
    in
    convert_location t.start_insn

  let is_parameter t =
    match t.start_insn with
    | Reg (_reg, is_parameter, _start_insn) -> is_parameter
    | Phantom (_provenance, is_parameter, _defining_expr) -> is_parameter

  let offset_from_stack_ptr_in_bytes t =
    let rec offset (start_insn : start_insn_or_phantom) =
      match start_insn with
      | Reg (_reg, _, insn) ->
        begin match insn.L.desc with
        | L.Lcapture_stack_offset offset -> !offset
        | L.Llabel _ ->
(* CR mshinwell: resurrect assertion *)
(*          assert (not (Reg.assigned_to_stack reg)); *)
          None
        | _ -> assert false
        end
      | Phantom (_, _, phantom) ->
        match phantom with
        | Const_int _ | Const_symbol _ | Read_symbol_field _ -> None
        | Read_field { address; _ } | Offset_pointer { address; _ } ->
          offset address
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

  val create_from_existing
     : t
     -> location_map:(L.instruction Available_subrange.location
        -> L.instruction Available_subrange.location)
     -> t

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
  }

  let create () = { subranges = []; min_pos = None; max_pos = None; } 

  let create_from_existing t ~location_map =
    let subranges =
      List.map (fun subrange ->
          Available_subrange.create_from_existing subrange ~location_map)
        t.subranges
    in
    { t with subranges; }

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

module Make (S : sig
  module Key : sig
    type t

    module Map : Map.S with type key := t
    module Set : Set.S with type elt := t

    val assert_valid : t -> unit
    val needs_stack_offset_capture : t -> Reg.t option
  end

  val available_before : L.instruction -> Key.Set.t

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
    let label = lazy (Cmm.new_label ()) in
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
        let end_pos = Lazy.force label in
        let end_pos_offset = S.end_pos_offset ~prev_insn:!prev_insn ~key in
        match
          S.create_subrange ~fundecl ~key ~start_pos ~start_insn ~end_pos
            ~end_pos_offset
        with
        | None -> ()
        | Some (subrange, ident) -> add_subrange t ~ident ~subrange)
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
        phantom_available_before = insn.phantom_available_before;
      })
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
            | None -> Lazy.force label_insn
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
          KM.add key (Lazy.force label, new_insn) open_subrange_start_insns)
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

  let create_subrange ~fundecl:_ ~key:reg ~start_pos ~start_insn ~end_pos
        ~end_pos_offset =
    match RD.debug_info reg with
    | None -> None
    | Some debug_info ->
      let subrange =
        Available_subrange.create ~reg:(RD.reg reg)
          ~is_parameter:(RD.Debug_info.which_parameter debug_info)
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

  let available_before (insn : L.instruction) =
    insn.phantom_available_before

  let end_pos_offset ~prev_insn:_ ~key:_ = None

  let create_subrange ~fundecl ~key ~start_pos ~start_insn:_ ~end_pos
        ~end_pos_offset:_ =
    match Ident.Map.find key fundecl.L.fun_phantom_lets with
    | exception Not_found ->
      (* The range was filtered by [Resolve_phantom_lets]. *)
      None
    | provenance, defining_expr ->
      let convert_defining_expr defining_expr =
        let module AS = Available_subrange in
        match (defining_expr : Mach.phantom_defining_expr) with
        | Iphantom_const_int i -> Some (AS.Const_int i)
        | Iphantom_const_symbol symbol -> Some (AS.Const_symbol symbol)
        | Iphantom_read_symbol_field (symbol, field) ->
          Some (AS.Read_symbol_field { symbol; field; })
        | Iphantom_var _
        | Iphantom_read_var_field _
        | Iphantom_offset_var _ -> None
          (* CR-someday mshinwell: To do this properly, we'd have to intersect
             the ranges.  For the moment we treat these cases separately (see
             below). *)
      in
      let defining_expr = convert_defining_expr defining_expr in
      match provenance with
      | None -> None
      | Some provenance ->
        match defining_expr with
        | None -> None
        | Some defining_expr ->
          let subrange =
            Available_subrange.create_phantom ~provenance ~defining_expr
              ~start_pos ~end_pos
          in
          Some (subrange, key)
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
  (* (See CR-someday above.)  For the moment, ranges that are derived from
     others are treated naively: we just make them available iff the
     variables they ultimately derive from are available. *)
  Ident.Map.iter (fun ident (provenance, defining_expr) ->
      let rec resolve_range provenance defining_expr =
        match (defining_expr : Mach.phantom_defining_expr) with
        | Iphantom_const_int _
        | Iphantom_const_symbol _
        | Iphantom_read_symbol_field _ -> None
        | Iphantom_var var ->
          begin match Ident.Tbl.find t.ranges var with
          | exception Not_found -> None
          | range -> Some range
          end
        | Iphantom_read_var_field (defining_expr, field) ->
          begin match resolve_range provenance defining_expr with
          | None -> None
          | Some range ->
            let range =
              Available_range.create_from_existing range
                ~location_map:(fun address ->
                  Phantom (provenance, Local, Read_field { address; field; }))
            in
            Some range
          end
        | Iphantom_offset_var (defining_expr, offset_in_words) ->
          begin match resolve_range provenance defining_expr with
          | None -> None
          | Some range ->
            let range =
              Available_range.create_from_existing range
                ~location_map:(fun address ->
                  Phantom (provenance, Local,
                    Offset_pointer { address; offset_in_words; }))
            in
            Some range
          end
      in
      match provenance with
      | None -> ()
      | Some provenance ->
        match resolve_range provenance defining_expr with
        | None -> ()
        | Some range ->
          Ident.Tbl.add t.ranges ident range)
    fundecl.L.fun_phantom_lets;
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
