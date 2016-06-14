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

let rewrite_label env label =
  match Numbers.Int.Map.find label env with
  | exception Not_found -> label
  | label -> label

module Available_subrange : sig
  type t

  type 'a location =
    | Reg of Reg.t * 'a
    | Phantom of Clambda.ulet_provenance * 'a phantom

  and 'a phantom =
    | Const_int of int
    | Const_symbol of Symbol.t
    | Read_symbol_field of { symbol : Symbol.t; field : int; }
    | Read_field of { address : 'a location; field : int; }
    | Offset_pointer of { address : 'a location; offset_in_words : int; }

  val create
     : reg:Reg.t
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
  val offset_from_stack_ptr_in_bytes : t -> int option

  val rewrite_labels : t -> env:int Numbers.Int.Map.t -> t
end = struct
  type 'a location =
    | Reg of Reg.t * 'a
    | Phantom of Clambda.ulet_provenance * 'a phantom

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

  let create ~(reg : Reg.t) ~(start_insn : Linearize.instruction)
        ~start_pos ~end_pos ~end_pos_offset =
    assert (reg.name <> None);
    match start_insn.desc with
    | L.Lcapture_stack_offset _ | L.Llabel _ ->
      begin match start_insn.desc with
      | L.Lcapture_stack_offset _ ->
        assert (Array.length start_insn.arg = 1);
        assert (reg.name = start_insn.arg.(0).Reg.name);
        assert (reg.shared.loc = start_insn.arg.(0).Reg.shared.loc)
      | _ -> ()
      end;
      { start_insn = Reg (reg, start_insn);
        start_pos;
        end_pos;
        end_pos_offset;
      }
    | _ -> failwith "Available_subrange.create"

  let create_phantom ~provenance ~defining_expr ~start_pos ~end_pos =
    { start_insn = Phantom (provenance, defining_expr);
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
      | Reg (reg, _insn) -> Reg (reg, ())
      | Phantom (provenance, defining_expr) ->
        Phantom (provenance, convert_phantom defining_expr)
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

  let offset_from_stack_ptr_in_bytes t =
    let rec offset (start_insn : start_insn_or_phantom) =
      match start_insn with
      | Reg (reg, insn) ->
        begin match insn.L.desc with
        | L.Lcapture_stack_offset offset -> !offset
        | L.Llabel _ ->
          assert (not (Reg.assigned_to_stack reg));
          None
        | _ -> assert false
        end
      | Phantom (_, phantom) ->
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
    | subrange::_ ->
      (* CR-soon mshinwell: Phantom ones could in theory be parameters too
         (e.g. a specialised arg maybe?), although I'm not sure that can
         happen at the moment since Flambda always marks removed arguments
         with Dead phantom lets. *)
      match Available_subrange.location subrange with
      | Reg (reg, _) -> reg.Reg.shared.Reg.is_parameter
      | Phantom _ -> None

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
       the CFA, not from the stack pointer. *)
    let births =
      match prev_insn with
      | None -> S.available_before insn
      | Some prev_insn ->
        KS.diff (S.available_before insn) (S.available_before prev_insn)
    in
    let deaths =
      match prev_insn with
      | None -> KS.empty
      | Some prev_insn ->
        KS.diff (S.available_before prev_insn) (S.available_before insn)
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
    let label = lazy (L.new_label ()) in
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
end

module Make_ranges = Make (struct
  (* By the time this pass has run, register stamps are irrelevant; indeed,
     there may be multiple registers with different stamps assigned to the
     same location.  As such, we quotient register sets by the equivalence
     relation that identifies two registers iff they have the same name and
     location. *)
  module Key = struct
    type t = Reg.t

    let assert_valid (t : t) =
        assert (t.name <> None);  (* cf. [Available_filtering] *)

    module Map = Reg.Map_distinguishing_names_and_locations
    module Set = Reg.Set_distinguishing_names_and_locations

    let needs_stack_offset_capture t =
      if Reg.assigned_to_stack t then Some t else None
  end

  (* CR mshinwell: improve efficiency *)
  let available_before (insn : L.instruction) =
    Key.Set.of_list (Reg.Set.elements insn.available_before)

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
      | Lop ((Icall_ind | Icall_imm _ | Iextcall _) as op) ->
        let destroyed_locations =
          Array.map (fun (reg : Reg.t) -> reg.shared.loc)
            (Proc.destroyed_at_oper (Mach.Iop op))
        in
        if Array.mem reg.Reg.shared.loc destroyed_locations then
          Some (-1)
        else
          None
      | _ -> None

  let create_subrange ~fundecl:_ ~key:reg ~start_pos ~start_insn ~end_pos
        ~end_pos_offset =
    let subrange =
      Available_subrange.create ~reg ~start_pos ~start_insn ~end_pos
        ~end_pos_offset
    in
    let ident =
      match reg.Reg.name with
      | Some name -> name
      | None -> assert false
    in
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
    let provenance, defining_expr =
      match Ident.Map.find key fundecl.L.fun_phantom_lets with
      | provenance_and_defining_expr -> provenance_and_defining_expr
      | exception Not_found ->
        Misc.fatal_errorf "Available_ranges.Make_phantom_ranges: cannot \
            find phantom-let range definition for %a"
          Ident.print key
    in
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
    Make_ranges.process_instruction t ~fundecl ~first_insn ~insn:first_insn
      ~prev_insn:None
      ~open_subrange_start_insns:
        Reg.Map_distinguishing_names_and_locations.empty
  in
  let first_insn =
    Make_phantom_ranges.process_instruction t ~fundecl ~first_insn
      ~insn:first_insn ~prev_insn:None
      ~open_subrange_start_insns:Ident.Map.empty
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
                  Phantom (provenance, Read_field { address; field; }))
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
                  Phantom (provenance,
                    Offset_pointer { address; offset_in_words; }))
            in
            Some range
          end
      in
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
