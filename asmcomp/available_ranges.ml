(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                  Mark Shinwell, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright 2014, Jane Street Holding                                *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

module L = Linearize

module Available_subrange : sig
  type t

  val create
     : start_insn:L.instruction  (* must be [Lavailable_subrange] *)
    -> start_pos:L.label
    -> end_pos:L.label
    -> t

  val start_pos : t -> L.label
  val end_pos : t -> L.label
  val reg : t -> Reg.t
  val offset_from_stack_ptr : t -> int option
end = struct
  type t = {
    start_insn : L.instruction;
    start_pos : L.label;
    (* CR mshinwell: we need to check exactly what happens with function
       epilogues, including returns in the middle of functions. *)
    end_pos : L.label;
  }

  let create ~start_insn ~start_pos ~end_pos =
    match start_insn.L.desc with
    | L.Lavailable_subrange _ -> { start_insn; start_pos; end_pos; }
    | _ -> failwith "Available_subrange.create"

  let start_pos t = t.start_pos
  let end_pos t = t.end_pos

  let reg t =
    assert (Array.length t.start_insn.L.arg = 1);
    t.start_insn.L.arg.(0)

  let offset_from_stack_ptr t =
    match t.start_insn.L.desc with
    | L.Lavailable_subrange offset -> !offset
    | _ -> assert false
end

module Available_range : sig
  type t

  val create : unit -> t
  val is_parameter : t -> bool
  val add_subrange : t -> subrange:Available_subrange.t -> unit
  val extremities : t -> L.label * L.label

  val fold
     : t
    -> init:'a
    -> f:('a -> available_subrange:Available_subrange.t -> 'a)
    -> 'a
end = struct
  type t = {
    mutable subranges : Available_subrange.t list;
    mutable min_pos : L.label option;
    mutable max_pos : L.label option;
  }

  let create () = { subranges = []; min_pos = None; max_pos = None; } 

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
      let reg = Available_subrange.reg subrange in
      reg.Reg.is_parameter

  let extremities t =
    match t.min_pos, t.max_pos with
    | Some min, Some max -> min, max
    | Some _, None | None, Some _ -> assert false
    | None, None -> failwith "Available_ranges.extremities on empty range"

  let fold t ~init ~f =
    List.fold_left (fun acc available_subrange -> f acc ~available_subrange)
      init
      t.subranges
end

type t = {
  mutable ranges : Available_range.t Ident.tbl;
}

let fold t ~init ~f =
  Ident.fold_all (fun ident range acc ->
      let is_unique =
        List.length (Ident.find_all (Ident.name ident) t.ranges) <= 1
      in
      f acc ~ident ~is_unique ~range)
    t.ranges
    init

let ident_from_reg ~reg =
  match Reg.Raw_name.to_ident reg.Reg.raw_name with
  | Some ident -> ident
  | None -> assert false  (* most likely a bug in available_regs.ml *)

let add_subrange t ~subrange =
  let ident = ident_from_reg ~reg:(Available_subrange.reg subrange) in
  let range =
    try Ident.find_same ident t.ranges
    with Not_found -> begin
      let range = Available_range.create () in
      t.ranges <- Ident.add ident range t.ranges;
      range
    end
  in
  Available_range.add_subrange range ~subrange

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
    | None -> insn.L.available_before
    | Some prev_insn ->
      if not adjusts_sp then
        Reg.Set.diff insn.L.available_before prev_insn.L.available_before
      else
        insn.L.available_before
  in
  let deaths =
    match prev_insn with
    | None -> Reg.Set.empty
    | Some prev_insn ->
      if not adjusts_sp then
        Reg.Set.diff prev_insn.L.available_before insn.L.available_before
      else
        prev_insn.L.available_before
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

let create ~fundecl =
  let t =
    { ranges = Ident.empty;
    }
  in
  let first_insn =
    let first_insn = fundecl.L.fun_body in
    process_instruction t ~first_insn ~insn:first_insn ~prev_insn:None
      ~open_subrange_start_insns:Reg.Map.empty
  in
  (*
  Printf.printf "Available ranges for function: %s\n" fundecl.L.fun_name;
  fold t ~init:()
    ~f:(fun () ~ident ~is_unique ~range ->
        Printf.printf "  Identifier: %s (is unique? %s)\n"
          (Ident.unique_name ident) (if is_unique then "yes" else "no");
        Available_range.fold range ~init:()
          ~f:(fun () ~available_subrange ->
            Printf.printf "    Label range: %d -> %d  Register: %s (is param? %s)\n"
              (Available_subrange.start_pos available_subrange)    
              (Available_subrange.end_pos available_subrange)
              (Reg.name (Available_subrange.reg available_subrange))
              (if (Available_subrange.reg available_subrange)
                .Reg.is_parameter then "yes" else "no")));
  *)
  t, { fundecl with L.fun_body = first_insn; }
