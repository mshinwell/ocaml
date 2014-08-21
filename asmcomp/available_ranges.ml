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
     : start_pos:L.label
    -> end_pos:L.label
    -> reg:Reg.t
    -> t

  val start_pos : t -> L.label
  val end_pos : t -> L.label
  val reg : t -> Reg.t
end = struct
  type t = {
    start_pos : L.label;
    (* CR mshinwell: we need to check exactly what happens with function
       epilogues, including returns in the middle of functions. *)
    end_pos : L.label;
    reg : Reg.t;
  }

  let create ~start_pos ~end_pos ~reg = { start_pos; end_pos; reg; }

  let start_pos t = t.start_pos
  let end_pos t = t.end_pos
  let reg t = t.reg
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
  function_name : string;
  start_of_function_label : Linearize.label;
}

let function_name t = t.function_name

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

let insert_label_after ~insn =
  match insn.L.next.L.desc with
  (* CR-someday mshinwell: unfortunately this breaks the ordering assumption
     on labels. *)
(*  | L.Llabel label -> label  (* don't add unnecessary labels *) *)
  | L.Llabel _
  | L.Lend | L.Lop _ | L.Lreloadretaddr | L.Lreturn | L.Lbranch _
  | L.Lcondbranch _ | L.Lcondbranch3 _ | L.Lswitch _ | L.Lsetuptrap _
  | L.Lpushtrap | L.Lpoptrap | L.Lraise _ ->
    let label = L.new_label () in
    let insn' =
      { insn with L.
        desc = L.Llabel label;
        arg = [| |];
        res = [| |];
      }
    in
    insn.L.next <- insn';
    label

let births_and_deaths ~insn ~prev_insn =
  let births =
    match prev_insn with
    | None -> insn.L.available_before
    | Some prev_insn ->
      Reg.Set.diff insn.L.available_before prev_insn.L.available_before
  in
  let deaths =
    match prev_insn with
    | None -> Reg.Set.empty
    | Some prev_insn ->
      Reg.Set.diff prev_insn.L.available_before insn.L.available_before
  in
  births, deaths

let rec process_instruction t ~insn ~prev_insn ~open_subrange_start_positions =
  let births, deaths = births_and_deaths ~insn ~prev_insn in
  let pos =
    lazy (  (* avoid creating unnecessary labels *)
      match prev_insn with
      | None -> t.start_of_function_label
      | Some prev_insn -> insert_label_after ~insn:prev_insn)
  in
  Reg.Set.fold (fun reg () ->
      let start_pos =
        try Reg.Map.find reg open_subrange_start_positions
        with Not_found -> assert false
      in
      let end_pos = Lazy.force pos in
      let subrange = Available_subrange.create ~start_pos ~end_pos ~reg in
      add_subrange t ~subrange)
    deaths
    ();
  let open_subrange_start_positions =
    Reg.Set.fold (fun reg open_subrange_start_positions ->
        assert (not (Reg.Map.mem reg open_subrange_start_positions));
        Reg.Map.add reg (Lazy.force pos) open_subrange_start_positions)
      births
      (Reg.Map.filter (fun reg _start_pos -> not (Reg.Set.mem reg deaths))
        open_subrange_start_positions)
  in
  match insn.L.desc with
  | L.Lend -> ()
  | L.Lop _ | L.Lreloadretaddr | L.Lreturn | L.Llabel _ | L.Lbranch _
  | L.Lcondbranch _ | L.Lcondbranch3 _ | L.Lswitch _ | L.Lsetuptrap _
  | L.Lpushtrap | L.Lpoptrap | L.Lraise _ ->
    process_instruction t ~insn:insn.L.next ~prev_insn:(Some insn)
      ~open_subrange_start_positions

let create ~fundecl =
  let t =
    { ranges = Ident.empty;
      function_name = fundecl.L.fun_name;
      start_of_function_label = Linearize.new_label ();
    }
  in
  process_instruction t ~insn:fundecl.L.fun_body ~prev_insn:None
    ~open_subrange_start_positions:Reg.Map.empty;
  t

let start_of_function_label t = t.start_of_function_label
