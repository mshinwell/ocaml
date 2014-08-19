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
     : start_pos:[ `Start_of_function | `At_label of L.label ]
    -> end_pos:L.label
    -> reg:Reg.t
    -> t

  val reg : t -> Reg.t
end = struct
  type t = {
    start_pos : [ `Start_of_function | `At_label of L.label ];
    (* CR mshinwell: we need to check exactly what happens with function
       epilogues, including returns in the middle of functions. *)
    end_pos : L.label;
    reg : Reg.t;
  }
end

module Available_range : sig
  type t

  val create : unit -> t
  val is_parameter : t -> bool
  val add_subrange : t -> subrange:Available_subrange.t -> unit
  val extremities : t -> [ `Start_of_function | `At_label of L.label ] * L.label
end = struct
  type t = {
    mutable subranges : Available_subrange.t list;
  }

  let create () = { subranges = []; } 
  let add_subrange t ~subrange = t.subranges <- subrange::t.subranges

  let is_parameter t =
    match t.subranges with
    | [] -> assert false
    | subrange::_ ->
      let reg = Available_subrange.reg subrange in
      reg.Reg.is_parameter
end

type t = {
  mutable ranges : Available_ranges.t Ident.tbl;
  function_name : string;
}

let function_name t = t.function_name

let fold t ~init ~f =
  Ident.fold_all (fun ident range acc ->
      let is_unique =
        List.length (Ident.find_all (Ident.name ident) t.ranges) <= 1
      in
      f acc ~ident ~is_unique ~range)
    t.ranges
    acc

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
  match insn.L.next with
  | L.Llabel label -> label  (* don't add unnecessary labels *)
  | L.Lend | L.Lop _ | L.Lreloadretaddr | L.Lreturn L.Lbranch _
  | L.Lcondbranch _ | L.Lcondbranch3 _ | L.Lswitch _ | L.Lsetuptrap _
  | L.Lpushtrap | L.Lpoptrap | L.Lraise ->
    let label = L.new_label () in
    let insn' =
      { insn with L.
        desc = L.Llabel llabel;
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
      | None -> `Start_of_function
      | Some prev_insn -> `At_label (insert_label_after ~insn:prev_insn))
  in
  let t =
    Reg.Set.fold (fun reg t ->
        let start_pos =
          try Reg.Map.find open_subrange_start_positions reg
          with Not_found -> assert false
        in
        let subrange =
          Available_subrange.create ~start_pos ~end_pos:(Lazy.force pos) ~reg
        in
        add_subrange t ~subrange)
      deaths
      t
  in
  let open_subrange_start_positions =
    Reg.Set.fold (fun reg open_subrange_start_positions ->
        assert (not (Reg.Map.mem reg open_subrange_start_positions));
        Reg.Map.add reg (Lazy.force pos) open_subrange_start_positions)
      births
      (Reg.Map.filter (fun reg _start_pos -> not (Reg.Set.mem reg deaths)))
  in
  match insn.L.desc with
  | L.Lend -> ()
  | L.Lop _ | L.Lreloadretaddr | L.Lreturn | L.Llabel _ | L.Lbranch _
  | L.Lcondbranch _ | L.Lcondbranch3 _ | L.Lswitch _ | L.Lsetuptrap _
  | L.Lpushtrap | L.Lpoptrap | L.Lraise ->
    process_instruction t ~insn:insn.L.next ~prev_insn:insn
      ~open_subrange_start_positions

let create ~fundecl =
  let t =
    { ranges = Ident.empty;
      function_name = fundecl.L.fun_name;
    }
  in
  process_instruction t ~insn:fundecl.L.fun_body ~prev_insn:None
    ~open_subrange_start_positions:Reg.Map.empty;
  t
