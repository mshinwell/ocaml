(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*         Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
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

open Dwarf_low_dot_std
open Dwarf_std_internal

(* CR mshinwell: we should rename "live range" to "available range"
   or something. *)

module Reg_map = struct
  include Reg.Map
  let add t ~key ~data = add key data t
  let remove t key = remove key t
  let find t reg = try Some (find reg t) with Not_found -> None
  let for_all t ~f = for_all f t
  let exists t ~f = exists f t
  let filter t ~f = filter f t
end

module Reg_set = struct
  include Reg.Set
  let mem t x = mem x t
  let iter t ~f = iter f t
  let fold t ~init ~f = fold (fun elt acc -> f acc elt) t init
  let filter t ~f = filter f t
  let to_list = elements
end

module One_live_range = struct
  module T = struct
    type t = {
      id : int;
      parameter_or_variable : [ `Parameter of string | `Variable ];
      reg : Reg.t;
      mutable ending_label : Linearize.label option;
      mutable canonical : [
        | `Canonical of [
          | `Starts_at_beginning_of_function
          | `Starts_at_label of Linearize.label
          ]
        | `Not_canonical
      ];
    }

    let compare t t' =
      Pervasives.compare t.id t'.id
  end

  include T

  module Set = struct
    include Set.Make (T)
    let fold t ~init ~f = fold (fun elt acc -> f acc elt) t init
    let to_list = elements
  end

  let set_ending_label t lbl =
    t.ending_label <- Some lbl

  let ending_label_of_t_exn t =
    match t.ending_label with
    | None -> assert false
    | Some l -> l

  let unique_id = ref 0  (* CR mshinwell: may not suffice for 32-bit *)

  let create ~canonical ~parameter_or_variable ~reg =
    let our_id = !unique_id in
    unique_id := !unique_id + 1;
    {
      id = our_id;
      parameter_or_variable;
      reg;
      ending_label = None;
      canonical;
    }

  let is_canonical t =
    match t.canonical with
    | `Canonical _ -> true
    | `Not_canonical -> false

  let make_canonical t ~starting_label =
    if is_canonical t then
      failwith "make_canonical: range is already canonical";
    t.canonical <- `Canonical (`Starts_at_label starting_label)

  let starting_label_of_t_exn t =
    match t.canonical with
    | `Not_canonical ->
      failwith "starting_label_of_t_exn on non-canonical available range"
    | `Canonical `Starts_at_beginning_of_function ->
      failwith "starting_label_of_t_exn on canonical available range whose \
                starting point is the beginning of the function"
    | `Canonical (`Starts_at_label label) -> label

  let code_for_starting_label t =
    Linearize.Llabel (starting_label_of_t_exn t)

  let code_for_ending_label t =
    Linearize.Llabel (ending_label_of_t_exn t)

  let parameter_or_variable t =
    t.parameter_or_variable

  let dwarf_tag t =
    match t.parameter_or_variable with
    | `Parameter _name -> Dwarf_low.Tag.formal_parameter
    | `Variable -> Dwarf_low.Tag.variable

  let reg_name t =
    match t.parameter_or_variable with
    | `Parameter name -> name
    | `Variable ->
      let name = Reg.name t.reg in
      let spilled_prefix = "spilled-" in
      if String.length name <= String.length spilled_prefix then
        name
      else if String.sub name 0 (String.length spilled_prefix) = spilled_prefix then
        String.sub name (String.length spilled_prefix)
          (String.length name - String.length spilled_prefix)
      else
        name

  let unique_name t = Printf.sprintf "%s__%d" (reg_name t) t.id

  let starts_at_beginning_of_function t =
    match t.canonical with
    | `Not_canonical ->
      failwith "starts_at_beginning_of_function on non-canonical available range"
    | `Canonical `Starts_at_beginning_of_function -> true
    | `Canonical (`Starts_at_label _label) -> false

  let starting_label t ~start_of_function_label =
    if starts_at_beginning_of_function t then
      (* CR mshinwell: it's yucky in this case that we ignore
          [t.starting_label], which will actually have been emitted.  Clean
          this up as part of the coalescing-labels work. *)
      start_of_function_label
    else
      Printf.sprintf ".L%d" (starting_label_of_t_exn t)

  let location_list_entry t ~start_of_function_label =
    let starting_label = starting_label t ~start_of_function_label in
    let ending_label =
      Printf.sprintf ".L%d" (ending_label_of_t_exn t)
    in
    let location_expression =
      let internal_prefix = "__ocaml" in
      if String.length (reg_name t) > String.length internal_prefix
         && String.sub (reg_name t) 0 (String.length internal_prefix) = internal_prefix then
        None
      else
        match Reg.location t.reg with
        | Reg.Reg reg_number ->
          (* CR mshinwell: this needs fixing, ESPECIALLY "R".  and below.
             find out why there seems to be some problem with cloning [loc_args]
             ---we could just name them for this function if we could do that

             mshinwell: actually, now all we need to do is to work out how to avoid a
             name clash on "R", then always return [None] for it here.  We artifically
             extend the live ranges of the regs into which the "R" regs are moved, to
             the start of the function.
          *)
          begin match reg_name t with
          | "R" | "" -> None
          | reg_name ->
            if String.length reg_name >= 3
              && reg_name.[0] = 'R'
              && reg_name.[1] = '-'
              && (try
                    ignore (int_of_string (String.sub reg_name 2 (String.length reg_name - 2)));
                    true
                  with Failure _ -> false)
            then
              None
            else
              Some (Dwarf_low.Location_expression.in_register reg_number)
          end
        | Reg.Stack (Reg.Local stack_slot_index) ->
          Some (Dwarf_low.Location_expression.at_offset_from_stack_pointer
              ~offset_in_bytes:(stack_slot_index * 8))
        | Reg.Stack (Reg.Incoming _) -> None  (* CR mshinwell: don't know *)
        | Reg.Stack (Reg.Outgoing _) -> None
        | Reg.Unknown -> None
    in
    match location_expression with
    | None -> None
    | Some location_expression ->
(*
      Printf.printf "reg '%s' (lr name %s): %s -> %s\n%!" (Reg.name t.reg)
        (reg_name t)
        starting_label ending_label;
*)
      let location_list_entry =
        Dwarf_low.Location_list_entry.create_location_list_entry
          ~start_of_code_label:start_of_function_label
          ~first_address_when_in_scope:starting_label
          ~first_address_when_not_in_scope:ending_label
          ~location_expression
      in
      Some location_list_entry
end

(* CR mshinwell: this many/one live ranges thing isn't great; consider
   restructuring *)

module Many_live_ranges = struct
  (* Assumption: we're refering to the same variable in all the ranges *)
  (* CR mshinwell: we should tighten this up: the [reg_name] function above
     should now return stamped names, for example, which isn't made clear
     here (but is important). *)

  type t = One_live_range.t list

  let create live_ranges = List.sort ~cmp:One_live_range.compare live_ranges

  let compare t1 t2 =
    match t1, t2 with
    (* weird cases. (can they really happen?) *)
    | [], _ -> -1
    | _, [] ->  1
    (* general case *)
    | first1 :: _, first2 :: _ ->
      One_live_range.compare first1 first2

  let starting_label ~start_of_function_label = function
    | [] -> start_of_function_label
    | first :: _ -> One_live_range.starting_label ~start_of_function_label first

  let dwarf_tag t =
    (* CR mshinwell: needs sorting out too *)
    match List.dedup (List.map t ~f:One_live_range.dwarf_tag) with
    | [tag] -> tag
    | [] -> Dwarf_low.Tag.variable (* doesn't get used *)
    | _tags -> Dwarf_low.Tag.formal_parameter

  let name t =
    (* CR mshinwell: the name handling needs thought.  Maybe we should
       attach properly-stamped idents to Regs?  This must be required to
       fix problems when names are shadowed.

       mshinwell: [Reg.t] values now have [Ident.unique_name]s upon them.
       We need to fix up this old crap though, nonetheless. *)
    let names = List.map t ~f:One_live_range.reg_name in
    let without_dummies =
      List.filter names ~f:(function "R" | "" -> false | _ -> true)
    in
    match List.dedup without_dummies with
    | [name] -> name
    | [] -> "<anon>"
    | multiple ->
      (* Is that case realistic considering the previous assumption? *)
      (* CR mshinwell: this needs fixing.  see above *)
      String.concat "/" multiple

  (* [human_name t] returns the name of the variable associated with the set
     of available ranges [t] as it would be written in source code or typed
     into a debugger.  (Viz. [SYMBOL_NATURAL_NAME] in gdb.) *)
  let human_name t =
    let name = name t in
    try
      (* CR mshinwell: '_' should be factored out across here and ident.ml *)
      String.sub name 0 (String.rindex name '_')
    with Not_found -> name

  (* [stamped_name t] returns the name of the variable associated with the set
     of available ranges [t] qualified with its stamp.  (This corresponds to
     the output of [Ident.unique_name]; and also to [SYMBOL_LINKAGE_NAME] in
     gdb.)  These stamped names are used for cross-referencing with .cmt files
     in the debugger. *)
  let stamped_name t =
    name t

  let dwarf_attribute_values t ~type_creator ~debug_loc_table
      ~start_of_function_label =
    let base_address_selection_entry =
      Dwarf_low.Location_list_entry.create_base_address_selection_entry
        ~base_address_label:start_of_function_label
    in
    let location_list_entries =
      List.filter_map t
        ~f:(One_live_range.location_list_entry ~start_of_function_label)
    in
    match location_list_entries with
    | [] -> [], debug_loc_table
    | _ ->
      let location_list =
        Dwarf_low.Location_list.create
          (base_address_selection_entry :: location_list_entries)
      in
      let debug_loc_table, loclistptr_attribute_value =
        Dwarf_low.Debug_loc_table.insert debug_loc_table ~location_list
      in
      let type_label_name =
        type_creator ~stamped_name:(stamped_name t)
      in
      let attribute_values =
        let open Dwarf_low in [
          Attribute_value.create_name ~source_file_path:(human_name t);
          Attribute_value.create_linkage_name
            ~linkage_name:(stamped_name t);
          loclistptr_attribute_value;
          Attribute_value.create_type ~label_name:type_label_name;
        ]
      in
      attribute_values, debug_loc_table

  let to_dwarf t ~debug_loc_table ~type_creator ~start_of_function_label =
    let tag = dwarf_tag t in
    let attribute_values, debug_loc_table =
      dwarf_attribute_values t
        ~type_creator
        ~debug_loc_table
        ~start_of_function_label
    in
    tag, attribute_values, debug_loc_table

  let introduce_param ranges =
    List.exists ranges ~f:(fun range ->
      match range.One_live_range.parameter_or_variable with
      | `Variable -> false
      | _ -> true
    )
end

let rec process_instruction ~insn ~first_insn ~prev_insn
      ~current_live_ranges ~previous_live_ranges ~fundecl =
  let must_start_live_ranges_for =
    (* Regs whose live ranges will start immediately before this insn. *)
    match prev_insn with
    | None -> insn.Linearize.available_before
    | Some prev_insn ->
      (* Multiple registers in [available_before] may be holding the same named value.
         We pick one of them as the canonical representative (that is to say, the
         place where the debugger is going to look).  We do not forget about the other
         locations where the value can be found, however, since later on the canonical
         range might end yet the value continues to be available in one of these other
         locations.  (In which case one of those remaining ranges will then be chosen
         as canonical; see below.) *)
      Reg_set.diff insn.Linearize.available_before prev_insn.Linearize.available_before
  in
  let must_finish_live_ranges_for =
    match prev_insn with
    | None -> Reg_set.empty
    | Some prev_insn ->
      Reg_set.diff prev_insn.Linearize.available_before insn.Linearize.available_before
  in
  let label_from_opt = function
    | _, None ->
      begin match insn.Linearize.desc with
      | Linearize.Llabel l -> false, l
      | _ -> true, Linearize.new_label ()
      end
    | b, Some l -> b, l
  in
  let lbl_before_opt, current_live_ranges =
    Reg_set.fold must_start_live_ranges_for
      ~init:((false, None), current_live_ranges)
      ~f:(fun (lbl, current_live_ranges) reg ->
            (* We don't need to start a new range, from the debugger's point of view,
               if there is already a range (call it the "canonical" range) for the same
               named value that will not finish at this instruction.  We do however need
               to record such a non-canonical range internally since we might need it
               later (in the case where it actually extends past the end of the
               canonical range). *)
            let is_canonical =
              let existing_ranges =
                Reg_map.filter current_live_ranges
                 ~f:(fun existing_reg _range ->
                       Reg.name_strip_spilled existing_reg = Reg.name_strip_spilled reg
                         && not (Reg_set.mem must_finish_live_ranges_for existing_reg))
              in
              if Reg_map.cardinal existing_ranges = 0 then
                `Canonical
              else begin
                (* Sanity check: one of the existing ranges that we're saying still
                   suffices to find [Reg.name_strip_spilled reg] must be canonical. *)
(*                assert (Reg_map.exists existing_ranges
                  ~f:(fun _reg range -> One_live_range.is_canonical range)); *)
                `Not_canonical
              end
            in
            let parameter_or_variable =
              match Reg.is_parameter reg with
              | Some _parameter_index -> `Parameter (Reg.name reg)
              | None -> `Variable
            in
            let is_fresh, lbl = label_from_opt lbl in
            let canonical =
              let starting_point =
                match prev_insn with
                | None -> `Starts_at_beginning_of_function
                | Some _ -> `Starts_at_label lbl
              in
              match is_canonical with
              | `Canonical -> `Canonical starting_point
              | `Not_canonical -> `Not_canonical
            in
            let live_range =
              One_live_range.create ~canonical ~parameter_or_variable ~reg
            in
            let current_live_ranges =
              Reg_map.add current_live_ranges ~key:reg ~data:live_range
            in
            (is_fresh, Some lbl), current_live_ranges)
  in
  let current_live_ranges, previous_live_ranges, lbl_before_opt =
    Reg_set.fold must_finish_live_ranges_for
      ~init:(current_live_ranges, previous_live_ranges, lbl_before_opt)
      ~f:(fun (current_live_ranges, previous_live_ranges, lbl_opt) reg ->
            match Reg_map.find current_live_ranges reg with
            | None ->
              (* There should always be a range corresponding to [reg] (whether or not
                 it is canonical). *)
              assert false
            | Some live_range ->
              (* Nothing needs doing here for non-canonical ranges.
                 The case we do need to watch out for is when a (canonical) range finishes
                 yet there exists at least one other non-canonical range---that does not
                 also finish at this instruction---locating the same named value.
                 In this case we need to pick one of the other ranges as canonical (which
                 also involves setting its starting label to the current instruction). *)
                if One_live_range.is_canonical live_range then begin
                  let new_canonical_range =
                    let candidates =
                      Reg_map.filter current_live_ranges
                        ~f:(fun reg' range ->
                             Reg.name_strip_spilled reg = Reg.name_strip_spilled reg'
                               && not (Reg_set.mem must_finish_live_ranges_for reg')
                               && not (One_live_range.is_canonical range))
                    in
                    match Reg_map.bindings candidates with
                    | [] -> None
                    | ranges ->
                      (* Prefer regs that are on the stack, since they're more likely
                         to still be there from a previous frame when deep in a call
                         chain. *)
                      let on_stack =
                        List.filter ranges
                          ~f:(fun (reg, _range) ->
                                match Reg.location reg with
                                | Reg.Stack _ -> true 
                                | _ -> false)
                      in
                      match on_stack with
                      | (reg, range)::_ranges -> Some (reg, range)
                      | [] ->
                        match ranges with
                        | (reg, range)::_ranges -> Some (reg, range)
                        | [] -> assert false
                  in
                  let current_live_ranges =
                    Reg_map.remove current_live_ranges reg
                  in
                  let previous_live_ranges =
                    live_range :: previous_live_ranges
                  in
                  let b, end_label = label_from_opt lbl_opt in
                  One_live_range.set_ending_label live_range end_label;
                  let current_live_ranges =
                    match new_canonical_range with
                    | None -> current_live_ranges
                    | Some (reg, range) ->
                      (* CR mshinwell: convince ourselves that this really cannot
                         happen, and write down the argument. *)
                      assert (prev_insn <> None);
                      One_live_range.make_canonical range ~starting_label:end_label;
                      Reg_map.add current_live_ranges ~key:reg ~data:range
                  in
                  current_live_ranges, previous_live_ranges, (b, Some end_label)
                end else
                  current_live_ranges, previous_live_ranges, lbl_opt)

  in
  begin match lbl_before_opt with
  | _, None | false, _ -> ()
  | true, Some l  ->
    (* Inserting the code to emit the live range labels is complicated by the
       structure of values of type [Linearize.instruction]. *)
    let insn' =
      let open Linearize in {
        insn with
          desc = Llabel l ;
          next = insn ; (* dummy value, will be fixed below *)
          arg  = [| |] ;
          res  = [| |] ;
      }
    in
    match prev_insn with
    | None ->
      (* If there is no previous instruction, then [insn] was the first one, so
         we want [insn'] as the new first instruction. *)
      first_insn := insn'
    | Some prev_insn ->
      (* If there is one, we want to insert [insn'] in between it and [insn]. *)
      assert (prev_insn.Linearize.next == insn);
      prev_insn.Linearize.next <- insn'
  end;
  match insn.Linearize.desc with
  | Linearize.Lend -> !first_insn, previous_live_ranges
  | Linearize.Lop _ | Linearize.Lreloadretaddr | Linearize.Lreturn
  | Linearize.Llabel _ | Linearize.Lbranch _ | Linearize.Lcondbranch _
  | Linearize.Lcondbranch3 _ | Linearize.Lswitch _ | Linearize.Lsetuptrap _
  | Linearize.Lpushtrap | Linearize.Lpoptrap | Linearize.Lraise ->
    process_instruction ~insn:insn.Linearize.next
      ~first_insn
      ~prev_insn:(Some insn)
      ~current_live_ranges
      ~previous_live_ranges
      ~fundecl

let process_fundecl fundecl =
(*
  Printf.printf "STARTING FUNCTION: %s\n%!" fundecl.Linearize.fun_name;
*)
  let first_insn, live_ranges =
    process_instruction ~insn:fundecl.Linearize.fun_body
      ~first_insn:(ref fundecl.Linearize.fun_body)
      ~prev_insn:None
      ~current_live_ranges:Reg_map.empty
      ~previous_live_ranges:[]
      ~fundecl
  in
  let name_map =
    List.fold live_ranges
      ~init:String.Map.empty
      ~f:(fun name_map live_range ->
            if not (One_live_range.is_canonical live_range) then
              name_map
            else
              let name = One_live_range.reg_name live_range in
  (*             Printf.printf "adding lr: %s\n%!" name; *)
              match String.Map.find name_map name with
              | None -> String.Map.add name_map ~key:name ~data:[live_range]
              | Some live_ranges ->
                (* CR mshinwell: does something need thinking about here? *)
  (*
                Printf.printf "more than one lr for '%s'\n%!" name;
  *)
                let data = live_range::live_ranges in
                String.Map.add (* replace *) name_map ~key:name ~data)
  in
  let live_ranges =
    List.map (List.map (String.Map.to_alist name_map) ~f:snd)
      ~f:Many_live_ranges.create
  in
(*
  Printf.printf "FINISHING FUNCTION: %s\n%!" fundecl.Linearize.fun_name;
*)
  live_ranges, { fundecl with Linearize. fun_body = first_insn; }
