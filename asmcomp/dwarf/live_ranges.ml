(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
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
end

module Reg_set = struct
  include Reg.Set
  let iter t ~f = iter f t
  let fold t ~init ~f = fold (fun elt acc -> f acc elt) t init
  let to_list = elements
end

module One_live_range = struct
  module T = struct
    type t = {
      id : int;
      parameter_or_variable : [ `Parameter of string | `Variable ];
      reg : Reg.t;
      starting_label : Linearize.label;
      mutable ending_label : Linearize.label option;
      starts_at_beginning_of_function : bool;
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

  let create ?starting_label ?ending_label ~parameter_or_variable ~reg
    ~starts_at_beginning_of_function () =
    let our_id = !unique_id in
    unique_id := !unique_id + 1;
    let starting_label =
      match starting_label with
      | None -> Linearize.new_label ()
      | Some l -> l
    in
    {
      id = our_id;
      parameter_or_variable;
      reg;
      starting_label;
      ending_label = ending_label;
      starts_at_beginning_of_function;
    }

  let code_for_starting_label t =
    Linearize.Llabel t.starting_label

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

  let starting_label t ~start_of_function_label =
    if t.starts_at_beginning_of_function then
      (* CR mshinwell: it's yucky in this case that we ignore
          [t.starting_label], which will actually have been emitted.  Clean
          this up as part of the coalescing-labels work. *)
      start_of_function_label
    else
      Printf.sprintf ".L%d" t.starting_label

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
            let parameter_or_variable, starts_at_beginning_of_function =
              match Reg.is_parameter reg with
              | Some _parameter_index -> `Parameter (Reg.name reg), true
              | None -> `Variable, false
            in
            let is_fresh, lbl = label_from_opt lbl in
            let live_range =
              One_live_range.create ~starting_label:lbl ~parameter_or_variable
                ~reg ~starts_at_beginning_of_function ()
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
            | None -> assert false
            | Some live_range ->
              let current_live_ranges =
                Reg_map.remove current_live_ranges reg
              in
              let previous_live_ranges =
                live_range :: previous_live_ranges
              in
              let b, end_label = label_from_opt lbl_opt in
              One_live_range.set_ending_label live_range end_label ;
              current_live_ranges, previous_live_ranges, (b, Some end_label))
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
