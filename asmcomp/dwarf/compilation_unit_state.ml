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

(* CR-soon mshinwell: fix uses of [open] *)
open Dwarf_low_dot_std
open Dwarf_low
open Dwarf_std_internal

type t = {
  emitter : Emitter.t;
  source_file_path : string option;
  start_of_code_label : string;
  end_of_code_label : string;
  mutable externally_visible_functions : string list;
  mutable function_tags :
    (int * string * Tag.t * Dwarf_low.Attribute_value.t list) list;
  mutable debug_loc_table : Debug_loc_table.t;
  mutable type_names : (string * string) list;
}

let create ~emitter ~source_file_path ~start_of_code_label ~end_of_code_label =
  { emitter;
    source_file_path;
    start_of_code_label;
    end_of_code_label;
    externally_visible_functions = [];
    function_tags = [];
    debug_loc_table = Debug_loc_table.create ();
    type_names = [];
  }

let build_ocaml_type_tags ~type_label ~type_name =
  1, type_label, Dwarf_low.Tag.base_type, [
    Dwarf_low.Attribute_value.create_name ~source_file_path:type_name;
    Dwarf_low.Attribute_value.create_encoding
      ~encoding:Dwarf_low.Encoding_attribute.signed;
    Dwarf_low.Attribute_value.create_byte_size
      ~byte_size:8;
  ];

module Function = struct
  type t = string  (* function name, ahem *)
end

module Reg_location = struct
  type t = [
  | `Hard_register of int
  | `Stack of unit
  ]

  let hard_register ~reg_num = `Hard_register reg_num
  let stack () = `Stack ()
end

let next_type_label = ref 0

let put_ranges_in_scopes loc_table ~function_name ~starting_label ~ending_label lst
      ~source_file_path ~slot_offset_in_bytes =
  let live_ranges = List.sort ~cmp:Live_ranges.Many_live_ranges.compare lst in
  let rec aux id level debug_loc_table tags ~type_labels = function
    | [] -> debug_loc_table, tags, type_labels
    | live_range :: rest ->
      let name = Printf.sprintf "%d" id in
      let type_labels' = ref [] in
      (* CR mshinwell: this is all pretty nasty.  Review once we're sure this is
         the correct approach. *)
      let type_creator ~stamped_name =
        let type_label =
          let type_label = Printf.sprintf ".ocamltype%d" !next_type_label in
          next_type_label := !next_type_label + 1;
          type_label
        in
        let type_name =
          (* CR mshinwell: would be nice to share with the gdb side *)
          "__ocaml" ^ source_file_path ^ " " ^ stamped_name
        in
        type_labels' := (type_label, type_name)::!type_labels';
        type_label
      in
      let tag, attribute_values, debug_loc_table =
        (* CR mshinwell: should maybe return an option instead *)
        Live_ranges.Many_live_ranges.to_dwarf live_range
          ~type_creator
          ~debug_loc_table
          ~start_of_function_label:starting_label
          ~slot_offset_in_bytes
      in
      let type_labels = !type_labels' @ type_labels in
      let id = id + 1 in
      match attribute_values with
      | [] -> aux id level debug_loc_table tags rest ~type_labels
      | _ ->
        let lexical_block, range_level =
          (* We want the formal parameters to be in the scope of the function,
           * not in nested scopes. *)
          if Live_ranges.Many_live_ranges.introduce_param live_range then
            None, level
          else
            let start =
              Live_ranges.Many_live_ranges.starting_label live_range
                ~start_of_function_label:starting_label
            in
            let block =
              Some (level, function_name ^ "__lb__" ^ name, Tag.lexical_block, [
                Attribute_value.create_low_pc ~address_label:start ;
                Attribute_value.create_high_pc ~address_label:ending_label ;
              ])
            in
            block, level + 1
        in
        let live_range_tag =
          range_level, function_name ^ "__lr__" ^ name, tag, attribute_values
        in
        let acc =
          match lexical_block with
          | None -> live_range_tag :: tags
          | Some lexical_block -> live_range_tag :: lexical_block :: tags
        in
        aux id range_level debug_loc_table acc rest ~type_labels
  in
  aux 0 2 loc_table [] live_ranges ~type_labels:[]

module L = Linearize

let create_location_list_entry ~available_subrange ~slot_offset_in_bytes =
  let starting_label = starting_label t ~start_of_function_label in
  (* CR mshinwell: we need the same trick for the end of the function as
     [starts_at_beginning_of_function].  Or we could try to fix emit.mlp *)
  let ending_label =
    Printf.sprintf ".L%d" (ending_label_of_t_exn t)
  in
  let location_expression =
    if not (Reg.has_name_suitable_for_debugger t.reg) then
      None
    else
      let module LE = Dwarf_low.Location_expression in
      match Reg.location t.reg with
      | Reg.Unknown -> None
      | Reg.Reg reg_number -> Some (LE.in_register reg_number)
      | Reg.Stack stack_location ->
        (* Byte offsets from the stack pointer will be computed during the
           "emit" phase.  DWARF information will be emitted after such
           computations, so we can use the offsets then. *)
        let offset_in_bytes () =
          let stack_offset =
            match t.first_insn.Linearize.stack_offset with
            | None -> failwith "first_insn has no stack_offset"
            | Some stack_offset -> stack_offset
          in
          slot_offset_in_bytes ~reg_on_stack:t.reg ~stack_offset
        in
        (* We use an offset from the frame base rather than the stack pointer
           since it's possible the stack pointer might change during the
           execution of an available subrange.  (This could presumably be
           avoided by splitting subranges as required, but it seems easier to
           work from the frame base.)
           How would the debugger know where the frame base is, when we might
           not have a frame pointer?  It knows from the DWARF derived by
           the assembler (e.g. DW_AT_frame_base) from the CFI information we
           emit. *)


        Some (LE.at_computed_offset_from_frame_pointer ~offset_in_bytes)
  in
  match location_expression with
  | None -> None
  | Some location_expression ->
    let location_list_entry =
      Dwarf_low.Location_list_entry.create_location_list_entry
        ~start_of_code_label:start_of_function_label
        ~first_address_when_in_scope:starting_label
        ~first_address_when_not_in_scope:ending_label
        ~location_expression
    in
    Some location_list_entry

let dwarf_attribute_values t ~type_creator ~debug_loc_table
    ~start_of_function_label ~slot_offset_in_bytes =
  let base_address_selection_entry =
    Dwarf_low.Location_list_entry.create_base_address_selection_entry
      ~base_address_label:start_of_function_label
  in
  let location_list_entries =
    List.filter_map t.live_ranges
      ~f:(One_live_range.location_list_entry ~start_of_function_label
            ~slot_offset_in_bytes)
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

let dwarf_for_lexical_blocks ... =
  Lexical_blocks.fold ...
    ~init:...
    ~f:(fun acc ~ident ~is_unique ~scope ~range ->




let to_dwarf t ~debug_loc_table ~type_creator ~start_of_function_label
      ~slot_offset_in_bytes =
  let tag = dwarf_tag t in
  let attribute_values, debug_loc_table =
    dwarf_attribute_values t
      ~type_creator
      ~debug_loc_table
      ~start_of_function_label
      ~slot_offset_in_bytes
  in
  tag, attribute_values, debug_loc_table

let start_function t ~linearized_fundecl:fundecl ~slot_offset_in_bytes =
  let function_name = fundecl.L.fun_name in
  (* CR mshinwell: sort this source_file_path stuff out *)
  match t.source_file_path with
  | None -> function_name, linearized_fundecl
  | Some source_file_path ->
    let starting_label = sprintf "L%s.start" function_name in
    let ending_label = sprintf "L%s.end" function_name in
    Emitter.emit_label_declaration t.emitter starting_label;
    let available_ranges = Available_ranges.create ~fundecl in

    let live_ranges, fundecl =
      (* note that [process_fundecl] may modify [linearize_fundecl] *)
      Live_ranges.process_fundecl linearized_fundecl
    in
    let debug_loc_table, live_range_tags, type_names =
      put_ranges_in_scopes t.debug_loc_table ~function_name ~starting_label
        ~ending_label ~source_file_path ~slot_offset_in_bytes live_ranges
    in
    let subprogram_tag =
      let tag =
        if List.length live_range_tags > 0 then
          Tag.subprogram
        else
          Tag.subprogram_with_no_children
      in
      let module AV = Attribute_value in
      1, function_name, tag, [
        AV.create_name ~source_file_path:function_name;
        AV.create_external ~is_visible_externally:true;
        AV.create_low_pc ~address_label:starting_label;
        AV.create_high_pc ~address_label:ending_label;
      ]
    in
    let this_function's_tags = subprogram_tag::(List.rev live_range_tags) in
    t.externally_visible_functions <-
      function_name::t.externally_visible_functions;
    t.debug_loc_table <- debug_loc_table;
    t.function_tags <- t.function_tags @ this_function's_tags;
    t.type_names <- t.type_names @ type_names;
    function_name, fundecl

(*
let start_function t ~function_name ~arguments_and_locations =
  let starting_label = sprintf "Llr_begin_%s" function_name in
  let ending_label = sprintf "Llr_end_%s" function_name in
  Emitter.emit_label_declaration t.emitter starting_label;
  let debug_loc_table, argument_tags =
    List.fold arguments_and_locations
      ~init:(t.debug_loc_table, [])
      ~f:(fun (debug_loc_table, tags) (ident, pseudoreg_location) ->
            let location_expression =
              match pseudoreg_location with
              (* CR mshinwell: fix the stack case *)
              | `Stack () -> None
              | `Hard_register reg_number ->
                Some (Dwarf_low.Location_expression.in_register reg_number)
            in
            match location_expression with
            | None -> debug_loc_table, tags
            | Some location_expression ->
              let base_address_selection_entry =
                Dwarf_low.Location_list_entry.
                  create_base_address_selection_entry
                  ~base_address_label:starting_label
              in
              let location_list_entry =
                Dwarf_low.Location_list_entry.create_location_list_entry
                  ~start_of_code_label:starting_label
                  ~first_address_when_in_scope:starting_label
                  ~first_address_when_not_in_scope:ending_label  (* fixme *)
                  ~location_expression
              in
              let location_list =
                Dwarf_low.Location_list.create
                  [base_address_selection_entry; location_list_entry]
              in
              let debug_loc_table, loclistptr_attribute_value =
                Dwarf_low.Debug_loc_table.insert debug_loc_table
                  ~location_list
              in
              let arg_name = Ident.name ident in
              let tag =
                2, function_name ^ "__arg__" ^ (Ident.unique_name ident),
                  Dwarf_low.Tag.formal_parameter,
                  [Dwarf_low.Attribute_value.create_name
                     ~source_file_path:arg_name;
                   loclistptr_attribute_value;
                   Dwarf_low.Attribute_value.create_type
                     ~label_name:builtin_ocaml_type_label_value;
                  ]
              in
              debug_loc_table, tag::tags)
  in
  let subprogram_tag =
    let tag =
      if List.length argument_tags > 0 then
        Tag.subprogram
      else
        Tag.subprogram_with_no_children
    in
    let module AV = Attribute_value in
    1, function_name, tag, [
      AV.create_name ~source_file_path:function_name;
      AV.create_external ~is_visible_externally:true;
      AV.create_low_pc ~address_label:starting_label;
      AV.create_high_pc ~address_label:ending_label;
    ]
  in
  let this_function's_tags = subprogram_tag::(List.rev argument_tags) in
  t.externally_visible_functions <-
    function_name::t.externally_visible_functions;
  t.debug_loc_table <- debug_loc_table;
  t.function_tags <- t.function_tags @ this_function's_tags;
  function_name
*)

let end_function t function_name =
  Emitter.emit_label_declaration t.emitter (sprintf "Llr_end_%s" function_name)

let with_emitter emitter fs =
  List.iter (fun f -> f emitter) fs

let emit_debugging_info_prologue t =
  let module SN = Section_names in
  with_emitter t.emitter [
    Emitter.emit_section_declaration ~section_name:SN.debug_abbrev;
    Emitter.emit_label_declaration ~label_name:"Ldebug_abbrev0";
    Emitter.emit_section_declaration ~section_name:SN.debug_line;
    Emitter.emit_label_declaration ~label_name:"Ldebug_line0";
    Emitter.emit_section_declaration ~section_name:SN.debug_loc;
    Emitter.emit_label_declaration ~label_name:"Ldebug_loc0";
  ]

let emit_debugging_info_epilogue t =
  let emitter = t.emitter in
  let producer_name = sprintf "ocamlopt %s" Sys.ocaml_version in
  let compile_unit_attribute_values =
    let module AV = Attribute_value in
    let directory =
      match t.source_file_path with
      | None -> Sys.getcwd ()
      | Some path ->
        if Filename.is_relative path then
          Sys.getcwd ()
        else
          Filename.dirname path
    in
    let common = [
      AV.create_producer ~producer_name;
      AV.create_low_pc ~address_label:t.start_of_code_label;
      AV.create_high_pc ~address_label:t.end_of_code_label;
      AV.create_stmt_list ~section_offset_label:"Ldebug_line0";
      AV.create_comp_dir ~directory;
    ]
    in
    match t.source_file_path with
    | None -> common
    | Some source_file_path -> (AV.create_name ~source_file_path)::common
  in
  let type_name_tags =
    List.map t.type_names
      ~f:(fun (type_label, type_name) ->
            build_ocaml_type_tags ~type_label ~type_name)
  in
  (* CR mshinwell: this "no children" thing (and above) is a symptom of a more general
     problem. *)
  let tag =
    if List.length t.function_tags = 0 && List.length type_name_tags = 0 then
      Tag.compile_unit_with_no_children
    else
      Tag.compile_unit
  in
  let tags_with_attribute_values = [
    0, "compile_unit",
      tag, compile_unit_attribute_values;
  ] @ t.function_tags @ type_name_tags
  in
  let debug_info = Debug_info_section.create ~tags_with_attribute_values in
  let debug_abbrev = Debug_info_section.to_abbreviations_table debug_info in
  let pubnames_table =
    Pubnames_table.create
      ~externally_visible_functions:t.externally_visible_functions
      ~debug_info
  in
  let aranges_table =
    Aranges_table.create ~start_of_code_label:t.start_of_code_label
      ~end_of_code_label:t.end_of_code_label
  in
  let module SN = Section_names in
  (* CR-someday mshinwell: consider using [with_emitter] *)
  Emitter.emit_section_declaration emitter ~section_name:SN.debug_info;
  Emitter.emit_label_declaration emitter ~label_name:"Ldebug_info0";
  Debug_info_section.emit debug_info ~emitter;
  Emitter.emit_switch_to_section emitter ~section_name:SN.debug_abbrev;
  Abbreviations_table.emit debug_abbrev ~emitter;
  Emitter.emit_section_declaration emitter ~section_name:SN.debug_pubnames;
  Pubnames_table.emit pubnames_table ~emitter;
  Emitter.emit_section_declaration emitter ~section_name:SN.debug_aranges;
  Aranges_table.emit aranges_table ~emitter;
  Emitter.emit_switch_to_section emitter ~section_name:SN.debug_loc;
  Debug_loc_table.emit t.debug_loc_table ~emitter
