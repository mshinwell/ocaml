(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2014, Jane Street Holding                          *)
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
open Dwarf_low
open Dwarf_std_internal

type t = {
  source_file_path : string option;
  mutable externally_visible_functions : string list;
}

let location_list_entry ~available_subrange ~start_of_function_label =
  let starting_label = starting_label t ~start_of_function_label in
  let ending_label = Printf.sprintf ".L%d" (ending_label_of_t_exn t) in
  let location_expression =
    let module LE = Dwarf_low.Location_expression in
    match Reg.location t.reg with
    | Reg.Unknown -> assert false  (* probably a bug in available_regs.ml *)
    | Reg.Reg reg_number -> LE.in_register reg_number
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
      LE.at_computed_offset_from_frame_pointer ~offset_in_bytes
  in
  Location_list_entry.create_location_list_entry
    ~start_of_code_label:start_of_function_label
    ~first_address_when_in_scope:starting_label
    ~first_address_when_not_in_scope:ending_label
    ~location_expression

let dwarf_for_identifier ~function_name ~start_of_function_label
      ~compilation_unit_proto_die ~function_proto_die ~debug_loc_table
      ~lexical_block_cache ~ident ~is_unique ~range =
  let (start_pos, end_pos) as cache_key = Available_ranges.extremities range in
  let parent_proto_die =
    if Available_range.is_parameter range then begin
      (* Parameters need to be children of the function in question. *)
      function_proto_die
    end else begin
      (* Local variables need to be children of "lexical blocks", which in turn
         are children of the function.  We use a cache to avoid creating more
         than one proto-DIE for any given lexical block position and size. *)
      try Hashtbl.find cache_key lexical_block_cache
      with Not_found -> begin
        let lexical_block =
          let start_pos =
            match start_pos with
            | `Start_of_function -> start_of_function_label
            | `At_label start_pos -> start_pos
          in
          Proto_DIE.create ~parent:(Some function_proto_die)
            ~tag:Tag.lexical_block
            ~attribute_values:[
              Attribute_value.create_low_pc ~address_label:start_pos;
              Attribute_value.create_high_pc ~address_label:end_pos;
            ]
        in
        Hashtbl.add cache_key lexical_block_die lexical_block_cache;
        lexical_block_die
      end
    end
  in
  (* Build a location list that identifies where the value of [ident] may be
     found at runtime, indexed by program counter range, and insert the list
     into the .debug_loc table. *)
  let location_list =
    let base_address_selection_entry =
      Dwarf_low.Location_list_entry.create_base_address_selection_entry
        ~base_address_label:start_of_function_label
    in
    let location_list_entries =
      Available_range.fold range
        ~init:[]
        ~f:(fun location_list_entries ~available_subrange ->
          location_list_entry ~available_subrange ~start_of_function_label)
    in
    Dwarf_low.Location_list.create
      (base_address_selection_entry :: location_list_entries)
  in
  let loclistptr_attribute_value =
    Debug_loc_table.insert debug_loc_table ~location_list
  in
  (* Build a new DWARF type for this identifier.  Each identifier has its
     own type, which is actually its stamped name, and is nothing to do with
     its inferred OCaml type.  The inferred type may be recovered by the
     debugger by extracting the stamped name and then using that as a key
     for lookup into the .cmt file for the appropriate module. *)
  let type_proto_die =
    Proto_DIE.create
      ~parent:(Some compilation_unit_proto_die)
      ~tag:Tag.base_type
      ~attribute_values:[
        Attribute_value.create_name ~source_file_path:type_name;
        Attribute_value.create_encoding ~encoding:Encoding_attribute.signed;
        Attribute_value.create_byte_size ~byte_size:8;
      ]
  in
  (* If the unstamped name of [ident] is unambiguous within the function,
     then use it; otherwise, emit the stamped name. *)
  let name_for_ident =
    if is_unique then Ident.name ident else Ident.unique_name ident
  in
  let tag =
    if Available_range.is_parameter range then Tag.formal_parameter
    else Tag.variable
  in
  Proto_DIE.create_ignore ~parent:(Some parent_proto_die)
    ~tag
    ~attribute_values:[
      Attribute_value.create_name name_for_ident;
      Attribute_value.create_linkage_name (Ident.unique_name ident);
      loclistptr_attribute_value;
      Attribute_value.create_type ~proto_die:type_proto_die;
    ]

let start_function t ~compilation_unit_proto_die ~fundecl =
  let function_name = fundecl.Linearize.fun_name in
  (* CR mshinwell: sort this source_file_path stuff out *)
  match t.source_file_path with
  | None -> function_name, fundecl
  | Some source_file_path ->
    let starting_label = sprintf "L%s.start" function_name in
    let ending_label = sprintf "L%s.end" function_name in
    let subprogram_proto_die =  (* "subprogram" means "function" for us. *)
      Proto_DIE.create ~label_name:None
        ~tag:Tag.subprogram
        ~attribute_values:[
          Attribute_value.create_name ~source_file_path:function_name;
          Attribute_value.create_external ~is_visible_externally:true;
          Attribute_value.create_low_pc ~address_label:starting_label;
          Attribute_value.create_high_pc ~address_label:ending_label;
        ]
    in
    Emitter.emit_label_declaration t.emitter starting_label;
    (* [Available_ranges.create] may modify [fundecl], but it never changes
       the first instruction. *)
    let available_ranges = Available_ranges.create ~fundecl in
    let lexical_block_cache = Hashtbl.create () in
    (* For each identifier for which we have available ranges, construct
       DWARF information to describe how to access the values of such
       identifiers, indexed by program counter ranges. *)
    Available_ranges.fold available_ranges
      ~init:()
      ~f:(fun () -> dwarf_for_identifier ~function_name
        ~start_of_function_label ~compilation_unit_proto_die
        ~function_proto_die:subprogram_proto_die ~debug_loc_table
        ~lexical_block_cache);
    t.externally_visible_functions
      <- function_name::t.externally_visible_functions

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
  let compilation_unit_proto_die =
    let attribute_values =
      let producer_name = sprintf "ocamlopt %s" Sys.ocaml_version in
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
        Attribute_value.create_producer ~producer_name;
        Attribute_value.create_low_pc ~address_label:t.start_of_code_label;
        Attribute_value.create_high_pc ~address_label:t.end_of_code_label;
        Attribute_value.create_stmt_list ~section_offset_label:"Ldebug_line0";
        Attribute_value.create_comp_dir ~directory;
      ]
      in
      match t.source_file_path with
      | None -> common
      | Some source_file_path -> (AV.create_name source_file_path)::common
    in
    Proto_DIE.create ~parent:None
      ~tag:Tag.compile_unit
      ~attribute_values
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
  let emitter = t.emitter in
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
