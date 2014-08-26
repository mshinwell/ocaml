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

open Dwarf_low_dot_std.Dwarf_low

module Available_subrange = Available_ranges.Available_subrange
module Available_range = Available_ranges.Available_range

(* DWARF-related state for a single compilation unit. *)
type t = {
  compilation_unit_proto_die : Proto_DIE.t;
  emitter : Emitter.t;
  debug_loc_table : Debug_loc_table.t;
  debug_line_label : Linearize.label;
  start_of_code_symbol : string;
  end_of_code_symbol : string;
  source_file_path : string;
  mutable available_ranges_and_fundecl
    : (Available_ranges.t * Linearize.fundecl) option;
  mutable externally_visible_functions : string list;
}

let create ~source_file_path ~emit_string ~emit_symbol ~emit_label
      ~emit_label_declaration ~emit_section_declaration
      ~emit_switch_to_section ~start_of_code_symbol ~end_of_code_symbol =
  let emitter =
    Emitter.create ~emit_string
      ~emit_symbol
      ~emit_label
      ~emit_label_declaration
      ~emit_section_declaration
      ~emit_switch_to_section
  in
  let debug_line_label = Linearize.new_label () in
  let source_file_path, directory =
    match source_file_path with
    (* CR-soon mshinwell: think about the source file path stuff *)
    | None -> "<unknown>", Sys.getcwd ()
    | Some path ->
      if Filename.is_relative path then
        let dir = Sys.getcwd () in
        Filename.concat dir path, dir
      else
        path, Filename.dirname path
  in
  let compilation_unit_proto_die =
    let attribute_values =
      let producer_name = Printf.sprintf "ocamlopt %s" Sys.ocaml_version in [
        Attribute_value.create_producer ~producer_name;
        Attribute_value.create_name source_file_path;
        Attribute_value.create_comp_dir ~directory;
        Attribute_value.create_low_pc_from_symbol ~symbol:start_of_code_symbol;
        Attribute_value.create_high_pc_from_symbol ~symbol:end_of_code_symbol;
        Attribute_value.create_stmt_list
          ~section_offset_label:debug_line_label;
      ]
    in
    Proto_DIE.create ~parent:None
      ~tag:Tag.compile_unit
      ~attribute_values
  in
  { compilation_unit_proto_die;
    externally_visible_functions = [];
    emitter;
    debug_loc_table = Debug_loc_table.create ();
    debug_line_label;
    start_of_code_symbol;
    end_of_code_symbol;
    source_file_path;
    available_ranges_and_fundecl = None;
  }

let pre_emission_dwarf_for_function t ~fundecl =
  if t.available_ranges_and_fundecl <> None then begin
    failwith "Dwarf.pre_emission_dwarf_for_function"
  end;
  let available_ranges, fundecl = Available_ranges.create ~fundecl in
  t.available_ranges_and_fundecl <- Some (available_ranges, fundecl);
  fundecl

let location_list_entry ~fundecl ~available_subrange =
  let reg = Available_subrange.reg available_subrange in
  Printf.printf "location list entry for subrange t=%d start=%d... "
    (Obj.magic available_subrange)
    (Available_subrange.start_pos available_subrange);
  let location_expression =
    let module LE = Location_expression in
    match reg.Reg.loc with
    | Reg.Unknown -> assert false  (* probably a bug in available_regs.ml *)
    | Reg.Reg reg_number -> Printf.printf "reg\n%!"; Some (LE.in_register reg_number)
    | Reg.Stack _ ->
      match Available_subrange.offset_from_stack_ptr available_subrange with
      | None -> assert false  (* emit.mlp should have set the offset *)
      | Some offset_in_bytes ->
        Printf.printf "stack\n%!";
        Some (LE.at_offset_from_stack_pointer ~offset_in_bytes)
  in
  match location_expression with
  | None -> None
  | Some location_expression ->
    (* CR-someday: rename to "_when_available". *)
    let first_address_when_in_scope =
      Available_subrange.start_pos available_subrange
    in
    let first_address_when_not_in_scope =
      Available_subrange.end_pos available_subrange
    in
    let entry =
      Location_list_entry.create_location_list_entry
        ~start_of_code_symbol:fundecl.Linearize.fun_name
        ~first_address_when_in_scope
        ~first_address_when_not_in_scope
        ~location_expression
    in
    Some entry

let dwarf_for_identifier t ~fundecl ~function_proto_die ~lexical_block_cache
      ~ident ~is_unique ~range =
  Printf.printf "------ dwarf for identifier '%s' -------\n%!"
    (Ident.unique_name ident);
  let (start_pos, end_pos) as cache_key = Available_range.extremities range in
  let parent_proto_die =
    if Available_range.is_parameter range then begin
      (* Parameters need to be children of the function in question. *)
      function_proto_die
    end else begin
      (* Local variables need to be children of "lexical blocks", which in turn
         are children of the function.  We use a cache to avoid creating more
         than one proto-DIE for any given lexical block position and size. *)
      try Hashtbl.find lexical_block_cache cache_key
      with Not_found -> begin
        let lexical_block_proto_die =
          let start_pos =
            Attribute_value.create_low_pc ~address_label:start_pos
          in
          Proto_DIE.create ~parent:(Some function_proto_die)
            ~tag:Tag.lexical_block
            ~attribute_values:[
              start_pos;
              Attribute_value.create_high_pc ~address_label:end_pos;
            ]
        in
        Hashtbl.add lexical_block_cache cache_key lexical_block_proto_die;
        lexical_block_proto_die
      end
    end
  in
  (* Build a location list that identifies where the value of [ident] may be
     found at runtime, indexed by program counter range, and insert the list
     into the .debug_loc table. *)
  let location_list_attribute_value =
    (* DWARF-4 spec 2.6.2: "In the case of a compilation unit where all of the
       machine code is contained in a single contiguous section, no base
       address selection entry is needed."
       However, we tried this (and emitted plain label addresses rather than
       deltas in [Location_list_entry]), and the addresses were wrong in the
       final executable.  Oh well. *)
    let base_address_selection_entry =
      Location_list_entry.create_base_address_selection_entry
        ~base_address_symbol:fundecl.Linearize.fun_name
    in
    let location_list_entries =
      Available_range.fold range
        ~init:[]
        ~f:(fun location_list_entries ~available_subrange ->
           match location_list_entry ~fundecl ~available_subrange with
           | None -> location_list_entries
           | Some entry -> entry::location_list_entries)
    in
    let location_list =
      Location_list.create
        (base_address_selection_entry :: location_list_entries)
    in
    Debug_loc_table.insert t.debug_loc_table ~location_list
  in
  (* Build a new DWARF type for this identifier.  Each identifier has its
     own type, which is basically its stamped name, and is nothing to do with
     its inferred OCaml type.  The inferred type may be recovered by the
     debugger by extracting the stamped name and then using that as a key
     for lookup into the .cmt file for the appropriate module. *)
  let type_proto_die =
    Proto_DIE.create ~parent:(Some t.compilation_unit_proto_die)
      ~tag:Tag.base_type
      ~attribute_values:[
        Attribute_value.create_name
          ("__ocaml" ^ t.source_file_path ^ " " ^ (Ident.unique_name ident));
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
      Attribute_value.create_type
        ~proto_die:(Proto_DIE.reference type_proto_die);
      location_list_attribute_value;
    ]

let post_emission_dwarf_for_function t ~end_of_function_label =
  match t.available_ranges_and_fundecl with
  | None -> failwith "Dwarf.post_emission_dwarf_for_function"
  | Some (available_ranges, fundecl) ->
    let lexical_block_cache = Hashtbl.create 42 in
    let subprogram_proto_die =  (* "subprogram" means "function" for us. *)
      Proto_DIE.create ~parent:(Some t.compilation_unit_proto_die)
        ~tag:Tag.subprogram
        ~attribute_values:[
          Attribute_value.create_name fundecl.Linearize.fun_name;
          Attribute_value.create_external ~is_visible_externally:true;
          Attribute_value.create_low_pc_from_symbol
            ~symbol:fundecl.Linearize.fun_name;
          Attribute_value.create_high_pc ~address_label:end_of_function_label;
        ]
    in
    (* For each identifier for which we have available ranges, construct
       DWARF information to describe how to access the values of such
       identifiers, indexed by program counter ranges. *)
    Available_ranges.fold available_ranges
      ~init:()
      ~f:(fun () -> dwarf_for_identifier t ~fundecl
        ~function_proto_die:subprogram_proto_die ~lexical_block_cache);
    t.externally_visible_functions
      <- fundecl.Linearize.fun_name::t.externally_visible_functions;
    t.available_ranges_and_fundecl <- None

let emit t =
  let with_emitter emitter fs = List.iter (fun f -> f emitter) fs in
  let debug_abbrev0 = Linearize.new_label () in
  let debug_info =
    Debug_info_section.create ~compilation_unit:t.compilation_unit_proto_die
      ~debug_abbrev0
  in
  Debug_info_section.assign_abbreviations debug_info;
  let _pubnames_table =
    Pubnames_table.create
      ~externally_visible_functions:t.externally_visible_functions
      ~debug_info
  in
  let aranges_table =
    Aranges_table.create ~start_of_code_symbol:t.start_of_code_symbol
      ~end_of_code_symbol:t.end_of_code_symbol
  in
  let module SN = Section_names in
  let debug_loc0 = Linearize.new_label () in
  let debug_info0 = Linearize.new_label () in
  with_emitter t.emitter [
    Emitter.emit_section_declaration ~section_name:SN.debug_abbrev;
    Emitter.emit_label_declaration ~label_name:debug_abbrev0;
    Emitter.emit_section_declaration ~section_name:SN.debug_line;
    Emitter.emit_label_declaration ~label_name:t.debug_line_label;
    Emitter.emit_section_declaration ~section_name:SN.debug_loc;
    Emitter.emit_label_declaration ~label_name:debug_loc0;
  ];
  (* CR-someday mshinwell: consider using [with_emitter] *)
  let emitter = t.emitter in
  Emitter.emit_section_declaration emitter ~section_name:SN.debug_info;
  Emitter.emit_label_declaration emitter ~label_name:debug_info0;
  let debug_abbrev = Debug_info_section.emit debug_info ~emitter in
  Emitter.emit_switch_to_section emitter ~section_name:SN.debug_abbrev;
  Abbreviations_table.emit debug_abbrev ~emitter;
  Emitter.emit_section_declaration emitter ~section_name:SN.debug_pubnames;
(*  Pubnames_table.emit pubnames_table ~emitter ~debug_info0; *)
  Emitter.emit_section_declaration emitter ~section_name:SN.debug_aranges;
  Aranges_table.emit aranges_table ~emitter;
  Emitter.emit_switch_to_section emitter ~section_name:SN.debug_loc;
  Debug_loc_table.emit t.debug_loc_table ~emitter
