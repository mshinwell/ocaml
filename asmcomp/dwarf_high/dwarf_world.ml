(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let emit ~compilation_unit_proto_die ~start_of_code_symbol
      ~end_of_code_symbol ~debug_loc_table ~debug_line_label asm =
  (* CR-soon mshinwell: the [compilation_unit_die] member of the record
     returned from [Assign_abbrevs.run] is now unused *)
  let assigned_abbrevs =
    Assign_abbrevs.run ~proto_die_root:compilation_unit_proto_die
  in
  let debug_abbrev_label = Linearize.new_label () in
  let debug_info_label = Linearize.new_label () in
  let debug_info =
    Debug_info_section.create ~dies:assigned_abbrevs.dies
      ~debug_abbrev_label
  in
  let aranges_table =
    Aranges_table.create ~start_of_code_symbol:start_of_code_symbol
      ~end_of_code_symbol:end_of_code_symbol
      ~debug_info_label
  in
  let _pubnames_table =
    Pubnames_table.create
      (* CR mshinwell: decide what to do about Pubnames_table (it does
         nothing at the moment) *)
      ~externally_visible_functions:[]
      ~debug_info
  in
  let module A = (val asm : Asm_directives.S) in
  A.reset ();
  A.switch_to_section ~section_name:"text" ~is_dwarf:true;
  A.section_declaration ~section_name:"debug_info" ~is_dwarf:true;
  A.label_declaration ~label_name:debug_info_label;
  (* These labels may seem strange, but they are necessary so that
     references (DW_FORM_ref_addr / DW_FORM_sec_offset) to places that are
     currently at the start of these sections get relocated correctly when
     those places become not at the start (e.g. during linking). *)
  A.section_declaration ~section_name:"debug_abbrev" ~is_dwarf:true;
  A.label_declaration ~label_name:debug_abbrev_label;
  A.section_declaration ~section_name:"debug_aranges" ~is_dwarf:true;
  A.section_declaration ~section_name:"debug_loc" ~is_dwarf:true;
  A.section_declaration ~section_name:"debug_str" ~is_dwarf:true;
  A.section_declaration ~section_name:"debug_line" ~is_dwarf:true;
  A.label_declaration ~label_name:debug_line_label;
  A.switch_to_section ~section_name:"debug_info" ~is_dwarf:true;
  Debug_info_section.emit debug_info asm;
  A.switch_to_section ~section_name:"debug_abbrev" ~is_dwarf:true;
  Abbreviations_table.emit assigned_abbrevs.abbrev_table asm;
  A.switch_to_section ~section_name:"debug_aranges" ~is_dwarf:true;
  Aranges_table.emit aranges_table asm;
  A.switch_to_section ~section_name:"debug_loc" ~is_dwarf:true;
  Debug_loc_table.emit debug_loc_table asm;
  A.switch_to_section ~section_name:"debug_line" ~is_dwarf:true;
  A.switch_to_section ~section_name:"debug_str" ~is_dwarf:true;
  A.emit_cached_strings ();
  A.reset ()
