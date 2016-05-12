(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Available_subrange = Available_ranges.Available_subrange
module Available_range = Available_ranges.Available_range
module DAH = Dwarf_attribute_helpers

(* DWARF-related state for a single compilation unit. *)
type t = {
  compilation_unit_proto_die : Proto_die.t;
  debug_loc_table : Debug_loc_table.t;
  start_of_code_symbol : Symbol.t;
  end_of_code_symbol : Symbol.t;
  output_path : string;
  mutable emitted : bool;
}

(* CR mshinwell: We need to figure out how to set this.
   Note that on OS X 10.11 (El Capitan), dwarfdump doesn't seem to be able
   to read our 64-bit DWARF output. *)
let () = Dwarf_format.set Thirty_two

let create ~(source_provenance : Timings.source_provenance) =
  let output_path, directory =
    (* CR mshinwell: this should use the path as per "-o". *)
    match source_provenance with
    | File path ->
      if Filename.is_relative path then
        (* N.B. Relative---but may still contain directories,
           e.g. "foo/bar.ml". *)
        let dir = Sys.getcwd () in
        Filename.concat dir path,
          Filename.concat dir (Filename.dirname path)
      else
        path, Filename.dirname path
    | Pack pack_name -> Printf.sprintf "*pack(%s)*" pack_name, ""
    | Startup -> "*startup*", ""
    | Toplevel -> "*toplevel*", ""
  in
  let start_of_code_symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create "code_begin")
  in
  let end_of_code_symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create "code_end")
  in
  let debug_line_label = Asm_directives.label_for_section (Dwarf Debug_line) in
  let compilation_unit_proto_die =
    let attribute_values =
      let producer_name = Printf.sprintf "ocamlopt %s" Sys.ocaml_version in
      [ DAH.create_producer ~producer_name;
        DAH.create_name output_path;
        DAH.create_comp_dir ~directory;
        DAH.create_low_pc_from_symbol ~symbol:start_of_code_symbol;
        DAH.create_high_pc_from_symbol ~symbol:end_of_code_symbol;
        DAH.create_stmt_list ~debug_line_label;
      ]
    in
    Proto_die.create ~parent:None
      ~tag:Dwarf_tag.Compile_unit
      ~attribute_values
  in
  let debug_loc_table = Debug_loc_table.create () in
  { compilation_unit_proto_die;
    debug_loc_table;
    start_of_code_symbol;
    end_of_code_symbol;
    output_path;
    emitted = false;
  }

(* Build a new DWARF type for [ident].  Each identifier has its
   own type, which is basically its stamped name, and is nothing to do with
   its inferred OCaml type.  The inferred type may be recovered by the
   debugger by extracting the stamped name and then using that as a key
   for lookup into the .cmt file for the appropriate module.

   We emit the parameter index into the type if the identifier in question
   is a function parameter.  This is used in the debugger support library.
   It would be nice not to have to have this hack, but it avoids changes
   in the main gdb code to pass parameter indexes to the printing function.
   It is arguably more robust, too.
*)
let create_type_proto_die ~parent ~ident ~output_path ~is_parameter:_ =
  let ident =
    (* CR mshinwell: delete if not needed *)
    match ident with
    | `Ident ident -> ident
    | `Unique_name name -> Ident.create_persistent name
  in
  let name =
    Name_laundry.base_type_die_name_for_ident ~ident ~output_path
  in
  Proto_die.create ~parent
    ~tag:Dwarf_tag.Base_type
    ~attribute_values:[
      DAH.create_name name;
      DAH.create_encoding ~encoding:Encoding_attribute.signed;
      DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
    ]

(* CR mshinwell: consider renaming [offset_location_in_words].
   - When [None], the value is in the given register.
   - When [Some], the value is in the memory location whose address is the
     contents of the given register plus the offset. *)
let location_list_entry ~fundecl ~available_subrange
      ~offset_location_in_words =
  let location_expression =
    let module LE = Location_expression in
    match Available_subrange.location available_subrange with
    | Reg reg ->
      begin match reg.Reg.loc with
      | Reg.Unknown -> assert false  (* probably a bug in available_regs.ml *)
      | Reg.Reg _ ->
        begin match offset_location_in_words with
        | None -> Some (LE.in_register ~reg)
        | Some offset_location_in_words ->
          let offset_in_bytes =
            Target_addr.of_int (Arch.size_addr * offset_location_in_words)
          in
          Some (LE.at_offset_from_register ~reg ~offset_in_bytes)
        end
      | Reg.Stack _ ->
        match Available_subrange.offset_from_stack_ptr available_subrange with
        | None -> assert false  (* emit.mlp should have set the offset *)
        | Some offset_in_bytes ->
          let offset_in_bytes = Target_addr.of_int offset_in_bytes in
          Some (LE.at_offset_from_stack_pointer ~offset_in_bytes)
      end
    | Phantom (Symbol symbol) ->
      assert (offset_location_in_words = None);
      Some (LE.at_symbol symbol)
    | Phantom (Int i) ->
      assert (offset_location_in_words = None);
      Some (LE.implicit (Int i))
  in
  match location_expression with
  | None -> None
  | Some location_expression ->
    let start_of_code_symbol =
      Name_laundry.fun_name_to_symbol fundecl.Linearize.fun_name
    in
    let first_address_when_in_scope =
      Available_subrange.start_pos available_subrange
    in
    let first_address_when_not_in_scope =
      Available_subrange.end_pos available_subrange
    in
    let entry =
      Location_list_entry.create_location_list_entry
        ~start_of_code_symbol
        ~first_address_when_in_scope
        ~first_address_when_not_in_scope
        ~location_expression
    in
    Some entry

let dwarf_for_identifier ?force_is_parameter t ~fundecl ~function_proto_die
      ~lexical_block_cache ~ident ~is_unique:_ ~range
      ~offset_location_in_words =
  let is_parameter =
    match force_is_parameter with
    | Some is_parameter -> is_parameter
    | None -> Available_range.is_parameter range
  in
  let (start_pos, end_pos) as cache_key = Available_range.extremities range in
  let parent_proto_die =
    match is_parameter with
    | Some _index ->
      (* Parameters need to be children of the function in question. *)
      function_proto_die
    | None ->
      (* Local variables need to be children of "lexical blocks", which in turn
         are children of the function.  We use a cache to avoid creating more
         than one proto-DIE for any given lexical block position and size. *)
      try Hashtbl.find lexical_block_cache cache_key
      with Not_found -> begin
        let lexical_block_proto_die =
          Proto_die.create ~parent:(Some function_proto_die)
            ~tag:Dwarf_tag.Lexical_block
            ~attribute_values:[
              DAH.create_low_pc ~address_label:start_pos;
              DAH.create_high_pc ~address_label:end_pos;
            ]
        in
        Hashtbl.add lexical_block_cache cache_key lexical_block_proto_die;
        lexical_block_proto_die
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
      let fun_symbol =
        Name_laundry.fun_name_to_symbol fundecl.Linearize.fun_name
      in
      Location_list_entry.create_base_address_selection_entry
        ~base_address_symbol:fun_symbol
    in
    let location_list_entries =
      Available_range.fold range
        ~init:[]
        ~f:(fun location_list_entries ~available_subrange ->
          let location_list_entry =
            location_list_entry ~fundecl ~available_subrange
              ~offset_location_in_words
          in
          match location_list_entry with
          | None -> location_list_entries
          | Some entry -> entry::location_list_entries)
    in
    let location_list_entries =
      base_address_selection_entry :: location_list_entries
    in
    let location_list = Location_list.create ~location_list_entries in
    Debug_loc_table.insert t.debug_loc_table ~location_list
  in
  let type_proto_die =
    create_type_proto_die ~parent:(Some t.compilation_unit_proto_die)
      ~ident:(`Ident ident) ~output_path:t.output_path
      ~is_parameter
  in
  (* If the unstamped name of [ident] is unambiguous within the function,
     then use it; otherwise, emit the stamped name. *)
  (* CR mshinwell: this needs much more careful thought *)
  let name_for_ident = Ident.name ident in
(*
    if is_unique then Ident.name ident else Ident.unique_name ident
  in
*)
  let tag =
    match is_parameter with
    | Some _index -> Dwarf_tag.Formal_parameter
    | None -> Dwarf_tag.Variable
  in
  let proto_die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag
      ~attribute_values:[
        DAH.create_name name_for_ident;
        DAH.create_type ~proto_die:type_proto_die;
        location_list_attribute_value;
      ]
  in
  begin match is_parameter with
  | None -> ()
  | Some index ->
    (* Ensure that parameters appear in the correct order in the debugger. *)
    Proto_die.set_sort_priority proto_die index
  end

let dwarf_for_local_variables_and_parameters t ~function_proto_die
      ~lexical_block_cache ~available_ranges
      ~(fundecl : Linearize.fundecl) =
  (* This includes normal variables as well as those bound by phantom lets. *)
  Available_ranges.fold available_ranges
    ~init:()
    ?exclude:fundecl.fun_env_var
    ~f:(fun () -> dwarf_for_identifier t ~fundecl
      ~function_proto_die ~lexical_block_cache
      ~offset_location_in_words:None)

let dwarf_for_free_variables t ~function_proto_die ~lexical_block_cache
      ~available_ranges ~(fundecl : Linearize.fundecl) =
  match fundecl.fun_env_var with
  | None -> ()
  | Some fun_env_var ->
    let module A = Available_ranges in
    match A.find available_ranges ~ident:fun_env_var with
    | None -> ()
    | Some env_var_range ->
      (* Whenever the environment parameter is available, make everything
         in the closure available too. *)
      List.iteri (fun index closure_var ->
          let offset_location_in_words =
            if fundecl.fun_arity - 1 (* ignore env parameter *) < 2 then
              Some (2 + index)
            else
              Some (3 + index)
          in
          dwarf_for_identifier t ~fundecl ~function_proto_die
            ~lexical_block_cache ~ident:closure_var ~is_unique:true
            ~range:env_var_range ~offset_location_in_words
            ~force_is_parameter:None)
        fundecl.fun_closure_layout

let dwarf_for_function_definition t ~(fundecl:Linearize.fundecl)
      ~available_ranges ~(emit_info : Emit.fundecl_result) =
  let symbol =
    Name_laundry.fun_name_to_symbol fundecl.Linearize.fun_name
  in
  let start_of_function =
    DAH.create_low_pc_from_symbol ~symbol
  in
  let end_of_function =
    DAH.create_high_pc ~address_label:emit_info.end_of_function_label
  in
  let function_name =
    match fundecl.fun_module_path with
    | None ->
      begin match fundecl.fun_human_name with
      | "" -> "anon"
      | name -> name
      end
    | Some path ->
      let path = Printtyp.string_of_path path in
      (* CR-soon mshinwell: remove hack *)
      match path with
      | "_Ocaml_startup" ->
        begin match fundecl.fun_human_name with
        | "" -> "anon"
        | name -> name
        end
      | _ ->
        match fundecl.fun_human_name with
        | "" -> path
        | name -> path ^ "." ^ name
  in
  let is_visible_externally =
    (* Not strictly accurate---should probably depend on the .mli, but
       this should suffice for now. *)
    fundecl.fun_module_path <> None
  in
  let type_proto_die =
    create_type_proto_die ~parent:(Some t.compilation_unit_proto_die)
      ~ident:(`Unique_name fundecl.fun_name)
      ~output_path:t.output_path
      ~is_parameter:None
  in
  let function_proto_die =
    Proto_die.create ~parent:(Some t.compilation_unit_proto_die)
      ~tag:Dwarf_tag.Subprogram
      ~attribute_values:[
        DAH.create_name function_name;
        DAH.create_external ~is_visible_externally;
        start_of_function;
        end_of_function;
        DAH.create_type ~proto_die:type_proto_die;
      ]
  in
  let lexical_block_cache = Hashtbl.create 42 in
  dwarf_for_local_variables_and_parameters t ~function_proto_die
    ~lexical_block_cache ~available_ranges ~fundecl;
  dwarf_for_free_variables t ~function_proto_die ~available_ranges
    ~lexical_block_cache ~fundecl

let emit t asm =
  assert (not t.emitted);
  t.emitted <- true;
  Dwarf_world.emit ~compilation_unit_proto_die:t.compilation_unit_proto_die
    ~start_of_code_symbol:t.start_of_code_symbol
    ~end_of_code_symbol:t.end_of_code_symbol
    ~debug_loc_table:t.debug_loc_table
    asm
