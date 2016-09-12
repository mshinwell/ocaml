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
  compilation_unit_header_label : Linearize.label;
  compilation_unit_proto_die : Proto_die.t;
  value_type_proto_die : Proto_die.t;
  debug_loc_table : Debug_loc_table.t;
  start_of_code_symbol : Symbol.t;
  end_of_code_symbol : Symbol.t;
  output_path : string;
  idents_to_original_idents : Ident.t Ident.tbl;
  mutable emitted : bool;
}

(* CR mshinwell: We need to figure out how to set this.
   Note that on OS X 10.11 (El Capitan), dwarfdump doesn't seem to be able
   to read our 64-bit DWARF output. *)
let () = Dwarf_format.set Thirty_two

(* CR mshinwell: Remove setting from [Debug_info_section]. *)
let dwarf_version = Dwarf_version.four

let create ~(source_provenance : Timings.source_provenance)
      ~idents_to_original_idents =
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
      ()
  in
  let value_type_proto_die =
    Proto_die.create ~parent:(Some compilation_unit_proto_die)
      ~tag:Dwarf_tag.Base_type
      ~attribute_values:[
        DAH.create_name "<value>";
        DAH.create_encoding ~encoding:Encoding_attribute.signed;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
      ]
      ()
  in
  let debug_loc_table = Debug_loc_table.create () in
  { compilation_unit_proto_die;
    compilation_unit_header_label = Cmm.new_label ();
    value_type_proto_die;
    debug_loc_table;
    start_of_code_symbol;
    end_of_code_symbol;
    output_path;
    idents_to_original_idents;
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
let create_type_proto_die t ~parent ~ident ~output_path ~is_parameter:_
      ~array_type:_ =
  let ident =
    match ident with
    | `Ident ident -> ident
    | `Unique_name name -> Ident.create_persistent name
  in
  let name =
    Name_laundry.base_type_die_name_for_ident ~ident ~output_path
  in
  (* CR mshinwell: tidy up once decision made *)
  let array_type = false in
  if array_type then begin
    (* We mark many types as arrays so that GDB doesn't just print
      "<synthetic pointer>" when we use implicit pointers. *)
    let array_type =
      Proto_die.create ~parent
        ~tag:Dwarf_tag.Array_type
        ~attribute_values:[
          DAH.create_name name;
          DAH.create_type ~proto_die:t.value_type_proto_die;
        ]
        ()
    in
    (* If [array_type] doesn't have a child DIE, GDB won't set the name of
       the array type... *)
    Proto_die.create_ignore ~parent:(Some array_type)
      ~tag:Dwarf_tag.Subrange_type
      ~attribute_values:[
      ];
    array_type
  end else begin
    Proto_die.create ~parent
      ~tag:Dwarf_tag.Base_type
      ~attribute_values:[
        DAH.create_name name;
        DAH.create_encoding ~encoding:Encoding_attribute.signed;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
      ]
      ()
  end

let location_of_identifier t ~ident ~proto_dies_for_idents =
  (* We may need to reference the locations of other values in order to
     describe the location of some particular value.  This is done by using
     the "call" functionality of DWARF location descriptions.
     (DWARF-4 specification section 2.5.1.5, page 24.)  This avoids any need
     to transitively resolve phantom lets (to constants, symbols or
     non-phantom variables) in the compiler. *)
  match Ident.Tbl.find proto_dies_for_idents ident with
  | exception Not_found ->
    (* This can unfortunately happen despite best efforts in [Selectgen].
       One example: a "name for debugger" on a register assigned to %rax
       immediately before an allocation on x86-64 (which clobbers %rax).
       The register is explicitly removed from the availability sets by
       [Available_regs], and the name never appears on any available range. *)
    None
  | die_label ->
    Some (Simple_location_description.location_from_another_die ~die_label
      ~compilation_unit_header_label:t.compilation_unit_header_label)

let location_list_entry t ~parent ~fundecl ~available_subrange
      ~proto_dies_for_idents : Location_list_entry.t =
  let simple_location_description
        ~(location : unit Available_subrange.location) =
    let module SLD = Simple_location_description in
    match location with
    | Reg (reg, _, ()) ->
      begin match reg.Reg.loc with
      | Reg.Unknown -> assert false  (* probably a bug in available_regs.ml *)
      | Reg.Reg _ ->
        let reg_number = Proc.dwarf_register_number reg in
        SLD.in_register ~reg_number
      | Reg.Stack _ ->
        (* CR mshinwell: rename [Lcapture_stack_offset] *)
        match
          Available_subrange.offset_from_stack_ptr_in_bytes available_subrange
        with
        | None ->  (* emit.mlp should have set the offset *)
          Misc.fatal_errorf "Register %a assigned to stack but without \
              stack offset annotation"
            Printmach.reg reg
        | Some offset_in_bytes_from_cfa ->
          if offset_in_bytes_from_cfa mod Arch.size_addr <> 0 then begin
            Misc.fatal_errorf "Dwarf.location_list_entry: misaligned stack \
                slot at offset %d (reg %a)"
              offset_in_bytes_from_cfa
              Printmach.reg reg
          end;
          (* CR-soon mshinwell: use [offset_in_bytes] instead *)
          SLD.in_stack_slot
            ~offset_in_words:(offset_in_bytes_from_cfa / Arch.size_addr)
      end
    (* CR mshinwell: don't ignore provenance
       Follow-up: Is it really needed?  For example, Inlining_transforms is
       using dummy module paths for function parameters, etc.
     *)
    | Phantom (_, _, Iphantom_const_int i) -> SLD.const_int (Int64.of_int i)
    | Phantom (_, _, Iphantom_const_symbol symbol) -> SLD.const_symbol symbol
    | Phantom (_, _, Iphantom_read_symbol_field (symbol, field)) ->
      SLD.read_symbol_field ~symbol ~field
    | Phantom (_, _, Iphantom_var ident) ->
      (* CR mshinwell: What happens if [ident] isn't available at some point
         just due to the location list?  Should we push zero on the stack
         first?  Or can we detect the stack is empty?  Or does gdb just abort
         evaluation of the whole thing if the location list doesn't match? *)
      begin match location_of_identifier t ~ident ~proto_dies_for_idents with
      | None -> SLD.empty
      | Some location -> location
      end
    | Phantom (_, _, Iphantom_read_var_field (ident, field)) ->
      begin match location_of_identifier t ~ident ~proto_dies_for_idents with
      | None -> SLD.empty
      | Some location -> SLD.read_field location ~field
      end
    | Phantom (_, _, Iphantom_offset_var (ident, offset_in_words)) ->
      begin match location_of_identifier t ~ident ~proto_dies_for_idents with
      | None -> SLD.empty
      | Some location -> SLD.offset_pointer location ~offset_in_words
      end
    | Phantom (_, _, Iphantom_block { tag; fields; }) ->
      (* A phantom block construction: instead of the block existing in the
         target program's address space, it is going to be conjured up in the
         *debugger's* address space using instructions described in DWARF.
         References between such blocks do not use normal pointers in the
         target's address space---instead they use "implicit pointers"
         (requires GNU DWARF extensions prior to DWARF-5). *)
      (* CR mshinwell: use a cache to dedup the CLDs *)
      let header =
        Simple_location_description.const_int (
          Int64.of_nativeint (
            Cmmgen.black_block_header tag (List.length fields)))
      in
      let header_size = Arch.size_addr in
      let field_size = Arch.size_addr in
      let fields =
        List.map (fun ident ->
            let simple_location_description =
              match ident with
              | None ->
                (* This element of the block isn't accessible. *)
                Simple_location_description.empty
              | Some ident ->
                match
                  location_of_identifier t ~ident ~proto_dies_for_idents
                with
                | None -> Simple_location_description.empty
                | Some location -> location
             in
             simple_location_description, field_size)
          fields
      in
      let composite_location_description =
        Composite_location_description.pieces_of_simple_location_descriptions
          ((header, header_size) :: fields)
      in
      let proto_die =
        Proto_die.create ~parent
          ~tag:Dwarf_tag.Variable
          ~attribute_values:[
            DAH.create_composite_location_description
              composite_location_description;
          ]
          ()
      in
      (* Implicit pointers point after the header, just like for values in the
         OCaml heap (how cool is that)? *)
      SLD.implicit_pointer ~offset_in_bytes:Arch.size_addr
        ~die_label:(Proto_die.reference proto_die)
        ~dwarf_version
  in
  let single_location_description =
    Single_location_description.of_simple_location_description (
      simple_location_description
        ~location:(Available_subrange.location available_subrange))
  in
  let start_of_code_symbol =
    Name_laundry.fun_name_to_symbol fundecl.Linearize.fun_name
  in
  let first_address_when_in_scope =
    Available_subrange.start_pos available_subrange
  in
  let first_address_when_not_in_scope =
    Available_subrange.end_pos available_subrange
  in
  let first_address_when_not_in_scope_offset =
    Available_subrange.end_pos_offset available_subrange
  in
  Location_list_entry.create_location_list_entry
    ~start_of_code_symbol
    ~first_address_when_in_scope
    ~first_address_when_not_in_scope
    ~first_address_when_not_in_scope_offset
    ~single_location_description

let dwarf_for_identifier t ~fundecl ~function_proto_die
      ~lexical_block_proto_die ~proto_dies_for_idents
      ~(ident : Ident.t) ~(ident_for_type : Ident.t option) ~is_unique:_
      ~range =
  let is_parameter = Available_range.is_parameter range in
  let parent_proto_die : Proto_die.t =
    match is_parameter with
    | Parameter _index ->
      (* Parameters need to be children of the function in question. *)
      function_proto_die
    | Local ->
      (* Local variables need to be children of "lexical blocks", which in turn
         are children of the function.  We create a single lexical block per
         function to avoid the debugger getting confused. *)
      lexical_block_proto_die
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
            location_list_entry t ~parent:(Some function_proto_die) ~fundecl
              ~available_subrange ~proto_dies_for_idents
          in
          location_list_entry::location_list_entries)
    in
    let location_list_entries =
      base_address_selection_entry :: location_list_entries
    in
    let location_list = Location_list.create ~location_list_entries in
    Debug_loc_table.insert t.debug_loc_table ~location_list
  in
  let type_and_name_attributes =
    match ident_for_type with
    | None -> []
    | Some ident_for_type ->
      let type_proto_die =
        create_type_proto_die t ~parent:(Some t.compilation_unit_proto_die)
          ~ident:(`Ident ident_for_type) ~output_path:t.output_path
          ~is_parameter ~array_type:true
      in
      (* If the unstamped name of [ident] is unambiguous within the function,
         then use it; otherwise, emit the stamped name. *)
      (* CR mshinwell: this needs much more careful thought *)
      let name_for_ident = Ident.name ident_for_type in
      (*
          if is_unique then Ident.name ident else Ident.unique_name ident
        in
      *)
      [DAH.create_name name_for_ident;
       DAH.create_type ~proto_die:type_proto_die;
      ]
  in
  let tag =
    match is_parameter with
    | Parameter _index -> Dwarf_tag.Formal_parameter
    | Local -> Dwarf_tag.Variable
  in
  let reference =
    match Ident.Tbl.find proto_dies_for_idents ident with
    | exception Not_found ->
      Misc.fatal_errorf "Proto-DIE reference for %a not assigned"
        Ident.print ident
    | reference -> reference
  in
  let proto_die =
    Proto_die.create ~reference
      ~parent:(Some parent_proto_die)
      ~tag
      ~attribute_values:(type_and_name_attributes @ [
        location_list_attribute_value;
      ])
      ()
  in
  begin match is_parameter with
  | Local -> ()
  | Parameter { index; } ->
    (* Ensure that parameters appear in the correct order in the debugger. *)
    Proto_die.set_sort_priority proto_die index
  end

(* This function covers local variables, parameters, variables in closures
   and other "fun_var"s in the current mutually-recursive set.  (The last
   two cases are handled by the explicit addition of phantom lets way back
   in [Flambda_to_clambda].)  Phantom identifiers are also covered. *)
let iterate_over_variable_like_things t ~available_ranges ~f =
  Available_ranges.fold available_ranges
    ~init:()
    ~f:(fun () ~ident ~is_unique ~range ->
      (* There are two identifiers in play here:
         1. [ident] is the "real" identifier that is used to
           cross-reference between DIEs;
         2. [ident_for_type], if it is [Some], is the corresponding
           identifier with the stamp as it was in the typed tree.  This is
           the one used for lookup in .cmt files.
         We cannot conflate these since the multiple [idents] that might
         be associated with a given [ident_for_type] (due to inlining) may
         not all have the same value. *)
      let ident_for_type =
        if Ident.name ident = "*closure_env*" then
          None
        else
        match Ident.find_same ident t.idents_to_original_idents with
        | exception Not_found ->
          (* In this case the variable won't be given a name in the DWARF,
             so as not to appear in the debugger; but we still need to emit
             a DIE for it, as it may be referenced as part of some chain of
             phantom lets. *)
          None
        | ident -> Some ident
      in
      f ~ident ~ident_for_type ~is_unique ~range)

let dwarf_for_variables_and_parameters t ~function_proto_die
      ~lexical_block_proto_die ~available_ranges
      ~(fundecl : Linearize.fundecl) =
  let proto_dies_for_idents = Ident.Tbl.create 42 in
  iterate_over_variable_like_things t ~available_ranges
    ~f:(fun ~ident ~ident_for_type:_ ~is_unique:_ ~range:_ ->
      let reference = Proto_die.create_reference () in
      assert (not (Ident.Tbl.mem proto_dies_for_idents ident));
      Ident.Tbl.add proto_dies_for_idents ident reference);
  iterate_over_variable_like_things t ~available_ranges
    ~f:(dwarf_for_identifier t ~fundecl ~function_proto_die
      ~lexical_block_proto_die ~proto_dies_for_idents)

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
    create_type_proto_die t
      ~parent:(Some t.compilation_unit_proto_die)
      ~ident:(`Unique_name fundecl.fun_name)
      ~output_path:t.output_path
      ~is_parameter:None
      ~array_type:false
  in
  let function_proto_die =
    Proto_die.create ~parent:(Some t.compilation_unit_proto_die)
      ~tag:Dwarf_tag.Subprogram
      ~attribute_values:[
        DAH.create_name function_name;
        DAH.create_external ~is_visible_externally;
        start_of_function;
        end_of_function;
        DAH.create_type ~proto_die:type_proto_die
      ]
      ()
  in
  let lexical_block_proto_die =
    (* CR-someday mshinwell: Consider trying to improve this so that we don't
       have all locals visible at once. *)
    Proto_die.create ~parent:(Some function_proto_die)
      ~tag:Dwarf_tag.Lexical_block
      ~attribute_values:[
        start_of_function;
        end_of_function;
      ]
      ()
  in
  dwarf_for_variables_and_parameters t ~function_proto_die
    ~lexical_block_proto_die ~available_ranges ~fundecl

let dwarf_for_toplevel_constant t ~idents ~module_path ~symbol =
  (* Give each identifier the same definition for the moment. *)
  List.iter (fun ident ->
      let name =
        let path = Printtyp.string_of_path module_path in
        let name = Ident.name ident in
        path ^ "." ^ name
      in
      let type_proto_die =
        create_type_proto_die t
          ~parent:(Some t.compilation_unit_proto_die)
          ~ident:(`Ident ident)
          ~output_path:t.output_path
          ~is_parameter:None
          ~array_type:true
      in
      Proto_die.create_ignore ~parent:(Some t.compilation_unit_proto_die)
        ~tag:Dwarf_tag.Constant
        ~attribute_values:[
          DAH.create_name name;
          DAH.create_type ~proto_die:type_proto_die;
          DAH.create_const_value_from_symbol ~symbol;
          (* Mark everything as "external" so gdb puts the constants in its
            list of "global symbols". *)
          DAH.create_external ~is_visible_externally:true;
        ])
    idents

let dwarf_for_toplevel_constants t constants =
  List.iter (fun (constant : Clambda.preallocated_constant) ->
      match constant.provenance with
      | None -> ()
      | Some provenance ->
        (* Function declarations are emitted separately.  There's no more
           information that we require in a toplevel constant closure. *)
        match constant.definition with
        | Uconst_closure _ -> ()
        | _ ->
          let symbol =
            Symbol.unsafe_create (Compilation_unit.get_current_exn ())
              (Linkage_name.create constant.symbol)
          in
          dwarf_for_toplevel_constant t ~idents:provenance.original_idents
            ~module_path:provenance.module_path
            ~symbol)
    constants

let dwarf_for_toplevel_inconstant t ~ident ~module_path ~symbol =
  let name =
    let path = Printtyp.string_of_path module_path in
    let name = Ident.name ident in
    path ^ "." ^ name
  in
  let type_proto_die =
    create_type_proto_die t
      ~parent:(Some t.compilation_unit_proto_die)
      ~ident:(`Ident ident)
      ~output_path:t.output_path
      ~is_parameter:None
      ~array_type:true
  in
  (* Toplevel inconstant "preallocated blocks" contain the thing of interest
     in field 0 (once it has been initialised).  We describe them using a
     single location description rather than a location list, since they
     should be accessible at all times independent of the current value of
     the PC. *)
  let single_location_description =
    Single_location_description.of_simple_location_description (
      (* We emit DWARF to describe an rvalue, rather than an lvalue, since
         we manually read these values ourselves in libmonda (whereas for
         e.g. a local variable bound to a read-symbol-field, the debugger
         will do a final dereference after determining the lvalue from the
         DWARF).  We cannot currently detect in libmonda whether or not a
         reference to a toplevel module component "M.foo" is a constant
         (represented as an rvalue in the DWARF, just the symbol's address)
         or an inconstant---so we must be consistent as far as l/rvalue-ness
         goes between the two. *)
      (* CR-soon mshinwell: Actually this isn't the case.  We could use
         SYMBOL_CLASS to distinguish them.  However maybe we'd better not
         in case this doesn't work well with non-gdb. *)
      Simple_location_description.read_symbol_field_yielding_rvalue
        ~symbol ~field:0)
  in
  Proto_die.create_ignore ~parent:(Some t.compilation_unit_proto_die)
    ~tag:Dwarf_tag.Variable
    ~attribute_values:[
      DAH.create_name name;
      DAH.create_type ~proto_die:type_proto_die;
      DAH.create_single_location_description single_location_description;
      DAH.create_external ~is_visible_externally:true;  (* see above *)
    ]

let dwarf_for_toplevel_inconstants t inconstants =
  List.iter (fun (inconstant : Clambda.preallocated_block) ->
      (* CR mshinwell: Should we be discarding toplevel things that don't have
         provenance?  Maybe not -- think. *)
      match inconstant.provenance with
      | None -> ()
      | Some provenance ->
        let symbol =
          Symbol.unsafe_create (Compilation_unit.get_current_exn ())
            (Linkage_name.create inconstant.symbol)
        in
        (* CR-someday mshinwell: Support multi-field preallocated blocks
           (ignored for the moment as the only one is the module block, which
           isn't made visible in the debugger). *)
        match provenance.original_idents with
        | [] | _::_::_ -> ()
        | [ident] ->
          dwarf_for_toplevel_inconstant t ~ident
            ~module_path:provenance.module_path
            ~symbol)
    inconstants

let emit t asm =
  assert (not t.emitted);
  t.emitted <- true;
  Dwarf_world.emit ~compilation_unit_proto_die:t.compilation_unit_proto_die
    ~start_of_code_symbol:t.start_of_code_symbol
    ~end_of_code_symbol:t.end_of_code_symbol
    ~compilation_unit_header_label:t.compilation_unit_header_label
    ~debug_loc_table:t.debug_loc_table
    asm
