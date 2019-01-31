(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module ASS = Dwarf_attributes.Attribute_specification.Sealed
module DIE = Debugging_information_entry

type result = {
  abbrev_table : Abbreviations_table.t;
  dies : Debugging_information_entry.t list;
  compilation_unit_die : Debugging_information_entry.t option;
}

(* CR-someday mshinwell: Try to work out a straightforward way of ensuring
   that the order of location lists matches the order in which their
   references (in DW_AT_location attributes) will be found in a top-down
   linear scan of the flattened DIE tree.  This will suppress the objdump
   complaint "Location lists in .debug_loc start at ...". *)

(* For each pattern of attributes found in the tree of proto-DIEs (of which
   there should be few compared to the number of DIEs), assign an abbreviation
   code, generating an abbreviations table in the process.  At the same time,
   generate a list of DIEs in flattened format, ready for emission.  (These
   DIEs reference the particular patterns of attributes they use via the
   abbreviation codes.) *)
let run ~proto_die_root =
  let abbrev_table, dies_rev, compilation_unit_die =
    let next_abbreviation_code = ref 1 in
    Proto_die.depth_first_fold proto_die_root
      ~init:(Abbreviations_table.create (), [], None)
      ~f:(fun (abbrev_table, dies, compilation_unit_die)
              (action : Proto_die.fold_arg) ->
        let abbrev_table, die, compilation_unit_die =
          match action with
          | End_of_siblings ->
            abbrev_table, DIE.create_null (), compilation_unit_die
          | DIE (tag, has_children, attribute_values, label, name) ->
            let attribute_specs = ASS.Map.keys attribute_values in
            let abbrev_table, abbreviation_code =
              match
                Abbreviations_table.find abbrev_table ~tag ~has_children
                  ~attribute_specs
              with
              | Some abbrev_code -> abbrev_table, abbrev_code
              | None -> 
                let abbreviation_code =
                  Abbreviation_code.of_int !next_abbreviation_code tag
                in
                incr next_abbreviation_code;
                let abbrev_table_entry =
                  Abbreviations_table_entry.create ~abbreviation_code ~tag
                    ~has_children ~attribute_specs
                in
                Abbreviations_table.add abbrev_table abbrev_table_entry,
                  abbreviation_code
            in
            let die =
              DIE.create ~label ~abbreviation_code ~attribute_values ~name
            in
            let compilation_unit_die =
              let is_compilation_unit =
                match tag with
                | Compile_unit -> true
                | _ -> false
              in
              if not is_compilation_unit then
                compilation_unit_die
              else
                match compilation_unit_die with
                | None -> Some die
                | Some _ ->
                  Misc.fatal_error "More than one `Compile_unit' DIE is present"
            in
            abbrev_table, die, compilation_unit_die
        in
        abbrev_table, die::dies, compilation_unit_die)
  in
  { abbrev_table;
    dies = List.rev dies_rev;
    compilation_unit_die;
  }
