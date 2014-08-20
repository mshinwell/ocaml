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

open Std_internal

type t = {
  compilation_unit : Proto_DIE.t;
  debug_abbrev0 : Linearize.label;
}

let create ~compilation_unit ~debug_abbrev0 =
  { compilation_unit;
    debug_abbrev0;
  }

(* For each pattern of attributes found in the tree of proto-DIEs (of which there
   should be few compared to the number of DIEs), assign an abbreviation code,
   generating abbreviations table entries in the process.  At the same time, generate a
   list of DIEs in flattened format, ready for emission.  (These DIEs reference the
   particular patterns of attributes they use via the abbreviation codes.) *)
let generate_abbrev_table_entries_and_dies t =
  let next_abbreviation_code = ref 0 in
  Proto_DIE.depth_first_fold t.compilation_unit
    ~init:([], [])
    ~f:(fun (abbrev_table_entries, dies) action ~set_abbrev_code ->
      let abbrev_table_entry, die =
        match action with
        | `End_of_siblings -> [], Debugging_information_entry.create_null ()
        | `DIE (tag, has_children, attribute_values, label) ->
          (* Note that [Proto_DIE.create] sorted the attribute values, ensuring that
             a simple re-ordering does not cause a new abbreviation to be created. *)
          let attributes = List.map Attribute_value.attribute attribute_values in
          let abbrev_table_entry, abbreviation_code =
            match Abbreviations_table.find abbrev_table ~tag ~attributes with
            | Some abbrev_code -> [], abbrev_code
            | None -> 
              let abbrev_code = Abbreviation_code.of_int !next_abbreviation_code in
              incr next_abbreviation_code;
              let abbrev_table_entry =
                Abbreviations_table_entry.create abbrev_table
                  ~abbreviation_code:abbrev_code
                  ~tag
                  ~has_children
                  ~attributes
              in
              [abbrev_table_entry], abbrev_code
          in
          let die =
            Debugging_information_entry.create ~label ~abbreviation_code
              ~attribute_values
          in
          abbrev_table_entry, die
      in
      abbrev_table_entry @ abbrev_table_entries, die::dies)

let dwarf_version = Version.two
let debug_abbrev_offset t = Value.as_four_byte_int_from_label t.debug_abbrev0
let address_width_in_bytes_on_target = Value.as_byte Arch.size_addr

let size_without_first_word t ~dies =
  let total_die_size =
    List.fold_left dies
      ~init:0
      ~f:(fun size die -> size + Debugging_information_entry.size die)
  in
  Version.size dwarf_version
    + Value.size (debug_abbrev_offset t)
    + Value.size address_width_in_bytes_on_target
    + total_die_size

(* The "4 +" is for the size field---see [emit], below. *)
let size t = 4 + size_without_first_word t

let emit t ~emitter =
  let abbrev_table_entries, dies = generate_abbrev_table_and_dies t in
  let size = size_without_first_word t ~dies in
  Value.emit (Value.as_four_byte_int size) ~emitter;
  Version.emit dwarf_version ~emitter;
  Value.emit debug_abbrev_offset ~emitter;
  Value.emit address_width_in_bytes_on_target ~emitter;
  List.iter dies ~f:(Debugging_information_entry.emit ~emitter);
  Abbrevations_table.create abbrev_table_entries
