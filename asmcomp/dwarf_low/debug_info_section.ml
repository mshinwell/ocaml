(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2015, Jane Street Holding                          *)
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

module DIE = Debugging_information_entry
module Proto_DIE = Proto_die

type t = {
  address_width_in_bytes_on_target : int;
  (* CR mshinwell: update name ("proto_die_root")? *)
  compilation_unit : Proto_DIE.t;
  mutable abbrev_table_and_dies
    : (Abbreviations_table.t * (DIE.t list)) option;
}

let create ~compilation_unit =
  { address_width_in_bytes_on_target = 8;
    compilation_unit;
    abbrev_table_and_dies = None;
  }

(* For each pattern of attributes found in the tree of proto-DIEs (of which
   there should be few compared to the number of DIEs), assign an abbreviation
   code, generating an abbreviations table in the process.  At the same time,
   generate a list of DIEs in flattened format, ready for emission.  (These
   DIEs reference the particular patterns of attributes they use via the
   abbreviation codes.) *)
let assign_abbreviations t =
  if t.abbrev_table_and_dies <> None then begin
    failwith "proto-DIE to DIE conversion may only be performed once"
  end;
  let abbrev_table, dies =
    let next_abbreviation_code = ref 1 in
    Proto_DIE.depth_first_fold t.compilation_unit
      ~init:(Abbreviations_table.create (), [])
      ~f:(fun (abbrev_table, dies) action ->
        let abbrev_table, die =
          match action with
          | `End_of_siblings ->
            abbrev_table, DIE.create_null ()
          | `DIE (tag, has_children, attribute_values, label, name) ->
            (* Note that [Proto_DIE.create] sorted the attribute values,
               ensuring that a simple re-ordering does not cause a new
               abbreviation to be created. *)
            let attributes =
              List.map Attribute_value.attribute attribute_values
            in
            let abbrev_table, abbreviation_code =
              match
                Abbreviations_table.find abbrev_table ~tag ~has_children
                  ~attributes
              with
              | Some abbrev_code -> abbrev_table, abbrev_code
              | None -> 
                let abbreviation_code =
                  Abbreviation_code.of_int !next_abbreviation_code
                in
                incr next_abbreviation_code;
                let abbrev_table_entry =
                  Abbreviations_table_entry.create ~abbreviation_code ~tag
                    ~has_children ~attributes
                in
                Abbreviations_table.add abbrev_table abbrev_table_entry,
                  abbreviation_code
            in
            let die =
              DIE.create ~label ~abbreviation_code
                ~attribute_values ~name
            in
            abbrev_table, die
        in
        abbrev_table, die::dies)
  in
  t.abbrev_table_and_dies <- Some (abbrev_table, List.rev dies)

let dwarf_version = Version.four

let debug_abbrev_offset t =
  let section = Section_names.debug_abbrev in
  Value.as_offset_from_label (Section_names.starting_label section) ~section

let size_without_first_word t ~dies =
  let (+) = Int64.add in
  let total_die_size =
    List.fold_left dies
      ~init:Int64.zero
      ~f:(fun size die -> size + DIE.size die)
  in
  Version.size dwarf_version
    + Value.size (debug_abbrev_offset t)
    + Value.size address_width_in_bytes_on_target
    + total_die_size

let size t =
  match t.abbrev_table_and_dies with
  | None ->
    failwith "must assign abbreviations before calculating debug info \
              section size"
  | Some (_abbrev_table, dies) ->
    let size_without_first_word = size_without_first_word t ~dies in
    let initial_length = Initial_length.create size_without_first_word in
    Int64.add (Initial_length.size initial_length) size_without_first_word

let emit t ~emitter =
  match t.abbrev_table_and_dies with
  | None ->
    failwith "must assign abbreviations before emitting debug info section"
  | Some (abbrev_table, dies) ->
    let size_without_first_word = size_without_first_word t ~dies in
    let initial_length = Initial_length.create size_without_first_word in
    Initial_length.emit initial_length ~emitter;
    Version.emit dwarf_version ~emitter;
    Value.emit (debug_abbrev_offset t) ~emitter;
    let address_width_in_bytes_on_target =
      Value.as_byte Arch.size_addr t.address_width_in_bytes_on_target
    in
    Value.emit address_width_in_bytes_on_target ~emitter;
    List.iter dies ~f:(DIE.emit ~emitter);
    abbrev_table

let rebuild_proto_dies ~dies ~abbrev_table =
  let offset_table = Hashtbl.create (List.length dies) in
  let rec for_each_die ~proto_dies ~parents ~dies =
    match dies with
    | [] -> failwith "DIE list ended prematurely"
    | (offset_from_start_of_debug_info, die)::dies ->
      if DIE.is_null die then  (* end of siblings *)
        match parents with
        | _parent::parents ->
          for_each_die ~proto_dies ~parents ~dies
        | [] ->
          match remaining_dies with
          | [] -> proto_dies  (* all DIEs processed successfully *)
          | _ ->
            failwith (Printf.sprintf "reached end-of-sibling chain at \
              toplevel, but there follow a further %d DIE(s)"
              (List.length remaining_dies))
      else
        let abbreviation_code = DIE.abbreviation_code die in
        match
          Abbreviations_table.find_by_code abbrev_table ~abbreviation_code
        with
        | None ->
          failwith (Printf.sprintf "no DIE for abbreviation code %d"
              abbreviation_code)
        | Some abbrev_table_entry ->
          let module A = Abbreviations_table_entry in
          let name =
            Printf.sprintf ".debug_info+0x%x" offset_from_start_of_debug_info
          in
          let parent =
            match parents with
            | parent::_ -> Some parent
            | [] -> None
          in
          let proto_die =
            Proto_DIE.create_whout_parent ~parent
              ~tag:(A.tag abbrev_table_entry)
              ~attribute_values:(A.attribute_values abbrev_table_entry)
          in
          Proto_DIE.set_name proto_die name;
          Hashtbl.replace name_table offset_from_start_of_debug_info proto_die;
          let proto_dies = proto_die::proto_dies in
          let next_name = next_name + 1 in
          let parents =
            if A.has_children abbrev_table_entry then proto_die::parents
            else parents
          in
          for_each_die ~proto_dies ~parents ~dies
  in
  (* First pass: create [Proto_DIE.t] values from [DIE.t] values.  References
     between DIEs, except for the normal parent/sibling relationships, are
     not patched up at this stage.  (An example is DW_AT_specification.) *)
  let proto_dies = for_each_die ~proto_dies:[] ~parents:[] ~dies in
  (* Second pass: patch up any inter-DIE references within attribute values,
     together with references to the .debug_loc section (for locations). *)
  List.iter proto_dies ~f:(fun proto_die ->
    Proto_DIE.replace_attribute_values proto_die ~f:(fun ~attribute_value ->
      let attribute = Attribute_value.attribute attribute_value in
      let module A = Attribute in
      match A.to_variant attribute with
      | A.Stmt_list
      | A.Location_using_single_location_description
      | A.Location_using_location_list

      | A.Low_pc
      | A.High_pc
      | A.Producer
      | A.Name
      | A.Comp_dir
      | A.Extern'l
      | A.Typ'
      | A.Encoding
      | A.Byte_size
      | A.Linkage_name
      | A.Sibling
      | A.Import
      | A.Specification
    );
  proto_dies

let parse ~debug_info_stream:stream ~debug_abbrev_stream =
  Initial_length.parse ~stream
  >>= fun initial_length ->
  Version.parse ~stream
  >>= fun dwarf_version' ->
  if dwarf_version' > dwarf_version then
    Error (Printf.sprintf "DWARF version %d unsupported \
        (latest supported is %d)" dwarf_version' dwarf_version)
  else
    let dwarf_format = Initial_length.dwarf_format initial_length in
    begin match dwarf_format with
    | Dwarf_format.Thirty_two -> Int64.of_int32 (Stream.parse_int32 stream)
    | Dwarf_format.Sixty_four -> Stream.parse_int64 stream
    end
    >>= fun debug_abbrev_offset ->
    Stream.parse_byte stream
    >>= fun address_width_in_bytes_on_target ->
    Stream.parse_list_with_stream_offsets stream ~f:DIE.parse
    >>= fun dies ->
    Stream.advance debug_abbrev_stream ~bytes:debug_abbrev_offset;
    Abbreviations_table.parse ~stream:debug_abbrev_stream
    >>= fun abbrev_table ->
    let compilation_unit = rebuild_proto_dies ~dies ~abbrev_table in
    { address_width_in_bytes_on_target;
      compilation_unit;
      abbrev_table_and_dies = Some (abbrev_table, dies);
    }
