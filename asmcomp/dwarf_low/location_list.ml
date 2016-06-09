(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Std_internal

type t = {
  name : Linearize.label;
  entries : Location_list_entry.t list;
}

(* It isn't exactly clear what the sorting requirement is, but we sort
   within a location list by increasing virtual memory address on the
   start addresses of the entries.  This also means that we can fill the
   "holes" in the location lists and keep objdump quiet. *)
let sort entries =
  List.sort entries ~cmp:Location_list_entry.compare_ascending_vma

let create ~location_list_entries =
  { name = Linearize.new_label ();
    entries = sort location_list_entries;
  }

let label t = t.name

let end_marker = Dwarf_value.Absolute_code_address Target_addr.zero

let size t =
  let (+) = Int64.add in
  let body_size =
    List.fold t.entries
      ~init:Int64.zero
      ~f:(fun size entry -> size + (Location_list_entry.size entry))
  in
  body_size + Dwarf_value.size end_marker + Dwarf_value.size end_marker

let compare_increasing_vma t1 t2 =
  match t1.entries, t2.entries with
  | t1_entry::_, t2_entry::_ ->
    Location_list_entry.compare_ascending_vma t1_entry t2_entry
  | _ -> failwith "Location_list.compare on empty location list(s)"

let emit t asm =
  let module A = (val asm : Asm_directives.S) in
  A.label_declaration ~label_name:t.name;
  ignore ((List.fold t.entries ~init:None ~f:(fun prev_entry entry ->
    (* If the previous entry's end-of-scope label is not the same as the
       current entry's start-of-scope label, insert a "no location" entry, so
       objdump doesn't complain about holes in .debug_loc.
       This relies on the entries being in order with base address selection
       entries first (see location_list_entry.ml). *)
    begin match prev_entry with
    | None -> ()
    | Some prev_entry ->
      match Location_list_entry.scope prev_entry with
      | None -> ()
      | Some (_, _, prev_end) ->
        match Location_list_entry.scope entry with
        | None -> ()
        | Some (start_of_code_symbol, cur_start, _) ->
          if prev_end = cur_start then ()
          else
            let entry =
              Location_list_entry.create_location_list_entry
                ~start_of_code_symbol
                ~first_address_when_in_scope:prev_end
                ~first_address_when_not_in_scope:cur_start
                ~location_expression:Location_expression.no_location
            in
            Location_list_entry.emit entry asm
    end;
    Location_list_entry.emit entry asm;
    Some entry)) : Location_list_entry.t option);
  (* DWARF-4 spec, section 2.6.2. *)
  Dwarf_value.emit end_marker asm;
  Dwarf_value.emit end_marker asm
