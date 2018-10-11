(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module A = Asm_directives

type t = {
  name : Linearize.label;
  entries : Location_list_entry.t list;
}

(* It isn't exactly clear what the sorting requirement is, but we sort
   within a location list by increasing virtual memory address on the
   start addresses of the entries. *)
let sort entries =
  List.sort Location_list_entry.compare_ascending_vma entries

let create ~location_list_entries =
  { name = Cmm.new_label ();
    entries = sort location_list_entries;
  }

let label t = t.name

let end_marker () =
  Dwarf_value.Absolute_code_address Targetint.zero

let size t =
  let (+) = Dwarf_int.add in
  let body_size =
    List.fold_left (fun size entry -> size + (Location_list_entry.size entry))
      (Dwarf_int.zero ())
      t.entries
  in
  let end_marker = end_marker () in
  body_size + Dwarf_value.size end_marker + Dwarf_value.size end_marker

let compare_increasing_vma t1 t2 =
  match t1.entries, t2.entries with
  | t1_entry::_, t2_entry::_ ->
    Location_list_entry.compare_ascending_vma t1_entry t2_entry
  | _ -> failwith "Location_list.compare on empty location list(s)"

let emit t =
  A.define_label (Asm_label.create_int t.name);
  List.iter (fun entry -> Location_list_entry.emit entry) t.entries;
  (* DWARF-4 spec, section 2.6.2. *)
  let end_marker = end_marker () in
  Dwarf_value.emit end_marker;
  Dwarf_value.emit end_marker
