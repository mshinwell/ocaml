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

open Std_internal

type t = Abbreviations_table_entry.t list

let create () = []

let add t entry = entry::t

let find t ~tag ~has_children ~attribute_specs =
  try
    Some (Abbreviations_table_entry.abbreviation_code
      (List.find t ~f:(fun entry ->
        tag = Abbreviations_table_entry.tag entry
          && has_children = Abbreviations_table_entry.has_children entry
          && Dwarf_attributes.Attribute_specification.Sealed.Set.equal
            attribute_specs
            (Abbreviations_table_entry.attribute_specs entry))))
  with Not_found -> None

let size t =
  let (+) = Int64.add in
  List.fold t
    ~init:Int64.zero
    ~f:(fun size entry -> size + Abbreviations_table_entry.size entry)
    (* See below re. the zero word. *)
    + Dwarf_value.size (Dwarf_value.Uleb128 0L)

let emit t asm =
  List.iter (List.rev t)
    ~f:(fun entry -> Abbreviations_table_entry.emit entry asm);
  (* DWARF-4 spec section 7.5.3: "The abbreviations for a given compilation
     unit end with an entry consisting of a 0 byte for the abbreviation
     code." *)
  Dwarf_value.emit (Dwarf_value.Uleb128 0L) asm
