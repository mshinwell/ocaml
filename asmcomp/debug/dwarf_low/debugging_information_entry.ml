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

module A = Asm_directives
module Attribute_value = Dwarf_attribute_values.Attribute_value

type t = {
  label : Linearize.label;
  name : Backend_sym.t option;
  abbreviation_code : Abbreviation_code.t;
  attribute_values : Attribute_value.t list;
}

let create ~label ~name ~abbreviation_code ~attribute_values =
  { label;
    name;
    abbreviation_code;
    attribute_values;
  }

let null =
  lazy (
    { label = Cmm.new_label ();
      name = None;
      abbreviation_code = Abbreviation_code.null ();
      attribute_values = [];
    })

let create_null () = Lazy.force null

let emit t =
  (* The null DIE is likely to be emitted multiple times; we must not
     emit its label multiple times, or the assembler would complain.
     We don't actually need to point at the null DIE from anywhere else, so
     we elide emission of the label altogether. *)
  if t.abbreviation_code <> Abbreviation_code.null () then begin
    begin match t.name with
    | None -> ()
    | Some symbol -> A.define_data_symbol (Asm_symbol.create symbol)
    end;
    A.define_label (Asm_label.create_int t.label)
  end;
  Abbreviation_code.emit t.abbreviation_code;
  List.iter (fun av -> Attribute_value.emit av) t.attribute_values

let size t =
  List.fold_left (fun size attribute_value ->
      Dwarf_int.add size (Attribute_value.size attribute_value))
    (Abbreviation_code.size t.abbreviation_code)
    t.attribute_values

let label t = t.label
let abbreviation_code t = t.abbreviation_code
let attribute_values t = t.attribute_values
let is_null t = (t == (Lazy.force null))
let symbol t = t.name
