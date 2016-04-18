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

type t = Location_list.t list ref

let create () = ((ref []) : t)

let insert t ~location_list =
(*
  let size_so_far =
    List.fold t
      ~init:0
      ~f:(fun size loc_list -> size + Location_list.size loc_list)
  in
  let attribute_referencing_the_new_list =
    Attribute_value.create_location
      ~offset_from_start_of_debug_loc:size_so_far
  in
*)
  let attribute_referencing_the_new_list =
    let spec =
      Dwarf_attributes.Attribute_specification.create
        Dwarf_attributes.Attribute.Location
        Dwarf_attributes.Form.Sec_offset_loclistptr
    in
    Dwarf_attribute_values.Attribute_value.create spec
      (Dwarf_attribute_values.Value.offset_into_debug_loc
        (Location_list.label location_list))
  in
  t := location_list :: !t;
  attribute_referencing_the_new_list

let size t =
  List.fold !t
    ~init:0L
    ~f:(fun size loc_list -> Int64.add size (Location_list.size loc_list))

(*
let sort location_lists =
  List.sort Location_list.compare_increasing_vma location_lists
*)

let emit t asm =
  List.iter !t ~f:(fun loc_list -> Location_list.emit loc_list asm)
