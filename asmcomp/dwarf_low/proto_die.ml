(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2014, Jane Street Holding                                *)
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

type t = {
  mutable children : t list;
  tag : Tag.t;
  attribute_values : Attribute_value.t list;
  die_label : string;  (* for references between DIEs in the emitted DWARF *)
}

let die_label_counter = ref 0

let create ~parent ~tag ~attribute_values =
  begin match parent with
  | None ->
    if tag <> Tag.compile_unit then
      failwith "only compilation unit proto-DIEs may be without parents"
  | Some parent ->
    if Tag.child_determination parent.tag = Child_determination.no then
      failwith "attempt to attach proto-DIE to proto-DIE that \
                never has children"
  end;
  let die_label = Printf.sprintf "DIE_%d" !die_label_counter in
  incr die_label_counter;
  (* Insert DW_AT_sibling to point at any next sibling of [t].  (Section 2.3,
     DWARF-4 spec).  The order of siblings probably matters; we make sure that
     it is preserved by the use of :: below and within [depth_first_fold]. *)
  let attribute_values =
    match parent.children with
    | [] -> attribute_values
    | next_sibling_of_t ->
      (Attribute_value.create_sibling ~die_label:next_sibling_of_t.die_label)
        :: t.attribute_values
  in
  let t =
    { children = [];
      tag;
      attribute_values;
      die_label;
    }
  in
  parent.children <- t :: parent.children;
  t

let create_ignore ~parent ~tag ~attribute_values =
  let (_ : t) = create ~parent ~tag ~attribute_values in
  ()

let rec depth_first_fold t ~init ~f =
  let children =
    match t.children with
    | [] -> Child_determination.no
    | _ -> Child_determination.yes
  in
  let acc = f init (`DIE (t.tag, children, t.attribute_values)) in
  let rec traverse_children ts ~acc =
    match ts with
    | [] -> f acc `End_of_siblings
    | t::ts -> traverse_children ts ~acc:(depth_first_fold t ~init:acc ~f)
  in
  traverse_children t.children ~acc
