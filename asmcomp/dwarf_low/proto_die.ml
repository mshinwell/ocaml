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
  parent : t option;
  mutable children : t list;
  tag : Tag.t;
  mutable attribute_values : Attribute_value.t list;
  label : Linearize.label;  (* for references between DIEs *)
}

let sort_attribute_values ~attribute_values =
  List.sort (fun av1 av2 ->
    Attribute.compare
      (Attribute_value.attribute av1) (Attribute_value.attribute av2))
    attribute_values

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
  (* We impose an ordering on attributes to ensure that sharing of abbreviation
     table entries (that is to say, patterns of attributes) is maximised. *)
  let attribute_values = sort_attribute_values ~attribute_values in
  (* Insert DW_AT_sibling to point at any next sibling of [t], if this new
     node might have children.  (Section 2.3, DWARF-4 spec; and it seems
     pointless if the node can never have children).  The order of siblings
     probably matters; we make sure that it is preserved by the use of ::
     below and within [depth_first_fold]. *)
  let attribute_values =
    if Tag.child_determination tag = Child_determination.no then
      attribute_values
    else
      match parent with
      | None -> attribute_values
      | Some parent ->
        match parent.children with
        | [] -> attribute_values
        | next_sibling_of_t::_ ->
          (Attribute_value.create_sibling ~proto_die:next_sibling_of_t.label)
            :: attribute_values
  in
  let t =
    { parent;
      children = [];
      tag;
      attribute_values;
      label = Linearize.new_label ();
    }
  in
  begin match parent with
  | None -> ()
  | Some parent -> parent.children <- t :: parent.children
  end;
  t

let create_ignore ~parent ~tag ~attribute_values =
  let (_ : t) = create ~parent ~tag ~attribute_values in
  ()

let duplicate_as_sibling t =
  (* All we need to do is to copy the top level of [t] (assigning a new
     [label] in the process, of course); the new proto-DIE will then share
     the remainder with [t].  The sharing will be invisible after flattening
     using [depth_first_fold], below, and as such should cause no trouble
     during conversion to DWARF. *)
  let attribute_values =
    ListLabels.filter t.attribute_values ~f:(fun attribute_value ->
      let attribute = Attribute_value.attribute attribute_value in
      Attribute.compare attribute Attribute.sibling <> 0)
  in
  create ~parent:t.parent ~tag:t.tag ~attribute_values

let change_name_attribute_value t ~new_name =
  let attribute_values =
    ListLabels.map t.attribute_values ~f:(fun attribute_value ->
      let attribute = Attribute_value.attribute attribute_value in
      if Attribute.compare attribute Attribute.name = 0 then
        Attribute_value.create_name new_name
      else
        attribute_value)
  in
  t.attribute_values <- sort_attribute_values ~attribute_values

let rec depth_first_fold t ~init ~f =
  let children =
    match t.children with
    | [] -> Child_determination.no
    | _ -> Child_determination.yes
  in
  let acc = f init (`DIE (t.tag, children, t.attribute_values, t.label)) in
  match t.children with
  | [] -> acc
  | _ ->
    let rec traverse_children ts ~acc =
      match ts with
      | [] -> f acc `End_of_siblings
      | t::ts -> traverse_children ts ~acc:(depth_first_fold t ~init:acc ~f)
    in
    traverse_children t.children ~acc

let reference t = t.label
