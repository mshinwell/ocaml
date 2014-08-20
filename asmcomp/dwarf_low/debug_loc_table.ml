(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
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
    Attribute_value.create_location
      ~location_list_label:(Location_list.label location_list)
  in
  t := location_list::!t;
  attribute_referencing_the_new_list

let size t =
  List.fold !t
    ~init:0
    ~f:(fun size loc_list -> size + Location_list.size loc_list)

let emit t ~emitter =
  List.iter (List.rev !t) ~f:(Location_list.emit ~emitter)
