(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                  Mark Shinwell, Jane Street Europe                  *)
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

module L = Linearize

module Lexical_block : sig
  type t

  val create
     : start_pos:[ `Start_of_function | `At_label of L.label ]
    -> end_pos:L.label
    -> t

  val name : t -> string

  val start_pos : t -> L.label
  val end_pos : t -> L.label
end = struct
  type t = {
    start_pos : [ `Start_of_function | `At_label of L.label ];
    end_pos : L.label;
    name : string;
  }

  let create ~start_pos ~end_pos ~function_name ~id =
    let name = Printf.sprintf "L%s.lb%d" function_name id in
    { start_pos; end_pos; name; }

  let start_pos t = t.start_pos
  let end_pos t = t.end_pos
  let name t = t.name
end

type t =
  (bool  (* = [is_unique] (see available_ranges.mli) *)
    * [ `Block_scope of Lexical_block.t | `Function_scope ]
    * Available_range.t) Ident.tbl

let create ~available_ranges =
  Available_ranges.fold available_ranges
    ~init:(Ident.empty, 0)
    ~f:(fun (t, id) ~ident ~is_unique ~range ->
      let scope, id =
        if Available_range.is_parameter range then
          `Function_scope, id
        else
          let start_pos, end_pos = Available_range.extremities range in
          let block =
            Lexical_block.create ~start_pos ~end_pos ~id
              ~function_name:(Available_ranges.function_name available_ranges)
          in
          (`Block_scope block), id + 1
      in
      let t = Ident.add ident (is_unique, scope, range) t in
      t, id)

let fold t ~init ~f =
  Ident.fold_all (fun ident (is_unique, scope, range) acc ->
    f acc ~ident ~is_unique ~scope ~range)
