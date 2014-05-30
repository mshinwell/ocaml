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

module List = struct
  include ListLabels

  let fold = fold_left

  let filter_map t ~f =
    let output_rev =
      fold_right t
        ~init:[]
        ~f:(fun x output_rev ->
              match f x with
              | None -> output_rev
              | Some x -> x::output_rev)
    in
    rev output_rev

  let remove_consecutive_duplicates list ~equal =
    let rec loop list accum = match list with
      | [] -> accum
      | hd :: [] -> hd :: accum
      | hd1 :: hd2 :: tl ->
          if equal hd1 hd2
          then loop (hd2 :: tl) accum
          else loop (hd2 :: tl) (hd1 :: accum)
    in
    rev (loop list [])

  let dedup ?(compare=Pervasives.compare) list =
    let equal x x' = compare x x' = 0 in
    let sorted = sort compare list in
    remove_consecutive_duplicates ~equal sorted
end

module String = struct
  include String
  module Map = struct
    include Map.Make (String)
    let find t key = try Some (find key t) with Not_found -> None
    let to_alist = bindings
    let add t ~key ~data = add key data t
  end
end

let sprintf = Printf.sprintf
