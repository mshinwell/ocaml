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

module Confidence = struct
  type t = [ `Definitely | `Maybe ]

  let inter t1 t2 =
    match t1, t2 with
    | `Definitely, `Definitely -> `Definitely
    | `Definitely, `Maybe | `Maybe, `Definitely | `Maybe, `Maybe -> `Maybe

  let compare t1 t2 =
    match t1, t2 with
    | `Definitely, `Definitely | `Maybe, `Maybe -> 0
    | `Definitely, `Maybe -> -1
    | `Maybe, `Definitely -> 1
end

module T = struct
  type t = Reg.t * Confidence.t

  let compare (r1, c1) (r2, c2) =
    let result = Reg.compare_stamps r1 r2 in
    if result <> 0 then result
    else Confidence.compare c1 c2
end

include T

let reg = fst
let confidence = snd

let definitely_holds_non_ptr t = Reg.holds_non_pointer (reg t)

module Set = struct
  type ra = t
  include Set.Make (T)

  let of_array rs =
    match Array.length rs with
    | 0 -> empty
    | 1 -> add (rs.(0), `Definitely) empty
    | n ->
      let rec add_all i =
        if i >= n then empty
        else add (rs.(i), `Definitely) (add_all (i + 1))
      in
      add_all 0

  let iter t ~f = iter f t
  let fold t ~init ~f = fold f t init
  let filter t ~f =
    fold t ~init:empty
      ~f:(fun ra result -> if f ra then add ra result else result)
  let partition t ~f = partition f t

  let mem t ra = mem ra t

  let mem_reg t reg' =
    let t = filter t ~f:(fun ra -> reg' = reg ra) in
    match elements t with
    | [] -> false
    | [_ra] -> true
    | _ -> assert false

  let filter_and_change_confidence t ~f =
    fold t
      ~init:empty
      ~f:(fun ra result ->
        match f ra with
        | `Unchanged -> add ra result
        | `Remove -> result
        | `Degrade -> add (reg ra, `Maybe) result)

  let inter t1 t2 =
    fold t1
      ~init:empty
      ~f:(fun ra1 result ->
        try
          let t2 =
            filter t2
              ~f:(fun ra2 -> Reg.compare_stamps (reg ra1) (reg ra2) = 0)
          in
          begin match elements t2 with
          | [] -> result
          | [ra2] ->
            let conf1 = confidence ra1 in
            let conf2 = confidence ra2 in
            add (reg ra1, Confidence.inter conf1 conf2) result
          | _ -> assert false  (* at most one confidence per reg. *)
          end
        with Not_found -> result)

  let to_list = elements
end

module Map = struct
  include Map.Make (T)
end
