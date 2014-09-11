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

(* A pair of a pseudoregister and a confidence judgement as to its
   availability. *)
type t

val reg : t -> Reg.t
val confidence : t -> [ `Definitely | `Maybe ]

module Set : sig
  type ra = t

  (* There is at most one confidence judgement for a given register
     in a value of type [t]. *)
  type t

  (* [of_array] assigns [`Definitely] confidence to each register. *)
  val of_array : Reg.t array -> t

  (* [inter] may lower confidence, but never raises it. *)
  val inter : t -> t -> t

  val filter_and_change_confidence
     : t
    -> f:(ra -> [ `Unchanged | `Degrade | `Remove ])
    -> t

  val fold : t -> init:'a -> f:(ra -> 'a -> 'a) -> 'a
end

module Map : Map.S with type key = t
