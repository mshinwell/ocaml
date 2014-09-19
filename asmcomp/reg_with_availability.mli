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
val definitely_holds_non_ptr : t -> bool

module Set : sig
  type ra = t

  (* There is at most one confidence judgement for a given register
     in a value of type [t]. *)
  type t

  val equal : t -> t -> bool
  val subset : t -> t -> bool

  val mem : t -> ra -> bool
  (* CR mshinwell: delete this if unused *)
  val mem_reg : t -> Reg.t -> bool

  val empty : t

  (* CR-soon mshinwell: fix argument orderings *)
  val add : ra -> t -> t
  val remove : ra -> t -> t

  (* [of_array] assigns [`Definitely] confidence to each register. *)
  val of_array : Reg.t array -> t

  (* [inter] may lower confidence, but never raises it. *)
  val inter : t -> t -> t

  (* [diff] treats elements with the same register but different confidences
     as different.  For example:
     { r1/Maybe } \ { r1/Definitely } = { r1/Maybe }. *)
  val diff : t -> t -> t

  val filter_and_change_confidence
     : t
    -> f:(ra -> [ `Unchanged | `Degrade | `Remove ])
    -> t

  val iter : t -> f:(ra -> unit) -> unit
  val fold : t -> init:'a -> f:(ra -> 'a -> 'a) -> 'a
  val filter : t -> f:(ra -> bool) -> t
  val partition : t -> f:(ra -> bool) -> t * t

  val to_list : t -> ra list
end

module Map : Map.S with type key = t
