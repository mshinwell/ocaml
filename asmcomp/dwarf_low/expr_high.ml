(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Holding                                *)
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

module Function_name : sig
  type t
end

type cond =
  | Eq | Ge | Gt | Le | Lt | Ne

type t =
  | Unit
  | Signed_int of Int64.t
  | Unsigned_int of Int64.t
  | Address of Target_width.t
  | Var of Var.t
  | Let of Var.t * t * t
  | Neg of t
  | Plus of t * t
  | Minus of t * t
  | Mul of t * t
  | Div of t * t
  | Mod of t * t
  | Abs of t
  | Not of t
  | And of t * t
  | Or of t * t
  | Xor of t * t
  | Lsl of t * [ `by of t ]
  | Lsr of t * [ `by of t ]
  | Asr of t * [ `by of t ]
  | Deref of [ `Whole_word | `This_many_bytes of Int8.t ] * t
  | Call of Proto_DIE.t
  | Cond of cond * t
  | If of t * t * t
  | Canonical_frame_address
  | Contents_of_reg_plus_offset of Reg.t * Target_width.t
  | Contents_of_frame_base_reg_plus_offset of Target_width.t
