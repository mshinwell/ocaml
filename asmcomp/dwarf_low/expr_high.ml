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

type sint = [ `signed_int ]
type uint = [ `unsigned_int ]
type sint_or_uint = [ `signed_int | `unsigned_int ]
type sleb128 = [ `signed_leb128 ]
type uleb128 = [ `unsigned_leb128 ]
type ptr = [ `pointer ]
type reg = [ `register ]
type boolean = [ `boolean ]
type _ integer =
  | Signed : sint integer
  | Unsigned : uint integer
type _ integer_or_ptr =
  | Signed : sint integer
  | Unsigned : uint integer
  | Ptr : ptr

type 'a internal_var = 'a

type cond =
  | Eq | Ge | Gt | Le | Lt | Ne

type _ unop =
  | Neg : sint unop
  | Abs : sint unop
  | Not : uint unop

type binop =
  | Plus | Minus | Mul | Div | Mod | And | Or | Xor | Lsl | Lsr | Asr

type boolean_binop =
  | And | Or

type load_type =
  | Whole_word | This_many_bytes of Int8.t

type _ t =
  | Zero : 'a integer t
  | One : 'a integer t
  | Const_int : Int64.t -> 'a integer t
  | Const_ptr : Target_width.t -> ptr t
  | Var : int -> 'a internal_var t
  | Let : 'a t * ('a t -> 'b t) -> 'b t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Fst : ('a * 'b) t -> 'a t
  | Snd : ('a * 'b) t -> 'b t
  | Unop : 'a unop * 'a integer t -> 'a integer t
  | Binop : binop * 'a integer t * 'a integer t -> 'a integer t
  | Bool_binop : boolean_binop * boolean t * boolean t -> boolean t
  | Ptr_binop : binop * ptr t * uint integer t -> ptr t
  | Cond : cond * sint integer t * sint integer t -> boolean t
  | Ptr_cond : cond * ptr t * ptr t -> boolean t
  | If : boolean t * 'a t * 'a t -> 'a t
  | Load : load_type * ptr t -> 'a integer_or_ptr t
  | Call : 'a t * ('a, 'b) dwarf_function -> 'b t
  | Canonical_frame_address : ptr t
  | Reg_plus_offset : Reg.t * Target_width.t -> ptr t
  | Frame_base_reg_plus_offset : Target_width.t -> ptr t



val create_function
   : ('a t -> 'b t)
  -> name:string
  -> ('a, 'b) dwarf_function
