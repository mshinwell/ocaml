(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

include Emittable.S with type t := t

val at_offset_from_symbol
   : base:string
  -> symbol:Symbol.t
  -> offset_in_bytes:Target_addr.t
  -> t

val at_symbol : Symbol.t -> t

val register : reg_number:int -> t

val register_based_addressing
   : reg_number:int
  -> offset_in_bytes:Target_addr.t
  -> t

val frame_base_register : offset_in_bytes:Target_addr.t -> t

type implicit_value =
  | Int of int

val implicit : implicit_value -> t
