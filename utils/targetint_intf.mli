(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                    Mark Shinwell, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type Common = sig

  type t

  include Identifiable.S with type t := t

  val to_string : t -> string

  val zero : t
  val one : t
  val minus_one : t

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t

  val min: t -> t -> t
  val max: t -> t -> t

  val get_least_significant_16_bits_then_byte_swap : t -> t

  module Pair : sig
    type nonrec t = t * t
    include Identifiable.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t

end

module type S = sig
  include Common

  val max_int : t
  val min_int : t

  val rem : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val swap_byte_endianness : t -> t

  val of_int_exn : int -> t

  val of_int : int -> t
  val of_int32 : int32 -> t
  val of_int64 : int64 -> t
  val of_float : float -> t
  val of_string : string -> t

  val to_int : t -> int
  val to_int32 : t -> int32
  val to_int64 : t -> int64
  val to_float : t -> float
  val to_string : t -> string

  val unsigned_div : t -> t -> t
  val unsigned_rem : t -> t -> t
  val unsigned_compare : t -> t -> int
end

module type OCaml = sig

  include Common

  type targetint

  val min_value : t
  val max_value : t
  val max_string_length : t
  val ten : t
  val hex_ff : t

  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val xor : t -> t -> t
  val mod_ : t -> t -> t

  val (<=) : t -> t -> bool
  val (<) : t -> t -> bool

  val bottom_byte_to_int : t -> int

  val of_int_option : int -> t option

  val of_char : char -> t
  val of_int : int -> t  (* CR mshinwell: clarify semantics *)
  val of_int32 : int32 -> t
  val of_int64 : int64 -> t
  val of_targetint : targetint -> t
  val of_float : float -> t

  val to_float : t -> float
  val to_int : t -> int
  val to_int_exn : t -> int
  val to_int_option : t -> int option
  val to_int32 : t -> int32
  val to_int64 : t -> int64
  val to_targetint : t -> targetint

end
