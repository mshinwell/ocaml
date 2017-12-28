(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Modules about numbers, some of which satisfy {!Identifiable.S}. *)

module Int : sig
  include Identifiable.S with type t = int

  (** [zero_to_n n] is the set of numbers \{0, ..., n\} (inclusive). *)
  val zero_to_n : int -> Set.t
end

module Int8 : sig
  type t

  val zero : t
  val one : t

  val of_int_exn : int -> t
  val to_int : t -> int
end

module Int16 : sig
  type t

  val of_int_exn : int -> t
  val of_int64_exn : Int64.t -> t

  val to_int : t -> int
end

module Float : Identifiable.S with type t = float

module Float_by_bit_pattern : sig
  (** Floating point numbers whose comparison and equality relations are
      the usual [Int64] relations on the bit patterns of the floats.  This
      in particular means that different representations of NaN will be
      distinguished, as will the two signed zeros.

      Never use [Pervasives.compare] on values of type [t].  Use either
      [compare] (comparison on bit patterns) or [IEEE_semantics.compare]
      depending on which semantics you want.  Likewise for equality.
  *)

  include Identifiable.S

  val create : float -> t

  val of_bits : Int64.t -> t
  val of_string : string -> t

  val to_float : t -> float

  val one : t
  val zero : t
  val minus_one : t

  val is_either_zero : t -> bool
  val is_any_nan : t -> bool

  module IEEE_semantics : sig
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t

    val neg : t -> t
    val abs : t -> t

    val compare : t -> t -> int
    val equal : t -> t -> bool
  end
end

module Int32 : sig
  include module type of struct include Int32 end
  include Identifiable.S with type t := Int32.t

  val swap_byte_endianness : t -> t
end

module Int64 : sig
  include module type of struct include Int64 end
  include Identifiable.S with type t := Int64.t

  val swap_byte_endianness : t -> t
end

(* CR mshinwell: We may be able to remove this (all uses for target numbers
   should use Targetint instead) *)
module Nativeint : sig
  include module type of struct include Nativeint end
  include Identifiable.S with type t := Nativeint.t

  val swap_byte_endianness : t -> t
end
