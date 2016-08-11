(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type t
  val zero : t
  val one : t
  val minus_one : t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val size : int
  val max_int : t
  val min_int : t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val of_int : int -> t
  val to_int : t -> int
  val of_float : float -> t
  val to_float : t -> float
  val of_int32 : int32 -> t
  val to_int32 : t -> int32
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t
  val to_string : t -> string
  val compare: t -> t -> int
  val equal: t -> t -> bool
end

module Int32 = struct
  include Int32
  let size = 32
  let of_int32 x = x
  let to_int32 x = x
  let of_int64 = Int64.to_int32
  let to_int64 = Int64.of_int32
end

module Int64 = struct
  include Int64
  let size = 64
  let of_int32 = Int64.of_int32
  let to_int32 = Int64.to_int32
  let of_int64 x = x
  let to_int64 x = x
end

include (val
          (match Sys.word_size with
           | 32 -> (module Int32)
           | 64 -> (module Int64)
           | _ -> assert false) : S)
