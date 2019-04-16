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

type repr =
  | Int32 of int32
  | Int64 of int64

type num_bits =
  | Thirty_two
  | Sixty_four

(* CR mshinwell: Stop duplicating this signature, use a .intf file *)

module type S = sig
  type t
  type targetint = t
  val zero : t
  val one : t
  val minus_one : t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val unsigned_div : t -> t -> t
  val rem : t -> t -> t
  val unsigned_rem : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val num_bits : num_bits
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
  val of_int_exn : int -> t
  val to_int : t -> int
  val of_float : float -> t
  val to_float : t -> float
  val of_int32 : int32 -> t
  val to_int32 : t -> int32
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t
  val to_string : t -> string
  val unsigned_compare : t -> t -> int
  val repr: t -> repr
  val min: t -> t -> t
  val max: t -> t -> t
  val get_least_significant_16_bits_then_byte_swap : t -> t
  val swap_byte_endianness : t -> t

  include Identifiable.S with type t := t

  module Targetint_set = Set

  module Pair : sig
    type nonrec t = t * t
    include Identifiable.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t

  module OCaml : sig
    type t
    type targetint_ocaml = t
    val min_value : t
    val max_value : t
    val max_string_length : t
    val minus_one : t
    val zero : t
    val one : t
    val ten : t
    val hex_ff : t
    val (<=) : t -> t -> bool
    val (<) : t -> t -> bool
    val bottom_byte_to_int : t -> int
    val of_char : char -> t
    val of_int : int -> t  (* CR mshinwell: clarify semantics *)
    val of_int_option : int -> t option
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

    val neg : t -> t
    val get_least_significant_16_bits_then_byte_swap : t -> t
    val swap_byte_endianness : t -> t

    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val mod_ : t -> t -> t
    val div : t -> t -> t

    val and_ : t -> t -> t
    val or_ : t -> t -> t
    val xor : t -> t -> t

    val shift_left : t -> int -> t
    val shift_right : t -> int -> t
    val shift_right_logical : t -> int -> t

    val max : t -> t -> t

    include Identifiable.S with type t := t

    val set_of_targetint_set : Targetint_set.t -> Set.t

    module Pair : sig
      type nonrec t = t * t

      include Identifiable.S with type t := t
    end

    val cross_product : Set.t -> Set.t -> Pair.Set.t

    module Or_unknown : sig
      type nonrec t = private
        | Ok of t
        | Unknown

      val ok : targetint_ocaml -> t
      val unknown : unit -> t

      include Identifiable.S with type t := t
    end
  end
end

let size = Sys.word_size
(* Later, this will be set by the configure script
   in order to support cross-compilation. *)

module Int32 = struct
  include Int32

  type targetint = t

  let of_int_exn =
    match Sys.word_size with (* size of [int] *)
    | 32 ->
        Int32.of_int
    | 64 ->
        fun n ->
          if n < Int32.(to_int min_int) || n > Int32.(to_int max_int) then
            Misc.fatal_errorf "Targetint.of_int_exn: 0x%x out of range" n
          else
            Int32.of_int n
    | _ ->
        assert false
  let num_bits = Thirty_two
  let of_int32 x = x
  let to_int32 x = x
  let of_int64 = Int64.to_int32
  let to_int64 = Int64.of_int32
  let repr x = Int32 x

  include Identifiable.Make (struct
    type nonrec t = t
    let compare = Int32.compare
    let equal = Int32.equal
    let hash = Hashtbl.hash
    let output _ _ = Misc.fatal_error "Not implemented"
    let print ppf t = Format.fprintf ppf "%ld" t
  end)

  let min t1 t2 =
    if compare t1 t2 <= 0 then t1 else t2

  let max t1 t2 =
    if compare t1 t2 <= 0 then t2 else t1

  module Targetint_set = Set

  module Pair = struct
    type nonrec t = t * t

    module T_pair = Identifiable.Pair (T) (T)

    include Identifiable.Make (T_pair)
  end

  let cross_product set1 set2 =
    Set.fold (fun elt1 result ->
        Set.fold (fun elt2 result ->
            Pair.Set.add (elt1, elt2) result)
          set2
          result)
      set1
      Pair.Set.empty

  let get_least_significant_16_bits_then_byte_swap t =
    let least_significant_byte = Int32.logand t 0xffl in
    let second_to_least_significant_byte =
      shift_right_logical (Int32.logand t 0xff00l) 8
    in
    Int32.logor second_to_least_significant_byte
      (shift_left least_significant_byte 8)

  external swap_byte_endianness : t -> t = "%bswap_int32"

  module OCaml = struct
    type nonrec t = t

    type targetint_ocaml = t

    let minus_one = (-1l)
    let zero = 0l
    let one = 1l
    let ten = 10l
    let hex_ff = 0xffl

    (* XXX Implement correctly *)

    let min_value = Int32.min_int
    let max_value = Int32.max_int

    let sub = Int32.sub
    let neg = Int32.neg

    let swap_byte_endianness = swap_byte_endianness

    let shift_left = Int32.shift_left
    let shift_right = Int32.shift_right
    let shift_right_logical = Int32.shift_right_logical

    let xor = Int32.logxor
    let or_ = Int32.logor
    let and_ = Int32.logand
    let mod_ = Int32.rem
    let div = Int32.div
    let mul = Int32.mul
    let add = Int32.add

    let bottom_byte_to_int t =
      Int32.to_int (Int32.logand t hex_ff)

    let of_char c =
      Int32.of_int (Char.code c)

    let of_int = Int32.of_int
    let to_int = Int32.to_int

    let of_int32 t = t (* CR mshinwell: Overflow semantics? *)
    let of_int64 t = Int64.to_int32 t (* CR mshinwell: Overflow semantics? *)

    let to_int32 t = t
    let to_int64 t = Int64.of_int32 t
    let to_targetint t = t

    let of_float t = of_int64 (Int64.bits_of_float t)
    let to_float t = Int64.float_of_bits (to_int64 t)

    let of_int_option i =
      let t = of_int i in
      let via_t = Int64.of_int32 t in
      let not_via_t = Int64.of_int i in
      if Int64.equal via_t not_via_t then Some t
      else None

    let to_int_option t = (* XXX this is wrong, implement correctly *)
      Some (to_int t)

    let to_int_exn t =
      match to_int_option t with
      | Some i -> i
      | None -> Misc.fatal_errorf "Targetint.OCaml.to_int_exn %ld" t

    (* CR mshinwell: Overflow semantics? *)
    let of_targetint t = t

    (* XXX This needs to be retrieved properly.
       Also, there are bugs in asmcomp/closure.ml and cmmgen.ml where max_wosize
       is being calculated ignoring any PROFINFO_WIDTH *)
    let max_array_length = Int32.sub (Int32.shift_left 1l 22) 1l

    let max_string_length =
      Int32.sub (Int32.mul 4l max_array_length) 1l

    let max t1 t2 =
      if Stdlib.compare t1 t2 < 0 then t2 else t1

    (* CR mshinwell: implement *)
    let get_least_significant_16_bits_then_byte_swap _t = assert false

    let compare = compare
    let equal = equal
    let hash = hash
    let output = output
    let print = print

    module T = T
    module Map = Map
    module Set = Set
    module Tbl = Tbl

    module Pair = Pair
    let cross_product = cross_product

    let set_of_targetint_set set = set

    module Or_unknown = struct
      type nonrec t =
        | Ok of t
        | Unknown

      let ok imm = Ok imm
      let unknown () = Unknown

      include Identifiable.Make (struct
        type nonrec t = t

        let compare t1 t2 =
          match t1, t2 with
          | Ok _, Unknown -> -1
          | Unknown, Ok _ -> 1
          | Unknown, Unknown -> 0
          | Ok imm1, Ok imm2 -> compare imm1 imm2

        let equal t1 t2 = (compare t1 t2 = 0)

        let hash t =
          match t with
          | Ok imm -> Hashtbl.hash (0, hash imm)
          | Unknown -> Hashtbl.hash 1

        let print ppf t =
          match t with
          | Ok imm -> print ppf imm
          | Unknown -> Format.pp_print_string ppf "Unknown"

        let output chan t =
          print (Format.formatter_of_out_channel chan) t
      end)
    end

    let (<=) t1 t2 = Int32.compare t1 t2 <= 0
    let (<) t1 t2 = Int32.compare t1 t2 < 0
  end
end

module Int64 = struct
  include Int64

  type targetint = t

  let num_bits = Sixty_four
  let of_int_exn = Int64.of_int
  let of_int64 x = x
  let to_int64 x = x
  let repr x = Int64 x

  let min t1 t2 =
    if compare t1 t2 <= 0 then t1 else t2

  let max t1 t2 =
    if compare t1 t2 <= 0 then t2 else t1

  include Identifiable.Make (struct
    type nonrec t = t
    let compare = Int64.compare
    let equal t1 t2 = (compare t1 t2 = 0)
    let hash = Hashtbl.hash
    let print ppf t = Format.fprintf ppf "%Ld" t
    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  module Targetint_set = Set

  module Pair = struct
    type nonrec t = t * t

    module T_pair = Identifiable.Pair (T) (T)

    include Identifiable.Make (T_pair)
  end

  let cross_product set1 set2 =
    Set.fold (fun elt1 result ->
        Set.fold (fun elt2 result ->
            Pair.Set.add (elt1, elt2) result)
          set2
          result)
      set1
      Pair.Set.empty

  let get_least_significant_16_bits_then_byte_swap t =
    let least_significant_byte = Int64.logand t 0xffL in
    let second_to_least_significant_byte =
      Int64.shift_right_logical (Int64.logand t 0xff00L) 8
    in
    Int64.logor second_to_least_significant_byte
      (Int64.shift_left least_significant_byte 8)

  external swap_byte_endianness : t -> t = "%bswap_int32"

  module OCaml = struct
    type nonrec t = t

    type targetint_ocaml = t

    let minus_one = (-1L)
    let zero = 0L
    let one = 1L
    let ten = 10L
    let hex_ff = 0xffL

    (* XXX Implement correctly *)
    let min_value = Int64.min_int
    let max_value = Int64.max_int

    (* XXX Implement correctly *)
    let sub = Int64.sub
    let neg = Int64.neg

    let shift_left = Int64.shift_left
    let shift_right = Int64.shift_right
    let shift_right_logical = Int64.shift_right_logical

    let xor = Int64.logxor
    let or_ = Int64.logor
    let and_ = Int64.logand
    let mod_ = Int64.rem
    let div = Int64.div
    let mul = Int64.mul
    let add = Int64.add

    let swap_byte_endianness = swap_byte_endianness

    let bottom_byte_to_int t =
      Int64.to_int (Int64.logand t hex_ff)

    let of_char c =
      Int64.of_int (Char.code c)

    let of_int = Int64.of_int
    let to_int = Int64.to_int

    let to_int_option t = (* XXX this is wrong, implement correctly *)
      Some (to_int t)

    let to_int_exn t =
      match to_int_option t with
      | Some i -> i
      | None -> Misc.fatal_errorf "Targetint.OCaml.to_int_exn %Ld" t

    let of_int32 t = Int64.of_int32 t
    let of_int64 t = t (* CR mshinwell: Overflow semantics? *)
    let of_float t = Int64.bits_of_float t

    let to_int32 t = Int64.to_int32 t
    let to_int64 t = t
    let to_targetint t = t
    let to_float t = Int64.float_of_bits t

    let of_int_option i = Some (of_int i)

    (* CR mshinwell: Overflow semantics? *)
    let of_targetint t = t

    let max_array_length = Int64.sub (Int64.shift_left 1L 54) 1L

    let max_string_length =
      Int64.sub (Int64.mul 8L max_array_length) 1L

    let max t1 t2 =
      if Stdlib.compare t1 t2 < 0 then t2 else t1

    (* CR mshinwell: implement *)
    let get_least_significant_16_bits_then_byte_swap _t = assert false

    let compare = compare
    let equal = equal
    let hash = hash
    let output = output
    let print = print

    module T = T
    module Map = Map
    module Set = Set
    module Tbl = Tbl

    module Pair = Pair
    let cross_product = cross_product

    let set_of_targetint_set set = set

    (* CR mshinwell: share code with 32-bit version above *)
    module Or_unknown = struct
      type nonrec t =
        | Ok of t
        | Unknown

      let ok imm = Ok imm
      let unknown () = Unknown

      include Identifiable.Make (struct
        type nonrec t = t

        let compare t1 t2 =
          match t1, t2 with
          | Ok _, Unknown -> -1
          | Unknown, Ok _ -> 1
          | Unknown, Unknown -> 0
          | Ok imm1, Ok imm2 -> compare imm1 imm2

        let equal t1 t2 = (compare t1 t2 = 0)

        let hash t =
          match t with
          | Ok imm -> Hashtbl.hash (0, hash imm)
          | Unknown -> Hashtbl.hash 1

        let print ppf t =
          match t with
          | Ok imm -> print ppf imm
          | Unknown -> Format.pp_print_string ppf "Unknown"

        let output chan t =
          print (Format.formatter_of_out_channel chan) t
      end)
    end

    let (<=) t1 t2 = Stdlib.(<=) (compare t1 t2) 0
    let (<) t1 t2 = Stdlib.(<) (compare t1 t2) 0
  end
end

include (val
          (match size with
           | 32 -> (module Int32)
           | 64 -> (module Int64)
           | _ -> assert false
          ) : S)
