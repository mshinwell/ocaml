(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Additional information about kinds of numbers (mainly relating to
    conversions and boxing/unboxing) in a standard form that can be fed to
    functors parametric in number kinds. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type Num_common = sig
  include Identifiable.S

  module Pair : sig
    type nonrec t = t * t
    include Identifiable.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t

  val zero : t
  val one : t
  val minus_one : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t option
  val mod_ : t -> t -> t option

  val to_const : t -> Reg_width_const.t

  val to_immediate : t -> Targetint_31_63.t
  val to_naked_float : t -> Numbers.Float_by_bit_pattern.t
  val to_naked_int32 : t -> Numbers.Int32.t
  val to_naked_int64 : t -> Numbers.Int64.t
  val to_naked_nativeint : t -> Targetint.t
end

module type Number_kind_common = sig
  module Num : Identifiable.S

  (* CR mshinwell: Rename to standard_int_or_float_kind? *)
  val kind : Flambda_kind.Standard_int_or_float.t

  val unboxed_prover
     : (Flambda_type.t -> Num.Set.t Flambda_type.proof)
       Flambda_type.type_accessor

  val this_unboxed : Num.t -> Flambda_type.t
  val these_unboxed : Num.Set.t -> Flambda_type.t

  val term_unboxed : Num.t -> Flambda.Named.t
end

module type Number_kind = sig
  module Num : Num_common
  include Number_kind_common with module Num := Num
end

module type Int_number_kind = sig
  module Num : sig
    include Num_common

    val and_ : t -> t -> t
    val or_ : t -> t -> t
    val xor : t -> t -> t
    val shift_left : t -> Targetint_31_63.t -> t
    (* [shift_right] is arithmetic shift right, matching [Int32],
       [Int64], etc. *)
    val shift_right : t -> Targetint_31_63.t -> t
    val shift_right_logical : t -> Targetint_31_63.t -> t
    val swap_byte_endianness : t -> t
    val neg : t -> t

    val compare_unsigned : t -> t -> int
  end

  include Number_kind_common with module Num := Num

  val standard_int_kind : Flambda_kind.Standard_int.t
end

module type Boxable = sig
  module Num : Identifiable.S

  val boxable_number_kind : Flambda_kind.Boxable_number.t

  val boxed_prover
     : (Flambda_type.t -> Num.Set.t Flambda_type.proof)
         Flambda_type.type_accessor

  val this_boxed : Num.t -> Flambda_type.t
  val these_boxed : Num.Set.t -> Flambda_type.t

  val box : Flambda_type.t -> Flambda_type.t

  type naked_number_kind
end

module type Boxable_number_kind = sig
  include Number_kind
  include Boxable with module Num := Num
end

module type Boxable_int_number_kind = sig
  include Int_number_kind
  include Boxable with module Num := Num
end

module For_tagged_immediates : Int_number_kind
module For_naked_immediates : Int_number_kind
module For_floats : Boxable_number_kind
module For_int32s : Boxable_int_number_kind
module For_int64s : Boxable_int_number_kind
module For_nativeints : Boxable_int_number_kind
