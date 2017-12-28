(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Additional information about kinds of numbers (mainly relating to
    conversions and boxing/unboxing) in a standard form that can be fed to
    functors parametric in number kinds. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module type Number_kind = sig
  module Num : sig
    include Identifiable.S

    val to_const : t -> Simple.Const.t

    val to_tagged_immediate : t -> Immediate.t
    val to_naked_float : t -> Numbers.Float_by_bit_pattern.t
    val to_naked_int32 : t -> Numbers.Int32.t
    val to_naked_int64 : t -> Numbers.Int64.t
    val to_naked_nativeint : t -> Targetint.t
  end

  val kind : Flambda_kind.Standard_int_or_float.t

  val unboxed_prover
     : (Flambda_type.t -> Num.Set.t Flambda_type.proof)
       Flambda_type.type_accessor

  val this_unboxed : Num.t -> Flambda_type.t
  val these_unboxed : Num.Set.t -> Flambda_type.t
end

(* CR mshinwell: Share code somehow with previous "module type" *)
module type Int_number_kind = sig
  module Num : sig
    include Identifiable.S

    val swap_byte_endianness : t -> t
    val neg : t -> t

    val to_const : t -> Simple.Const.t

    val to_tagged_immediate : t -> Immediate.t
    val to_naked_float : t -> Numbers.Float_by_bit_pattern.t
    val to_naked_int32 : t -> Numbers.Int32.t
    val to_naked_int64 : t -> Numbers.Int64.t
    val to_naked_nativeint : t -> Targetint.t
  end

  val kind : Flambda_kind.Standard_int_or_float.t

  val unboxed_prover
     : (Flambda_type.t -> Num.Set.t Flambda_type.proof)
       Flambda_type.type_accessor

  val this_unboxed : Num.t -> Flambda_type.t
  val these_unboxed : Num.Set.t -> Flambda_type.t
end

module type Boxable = sig
  module Num : Identifiable.S

  val boxed_prover
     : (Flambda_type.t
         -> Num.Set.t Flambda_type.ty_naked_number Flambda_type.proof)
       Flambda_type.type_accessor

  val this_boxed : Num.t -> Flambda_type.t
  val these_boxed : Num.Set.t -> Flambda_type.t

  val box : Flambda_type.t -> Flambda_type.t
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
module For_floats : Boxable_number_kind
module For_int32s : Boxable_int_number_kind
module For_int64s : Boxable_int_number_kind
module For_nativeints : Boxable_int_number_kind
