(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Kinds of Flambda types. *)

module Naked_number_kind : sig
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val print : Format.formatter -> t -> unit
end

(* CR mshinwell: Once disambiguation works on GADTs, consider turning [t]
   into a GADT. *)
type t = private
  | Value
  | Naked_number of Naked_number_kind.t
  | Fabricated
    (** Values which have been introduced by Flambda and are never accessible
        at the source language level (for example sets of closures). *)

type kind = t

val value : unit -> t
val naked_immediate : unit -> t
val naked_float : unit -> t
val naked_int32 : unit -> t
val naked_int64 : unit -> t
val naked_nativeint : unit -> t
val fabricated : unit -> t

val is_value : t -> bool
val is_naked_float : t -> bool

(** The kind of the unit value. *)
val unit : unit -> t

(** [compatible t ~if_used_at] returns [true] iff a value of the kind [t] may
    be used in any context with a hole expecting a value of kind [if_used_at].
*)
val compatible : t -> if_used_at:t -> bool

include Identifiable.S with type t := t

module Standard_int : sig
  (** These kinds are known as the "standard integer kinds".  They correspond
      to the usual representations of tagged immediates, 32-bit, 64-bit and
      native integers as expected by the operations in [Flambda_primitive].
      (Boxing of the latter three kinds of integers is handled via explicit
      boxing and unboxing primitives; as such, the boxed versions are not
      known as "standard". *)
  type t =
    | Tagged_immediate
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Identifiable.S with type t := t
end

module Standard_int_or_float : sig
  (** The same as [Standard_int], but also permitting naked floats. *)
  type t =
    | Tagged_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Identifiable.S with type t := t
end

module Boxable_number : sig
  (** These kinds are those of the numbers for which a tailored boxed
      representation exists. *)

  type t =
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  (** The kind of the _unboxed_ representation of the given [t]. *)
  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Identifiable.S with type t := t
end

module Naked_number : sig
  type 'values t =
    | Naked_immediate : Immediate.Set.t t
    | Naked_float : Numbers.Float_by_bit_pattern.Set.t t
    | Naked_int32 : Numbers.Int32.Set.t t
    | Naked_int64 : Numbers.Int64.Set.t t
    | Naked_nativeint : Targetint.Set.t t

  val print : Format.formatter -> _ t -> unit
end
