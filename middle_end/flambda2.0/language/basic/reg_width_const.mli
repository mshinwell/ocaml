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

(** Unboxed constants that fit in registers. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Naked_immediate of Immediate.t
  | Tagged_immediate of Immediate.t
  | Naked_float of Numbers.Float_by_bit_pattern.t
  | Naked_int32 of Int32.t
  | Naked_int64 of Int64.t
  | Naked_nativeint of Targetint.t
  (* CR mshinwell: It's a bit strange that [Initial_rec_info] is here since
     it never exists at runtime. *)
  | Initial_rec_info

include Identifiable.S with type t := t

val kind : t -> Flambda_kind.t

(** The constant representating the given number of type "int". *)
val const_int : Targetint.OCaml.t -> t

(** The constant representating boolean true. *)
val const_true : t

(** The constant representating boolean false. *)
val const_false : t

(** The constant representating the number zero of type "int". *)
val const_zero : t

(** The constant representing the unit value. *)
val const_unit : t
