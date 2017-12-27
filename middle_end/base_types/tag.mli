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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Tags on runtime boxed values. *)

include Identifiable.S

type tag = t

val create_exn : int -> t

val to_int : t -> int
val to_targetint : t -> Targetint.t

val zero : t
val string_tag : t
val closure_tag : t
val double_tag : t
val double_array_tag : t
val custom_tag : t

val is_structured_block : t -> bool

val all_as_targetints : Targetint.Set.t

(* CR mshinwell: This name should be changed---all "value"s are scannable.
   "Structured"? *)
module Scannable : sig
  (** Tags that are strictly less than [No_scan_tag], corresponding to
      blocks with fields that can be scanned by the GC. *)
  type t

  (** Raises not only if the supplied integer is less than 0 but also if
      it is greater than or equal to [No_scan_tag]. *)
  val create_exn : int -> t

  val of_tag : tag -> t option

  val to_int : t -> int
  val to_targetint : t -> Targetint.t
  val to_tag : t -> tag

  val zero : t
  val object_tag : t

  include Identifiable.S with type t := t
end

val to_scannable_set : Set.t -> Scannable.Set.t

module Non_scannable : sig
  (** Tags that are at or above [No_scan_tag], corresponding to blocks whose
      fields cannot be scanned by the GC. *)
  type t

  (** Raises if the supplied integer is less than [No_scan_tag]. *)
  val create_exn : int -> t

  val of_tag : tag -> t option

  val to_int : t -> int
  val to_tag : t -> tag

  include Identifiable.S with type t := t
end
