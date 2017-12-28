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

(** Immediate constants that can be held in registers. *)

type 'a or_wrong = private
  | Ok of 'a
  | Wrong

type t = private {
  value : Targetint.OCaml.t;
  print_as_char : bool;
}

type immediate = t

(** The comparison function for type [t] ignores [print_as_char]. *)
include Identifiable.S with type t := t

val join : t -> t -> t or_wrong

val join_set : Set.t -> Set.t -> Set.t

val bool_true : t
val bool_false : t

val bool : bool -> t

val int : Targetint.OCaml.t -> t
val char : char -> t

val map : t -> f:(Targetint.OCaml.t -> Targetint.OCaml.t) -> t

val to_targetint : t -> Targetint.OCaml.t

val set_to_targetint_set : Set.t -> Targetint.OCaml.Set.t

val set_of_targetint_set : Targetint.OCaml.Set.t -> Set.t

val neg : t -> t

val get_least_significant_16_bits_then_byte_swap : t -> t

(** The set consisting of the representations of constant [true] and [false]. *)
val all_bools : Set.t

module Or_unknown : sig
  type nonrec t = private
    | Ok of t
    | Unknown

  val ok : immediate -> t
  val unknown : unit -> t

  include Identifiable.S with type t := t
end
