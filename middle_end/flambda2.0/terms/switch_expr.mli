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

(** Representation of conditional control flow: the [Switch] expression.

    Scrutinees of [Switch]es are [Discriminant]s of kind [Fabricated]---not
    regular integers or similar. There are no default cases. Switches always
    have at least two cases.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Sort : sig
  type t =
    | Int
    | Tag of { tags_to_sizes : Targetint.OCaml.t Tag.Scannable.Map.t; }
    | Is_int

  val print : Format.formatter -> t -> unit
end

type t

include Expr_std.S with type t := t

val create
   : Sort.t
  -> scrutinee:Simple.t
  -> arms:Continuation.t Discriminant.Map.t
  -> t

(** The scrutinee of the switch. *)
val scrutinee : t -> Simple.t

(** Call the given function [f] on each (discriminant, destination) pair
    in the switch. *)
val iter : t -> f:(Discriminant.t -> Continuation.t -> unit) -> unit

(** Where the switch will jump to for each possible value of the
    discriminant. *)
val arms : t -> Continuation.t Discriminant.Map.t

(** How many cases the switch has.  (Note that this is not the number of
    destinations reached by the switch, which may be a smaller number.) *)
val num_arms : t -> int

val sort : t -> Sort.t
