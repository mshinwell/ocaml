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

(** Measurement of the size of Flambda terms. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

include Identifiable.S with type t := t

val zero : t
val one : t

val of_int : int -> t

val (+) : t -> t -> t
val (-) : t -> t -> t

val (<) : t -> t -> bool
val (>) : t -> t -> bool

val min : t -> t -> t

val max_value : t
