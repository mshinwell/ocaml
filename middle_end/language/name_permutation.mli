(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Handling of permutations upon all kinds of bindable names. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create : unit -> t

val print : Format.formatter -> t -> unit

val is_empty : t -> bool

val compose : t -> t -> t

val add_continuation : t -> Continuation.t -> Continuation.t -> t

val apply_continuation : t -> Continuation.t -> Continuation.t

val add_name : t -> Name.t -> Name.t -> t

val apply_name : t -> Name.t -> Name.t

val apply_name_set : t -> Name.Set.t -> Name.Set.t
