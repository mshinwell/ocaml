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

(** Handling of permutations of various kinds of names.  Note that the use
    of permutations means that, at this level, whether a particular name is
    free or bound is irrelevant when applying a renaming to it. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create : unit -> t

val print : Format.formatter -> t -> unit

val is_empty : t -> bool

val compose : t -> t -> t

val add_continuation : t -> Continuation.t -> Continuation.t -> t

val apply_continuation : t -> Continuation.t -> Continuation.t

val add_kinded_parameter
   : t
  -> Kinded_parameter.t
  -> Kinded_parameter.t
  -> t

val apply_kinded_parameter
   : t
  -> Kinded_parameter.t
  -> Kinded_parameter.t

val add_logical_variable
   : t
  -> Logical_variable.t
  -> Logical_variable.t
  -> t

val apply_logical_variable
   : t
  -> Logical_variable.t
  -> Logical_variable.t

val add_name
   : t
  -> Name.t
  -> Name.t
  -> t

val apply_name : t -> Name.t -> Name.t

val apply_simple : t -> Simple.t -> Simple.t

val apply_simples : t -> Simple.t list -> Simple.t list

val add_symbol
   : t
  -> Symbol.t
  -> Symbol.t
  -> t

val apply_symbol : t -> Symbol.t -> Symbol.t

val add_variable
   : t
  -> Variable.t
  -> Variable.t
  -> t

val apply_variable : t -> Variable.t -> Variable.t
