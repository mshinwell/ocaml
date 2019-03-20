(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compilation of pattern-matching *)

open Typedtree
open Lambda


(* Entry points to match compiler *)
val for_function:
        Location.t -> int ref option -> lambda ->
        (pattern * block) list ->
        partial -> lambda
val for_trywith:
        Location.t -> lambda -> (pattern * block) list -> lambda
val for_let:
        Location.t -> lambda -> pattern -> lambda -> Location.t -> lambda
val for_multiple_match:
        Location.t -> lambda list -> (pattern * block) list
        -> partial -> lambda

val for_tupled_function:
        Location.t -> Ident.t list ->
        (pattern list * block) list ->
        partial -> lambda

exception Cannot_flatten

val flatten_pattern: int -> pattern -> pattern list

(** Expand [Lstringswitch] to a string test tree. *)
val expand_stringswitch
   : lambda
  -> (string * block) list
  -> default:block option
  -> lambda

val inline_lazy_force : lambda -> Location.t -> lambda
