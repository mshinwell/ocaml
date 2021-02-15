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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module DE = Downwards_env

type t

val empty : t

val is_empty : t -> bool

val print : Format.formatter -> t -> unit

val singleton : Lifted_constant.t -> t

(* Use if the order of constants doesn't matter. *)
val add : t -> Lifted_constant.t -> t

val add_innermost : t -> Lifted_constant.t -> t

val add_outermost : t -> Lifted_constant.t -> t

val singleton_sorted_array_of_constants
   : innermost_first:Lifted_constant.t array
   -> t

(* Use if the order of constants doesn't matter. *)
val union : t -> t -> t

val union_ordered : innermost:t -> outermost:t -> t

(* Use if the order of constants doesn't matter. *)
val fold
   : t
  -> init:'a
  -> f:('a -> Lifted_constant.t -> 'a)
  -> 'a

val fold_outermost_first
   : t
  -> init:'a
  -> f:('a -> Lifted_constant.t -> 'a)
  -> 'a

val fold_innermost_first
   : t
  -> init:'a
  -> f:('a -> Lifted_constant.t -> 'a)
  -> 'a

val all_defined_symbols : t -> Symbol.Set.t

val add_to_denv
   : ?maybe_already_defined:unit
  -> DE.t
  -> t
  -> DE.t

val add_singleton_to_denv : DE.t -> Lifted_constant.t -> DE.t

val add_list_to_denv : DE.t -> Lifted_constant.t list -> DE.t
