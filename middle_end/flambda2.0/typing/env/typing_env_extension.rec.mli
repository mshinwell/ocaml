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

type t

include Contains_names.S with type t := t

val invariant : t -> unit

val print : Format.formatter -> t -> unit

val print_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> t
  -> unit

val equal
   : Type_equality_env.t
  -> Type_equality_result.t
  -> t
  -> t
  -> Type_equality_result.t

val fast_equal : t -> t -> bool

val empty : t

val is_empty : t -> bool

val create : Typing_env_level.t -> t

val add_definition : t -> Name.t -> Flambda_kind.t -> t

val add_equation : t -> Name.t -> Flambda_types.t -> t

val meet_equation : t -> Typing_env.t -> Name.t -> Flambda_types.t -> t

val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

(*
val restrict_to_definitions : t -> t

val restrict_names_to_those_occurring_in_types
   : t
  -> Typing_env.t
  -> Typing_env.t
  -> Flambda_types.t list
  -> t

val diff : t -> Typing_env.t -> t
*)

val pattern_match
   : t
  -> f:(Typing_env_level.t -> 'a)
  -> 'a
