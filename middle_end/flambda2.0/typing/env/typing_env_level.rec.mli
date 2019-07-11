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

val print_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> t
  -> unit

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val apply_name_permutation : t -> Name_permutation.t -> t

val empty : unit -> t

val is_empty : t -> bool

val defined_vars : t -> Flambda_kind.t Variable.Map.t

val equations : t -> Flambda_types.t Name.Map.t

val one_equation : Name.t -> Flambda_types.t -> t

val add_definition : t -> Variable.t -> Flambda_kind.t -> t

val add_or_replace_equation : t -> Name.t -> Flambda_types.t -> t

val add_cse
   : t
  -> Flambda_primitive.With_fixed_value.t
  -> bound_to:Simple.t
  -> t

val meet : Meet_env.t -> t -> t -> t

val join : Typing_env.t -> t -> t -> t

val erase_aliases : Typing_env.t -> allowed:Variable.Set.t -> t -> t

val remove_definitions_and_equations : t -> allowed:Variable.Set.t -> t

val mem : t -> Name.t -> bool

val cse : t -> Simple.t Flambda_primitive.With_fixed_value.Map.t
