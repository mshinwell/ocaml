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

val empty : t

val is_empty : t -> bool

val defined_vars : t -> Flambda_kind.t Variable.Map.t

val equations : t -> Flambda_types.t Name.Map.t

val one_equation : Name.t -> Flambda_types.t -> t

val add_definition : t -> Variable.t -> Flambda_kind.t -> t

val add_or_replace_equation : t -> Name.t -> Flambda_types.t -> t

val meet_equation : t -> Meet_env.t -> Name.t -> Flambda_types.t -> t

val meet : Meet_env.t -> t -> t -> t

val join : Typing_env.t -> t -> t -> t

val erase_aliases : t -> allowed:Variable.Set.t -> t

val remove_definitions_and_equations_thereon : t -> t

val mem : t -> Name.t -> bool
