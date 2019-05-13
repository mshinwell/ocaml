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

include Contains_names.S

val print_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> t
  -> unit

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val empty : unit -> t

val is_empty : t -> bool

(*
val restrict_to_names : t -> Name_occurrences.t -> t
*)

val find_opt : t -> Name.t -> Flambda_types.t option

val add_definition : t -> Name.t -> Flambda_kind.t -> t

val add_equation : t -> Name.t -> Flambda_types.t -> t

val meet_equation
   : t
  -> Typing_env.t
  -> Name.t
  -> Flambda_types.t
  -> t

val add_or_replace_equation : t -> Name.t -> Flambda_types.t -> t
(*
val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

val defined_names_set : t -> Bindable_name.Set.t

val defined_names : t -> Flambda_kind.t Name.Map.t

val defined_names_in_order : t -> Bindable_name.t list

val equations_domain : t -> Name.Set.t

val equations_on_outer_env_domain : t -> Name.Set.t

val equations : t -> Flambda_types.t Name.Map.t

val cse : t -> Simple.t Flambda_primitive.With_fixed_value.Map.t
*)
