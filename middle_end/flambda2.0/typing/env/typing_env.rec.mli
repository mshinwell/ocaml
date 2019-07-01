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

val invariant : t -> unit

val print : Format.formatter -> t -> unit

val create : resolver:(Export_id.t -> Flambda_types.t option) -> t

val create_using_resolver_from : t -> t

val create_using_resolver_and_symbol_bindings_from : t -> t

val resolver : t -> (Export_id.t -> Flambda_types.t option)

val is_empty : t -> bool

val current_scope : t -> Scope.t

val increment_scope : t -> t

val increment_scope_to : t -> Scope.t -> t

val domain : t -> Name_occurrences.t

val var_domain : t -> Variable.Set.t

val add_definition : t -> Name_in_binding_pos.t -> Flambda_kind.t -> t

val add_equation : t -> Name.t -> Flambda_types.t -> t

val find : t -> Name.t -> Flambda_types.t

val mem : t -> Name.t -> bool

val add_env_extension
   : t
  -> Typing_env_extension.t
  -> t

val get_canonical_simple
   : t
  -> Simple.t
  -> min_occurrence_kind:Name_occurrence_kind.t
  -> Simple.t Or_bottom.t * Flambda_type.t

val resolve_ty
   : t
  -> force_to_kind:(Flambda_types.t -> 'a Flambda_types.ty)
  -> print_ty:(Format.formatter -> 'a Flambda_types.ty -> unit)
  -> 'a Flambda_types.ty
  -> 'a Flambda_types.unknown_or_join * (Simple.t option)

val resolve_type
   : t
  -> Flambda_types.t
  -> Flambda_types.resolved

val aliases_of_simple_allowable_in_types : t -> Simple.t -> Simple.Set.t

val cut
   : t
  -> unknown_if_defined_at_or_later_than:Scope.t
  -> Typing_env_extension.t * Variable.Set.t

val defined_earlier : t -> Simple.t -> than:Simple.t -> bool
