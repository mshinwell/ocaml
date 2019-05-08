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

type binding_type = private
  | Normal
  | Was_existential

val invariant : t -> unit

val print : Format.formatter -> t -> unit

val create : resolver:(Export_id.t -> Flambda_types.t option) -> t

val create_using_resolver_from : t -> t

val resolver : t -> (Export_id.t -> Flambda_types.t option)

val is_empty : t -> bool

val current_level : t -> Scope_level.t

val increment_scope_level : t -> t

val increment_scope_level_to : t -> Scope_level.t -> t

val fast_equal : t -> t -> bool

val domain : t -> Name_occurrences.t

val add_definition : t -> Name.t -> Flambda_kind.t -> t

val add_equation : t -> Name.t -> Flambda_types.t -> t

val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

val find_exn : t -> Name.t -> Flambda_types.t * binding_type

val find_opt : t -> Name.t -> (Flambda_types.t * binding_type) option

val find_cse : t -> Flambda_primitive.t -> Simple.t option

val mem : t -> Name.t -> bool

val add_env_extension
   : t
  -> Typing_env_extension.t
  -> t

val add_opened_env_extension
   : t
  -> Typing_env_level.t
  -> t

val resolve_aliases
   : ?bound_name:Name.t
  -> t
  -> Flambda_types.t
  -> Flambda_types.t * (Simple.t option)

val resolve_aliases_and_squash_unresolved_names_on_ty'
   : t
  -> ?bound_name:Name.t
  -> print_ty:(Format.formatter -> 'a Flambda_types.ty -> unit)
  -> force_to_kind:(Flambda_types.t -> 'a Flambda_types.ty)
  -> 'a Flambda_types.ty
  -> 'a Flambda_types.unknown_or_join * (Simple.t option)

val aliases_of_simple : t -> Simple.t -> Name.Set.t

val cut0
   : t
  -> existential_if_defined_at_or_later_than:Scope_level.t
  -> Typing_env_level.t

val cut
   : t
  -> existential_if_defined_at_or_later_than:Scope_level.t
  -> Typing_env_extension.t

val was_existential_exn : t -> Name.t -> bool
