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

val create : resolver:(Export_id.t -> Type_grammar.t option) -> t

val create_using_resolver_from : t -> t

val create_using_resolver_and_symbol_bindings_from : t -> t

val resolver : t -> (Export_id.t -> Type_grammar.t option)

val is_empty : t -> bool

val current_scope : t -> Scope.t

val increment_scope : t -> t

val increment_scope_to : t -> Scope.t -> t

val domain : t -> Name_occurrences.t

val var_domain : t -> Variable.Set.t

val add_definition : t -> Name_in_binding_pos.t -> Flambda_kind.t -> t

val add_equation : t -> Name.t -> Type_grammar.t -> t

val find : t -> Name.t -> Type_grammar.t

val mem : t -> Name.t -> bool

val add_cse
   : t
  -> Flambda_primitive.Eligible_for_cse.t
  -> bound_to:Simple.t
  -> t

val find_cse
   : t
  -> Flambda_primitive.Eligible_for_cse.t
  -> Simple.t option

val find_cse_rev
   : t
  -> bound_to:Simple.t
  -> Flambda_primitive.Eligible_for_cse.t option

val add_env_extension_from_level
   : t
  -> Typing_env_level.t
  -> t

(* CR mshinwell: clarify that this does not meet *)
val add_env_extension
   : t
  -> Typing_env_extension.t
  -> t

val get_canonical_simple
   : t
  -> ?min_occurrence_kind:Name_occurrence_kind.t
  -> Simple.t
  -> Simple.t option Or_bottom.t

val get_canonical_simple_with_kind
   : t
  -> ?min_occurrence_kind:Name_occurrence_kind.t
  -> Simple.t
  -> Simple.t option Or_bottom.t * Flambda_kind.t

val get_alias_ty_then_canonical_simple
   : t
  -> ?min_occurrence_kind:Name_occurrence_kind.t
  -> _ Type_grammar.ty
  -> Simple.t option Or_bottom.t

val get_alias_then_canonical_simple
   : t
  -> ?min_occurrence_kind:Name_occurrence_kind.t
  -> Type_grammar.t
  -> Simple.t option Or_bottom.t

val expand_head_ty
   : t
  -> force_to_kind:(Type_grammar.t -> 'a Type_grammar.ty)
  -> apply_rec_info:('a -> Rec_info.t -> 'a Or_bottom.t)
  -> print_ty:(Format.formatter -> 'a Type_grammar.ty -> unit)
  -> 'a Type_grammar.ty
  -> 'a Type_grammar.unknown_or_join

val expand_head : t -> Type_grammar.t -> Type_grammar.resolved

val aliases_of_simple
   : t
  -> min_occurrence_kind:Name_occurrence_kind.t
  -> Simple.t
  -> Simple.Set.t

val aliases_of_simple_allowable_in_types : t -> Simple.t -> Simple.Set.t

val cut
   : t
  -> unknown_if_defined_at_or_later_than:Scope.t
  -> Typing_env_extension.t * Variable.Set.t

val defined_earlier : t -> Simple.t -> than:Simple.t -> bool
