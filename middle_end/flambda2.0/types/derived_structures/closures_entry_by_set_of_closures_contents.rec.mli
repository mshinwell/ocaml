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

val create_exactly_multiple
   : Closures_entry.t Set_of_closures_contents.With_closure_id.Map.t
  -> t

val create_at_least_multiple
   : Closures_entry.t Set_of_closures_contents.With_closure_id_or_unknown.Map.t
  -> t

val map_function_decl_types
   : t
  -> f:(Function_declaration_type.t -> Function_declaration_type.t Or_bottom.t)
  -> t Or_bottom.t

val get_singleton
   : t
  -> ((Closure_id.t * Set_of_closures_contents.t) * Closures_entry.t) option

include Type_structure_intf.S
  with type t := t
  with type flambda_type := Type_grammar.t
  with type meet_env := Meet_env.t
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
