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

type t = Type_grammar.closures_entry

val create_bottom : unit -> t

val widen : t -> to_match:t -> t

val map_function_decl_types
   : t
  -> f:(Type_grammar.function_declaration
    -> Type_grammar.function_declaration Or_bottom.t)
  -> t Or_bottom.t

val find_function_declaration
   : t
  -> Closure_id.t
  -> Type_grammar.function_declaration Or_unknown.t

include Type_structure_intf.S
  with type t := t
  with type flambda_type := Type_grammar.t
  with type typing_env := Typing_env.t
  with type meet_env := Meet_env.t
  with type type_equality_env := Type_equality_env.t
  with type typing_env_extension := Typing_env_extension.t
