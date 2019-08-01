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

val create : Type_grammar.t Closure_id.Map.t -> t

val bottom : t

val map_closure_types
   : t
  -> f:(Type_grammar.t -> Type_grammar.t Or_bottom.t)
  -> t Or_bottom.t

val widen : t -> to_match:t -> t

include Type_structure_intf.S
  with type t := t
  with type typing_env := Typing_env.t
  with type meet_env := Meet_env.t
  with type type_equality_env := Type_equality_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type flambda_type := Type_grammar.t
