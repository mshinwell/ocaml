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
   : Flambda_types.closures_entry Set_of_closures_contents.Map.t
  -> t

val create_at_least_multiple
   : Flambda_types.closures_entry Set_of_closures_contents.Map.t
  -> t

val get_singleton
   : t
  -> ((Closure_id.t * Set_of_closures_contents.t)
       * Flambda_types.closures_entry) option

include Type_structure_intf.S
  with type t := t
  with type flambda_type := Flambda_types.t
  with type type_equality_env := Type_equality_env.t
  with type meet_env := Meet_env.t
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t