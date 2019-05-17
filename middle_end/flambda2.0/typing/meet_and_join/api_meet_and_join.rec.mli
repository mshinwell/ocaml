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

(** The external interface of the meet_and_join/ directory, providing meet
    and join functions on Flambda types, and some derived operations. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val meet
   : Meet_env.t
  -> Flambda_types.t
  -> Flambda_types.t
  -> Flambda_types.t * Typing_env_extension.t

val join
   : ?bound_name:Name.t
  -> Join_env.t
  -> Flambda_types.t
  -> Flambda_types.t
  -> Flambda_types.t

val meet_closures_entry
   : Meet_env.t
  -> Flambda_types.closures_entry
  -> Flambda_types.closures_entry
  -> (Flambda_types.closures_entry * Typing_env_extension.t) Or_bottom.t

val join_closures_entry
   : Join_env.t
  -> Flambda_types.closures_entry
  -> Flambda_types.closures_entry
  -> Flambda_types.closures_entry

val meet_set_of_closures_entry
   : Meet_env.t
  -> Flambda_types.set_of_closures_entry
  -> Flambda_types.set_of_closures_entry
  -> (Flambda_types.set_of_closures_entry * Typing_env_extension.t)
       Or_bottom.t

val join_set_of_closures_entry
   : Join_env.t
  -> Flambda_types.set_of_closures_entry
  -> Flambda_types.set_of_closures_entry
  -> Flambda_types.set_of_closures_entry
