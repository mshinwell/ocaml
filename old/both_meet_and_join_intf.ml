(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module T : Typing_world_abstract.S
  module Functor_T : Typing_world_abstract.Functor_S

  val meet
     : T.Meet_env.t
    -> T.Flambda_types.t
    -> T.Flambda_types.t
    -> T.Flambda_types.t * T.Typing_env_extension.t

  val join
     : T.Join_env.t
    -> T.Flambda_types.t
    -> T.Flambda_types.t
    -> T.Flambda_types.t

  val meet_closures_entry
     : T.Meet_env.t
    -> T.Flambda_types.closures_entry
    -> T.Flambda_types.closures_entry
    -> (T.Flambda_types.closures_entry * T.Typing_env_extension.t) Or_bottom.t

  val join_closures_entry
     : T.Join_env.t
    -> T.Flambda_types.closures_entry
    -> T.Flambda_types.closures_entry
    -> T.Flambda_types.closures_entry

  val meet_set_of_closures_entry
     : T.Meet_env.t
    -> T.Flambda_types.set_of_closures_entry
    -> T.Flambda_types.set_of_closures_entry
    -> (T.Flambda_types.set_of_closures_entry * T.Typing_env_extension.t)
         Or_bottom.t

  val join_set_of_closures_entry
     : T.Join_env.t
    -> T.Flambda_types.set_of_closures_entry
    -> T.Flambda_types.set_of_closures_entry
    -> T.Flambda_types.set_of_closures_entry

  val as_or_more_precise
     : T.Typing_env.t
    -> T.Flambda_types.t
    -> than:T.Flambda_types.t
    -> bool

  val strictly_more_precise
     : T.Typing_env.t
    -> T.Flambda_types.t
    -> than:T.Flambda_types.t
    -> bool
end
