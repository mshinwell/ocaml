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

module Make (T : Typing_world.S) = struct
  module Flambda_type0_core = T.Flambda_type0_core
  module Join_env = T.Join_env
  module Type_equality = T.Type_equality
  module Typing_env = T.Typing_env
  module Typing_env_extension = T.Typing_env_extension

  let meet env t1 t2 =
    Meet.Meet_and_join.meet_or_join (Join_env.create env) t1 t2

  let join env t1 t2 =
    let join_ty, _env_extension =
      Join.Meet_and_join.meet_or_join env t1 t2
    in
    join_ty

  let as_or_more_precise env t1 ~than:t2 =
    if Type_equality.fast_equal t1 t2 then true
    else
      let meet_t, _env_extension = meet env t1 t2 in
      Type_equality.equal meet_t t1

  let strictly_more_precise env t1 ~than:t2 =
    if Type_equality.fast_equal t1 t2 then false
    else
      let meet_t, _env_extension = meet env t1 t2 in
      Type_equality.equal meet_t t1
        && not (Type_equality.equal meet_t t2)
(*
  val meet_closures_entry
     : Meet_env.t
    -> Flambda_type0_core.closures_entry
    -> Flambda_type0_core.closures_entry
    -> (Flambda_type0_core.closures_entry * Typing_env_extension.t) Or_bottom.t

  val join_closures_entry
     : Join_env.t
    -> Flambda_type0_core.closures_entry
    -> Flambda_type0_core.closures_entry
    -> Flambda_type0_core.closures_entry

  val meet_set_of_closures_entry
     : Meet_env.t
    -> Flambda_type0_core.set_of_closures_entry
    -> Flambda_type0_core.set_of_closures_entry
    -> (Flambda_type0_core.set_of_closures_entry * Typing_env_extension.t)
         Or_bottom.t

  val join_set_of_closures_entry
     : Join_env.t
    -> Flambda_type0_core.set_of_closures_entry
    -> Flambda_type0_core.set_of_closures_entry
    -> Flambda_type0_core.set_of_closures_entry
*)
end
