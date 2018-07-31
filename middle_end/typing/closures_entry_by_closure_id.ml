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
  module Flambda_type = T.Flambda_type
  module Join_env = T.Join_env
  module Relational_product = T.Relational_product
  module Typing_env = T.Typing_env
  module Typing_env_extension = T.Typing_env_extension

  module RL =
    Row_like.Make (Closure_id) (Var_within_closure.Set)
      (Flambda_type.Closures_entry) (T)

  module Closure_id_and_var_within_closure = RL.Tag_and_index

  type t = RL.t

  type open_or_closed = Open | Closed

  let create_closed closure_id_and_vars_within_closure_map =
    RL.create_exactly_multiple closure_id_and_vars_within_closure_map

  let create_open vars_within_closure_map =
    RL.create_at_least_multiple vars_within_closure_map

  let invariant = RL.invariant
  let meet = RL.meet
  let join = RL.join
  let apply_name_permutation t = RL.apply_name_permutation
  let freshen t = RL.freshen
end
