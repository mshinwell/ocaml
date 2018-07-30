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

module Make (Thing_without_names : Map.With_set) (T : Typing_world.S) = struct
  module Flambda_type = T.Flambda_type
  module Join_env = T.Join_env
  module Typing_env = T.Typing_env
  module Typing_env_extension = T.Typing_env_extension

  module Index = struct
    include Thing_without_names

    let apply_name_permutation t _ = t
    let freshen t _ = t
  end

  module TEE = struct
    include Typing_env_extension

    let add_or_meet_equations t env t' = meet env t t'
  end

  module RL = Row_like.Make (Unit) (Index.Set) (TEE) (T)

  type t = RL.t

  type open_or_closed = Open | Closed

  let create_with_equations things env_extension open_or_closed =
    let size = List.length field_tys in
    match open_or_closed with
    | Open -> RL.create_at_least things env_extension
    | Closed -> RL.create_exactly () things env_extension

  let create things open_or_closed =
    create_with_equations things TEE.empty open_or_closed

  let invariant = RL.invariant
  let meet = RL.meet
  let join = RL.join
  let apply_name_permutation t = RL.apply_name_permutation
  let freshen t = RL.freshen
end
