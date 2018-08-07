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

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Flambda_type0_core = struct end
module Join_env = struct end
module Meet_env = struct end
module Row_like = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  module RL =
    Row_like.Make (Unit) (Closure_id_set)
      (Flambda_type0_core.Set_of_closures_entry)

  type t = RL.t

  type open_or_closed = Open | Closed

  let create closure_ids_map open_or_closed : t =
    match open_or_closed with
    | Open -> RL.create_at_least_multiple closure_ids_map
    | Closed ->
      let closure_ids_map =
        Closure_id_set.Map.fold (fun closure_ids set_of_closures_entry result ->
            RL.Tag_and_index.Map.add ((), closure_ids) set_of_closures_entry
              result)
          closure_ids_map
          RL.Tag_and_index.Map.empty
      in
      RL.create_exactly_multiple closure_ids_map

  let print = RL.print
  let invariant _t = ()  (* CR mshinwell: RL.invariant *)

  let meet env t1 t2 : _ Or_bottom.t =
    match RL.meet env Fresh t1 t2 with
    | Bottom -> Bottom
    | Ok (t, _set_of_closures_entry) -> Ok (t, Typing_env_extension.empty)

  let join env t1 t2 = RL.join env Fresh t1 t2

  let equal = RL.equal
  let free_names = RL.free_names
  let bound_names = RL.bound_names
  let apply_name_permutation = RL.apply_name_permutation
  let freshen = RL.freshen

  module Flambda_type0_core = W.Flambda_type0_core
  module Join_env = W.Join_env
  module Meet_env = W.Meet_env
  module Typing_env = W.Typing_env
  module Typing_env_extension = W.Typing_env_extension
end
