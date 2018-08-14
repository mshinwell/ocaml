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
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  module Make (Thing_without_names : Hashtbl.With_map) = struct
    module TEE = struct
      include Typing_env_extension

      let add_or_meet_equations t env t' =
        meet env t t'

      let meet env _ t1 t2 : _ Or_bottom.t =
        let t = meet env t1 t2 in
        if is_empty t then Bottom
        else Ok (t, empty)

      let join env _ t1 t2 = join env t1 t2

      let bottom () = empty
    end

    module RL = Row_like.Make (Thing_without_names) (Unit) (TEE)

    type t = RL.t

    let create_with_equations things_with_env_extensions =
      let things_with_env_extensions =
        Thing_without_names.Map.fold (fun thing extension result ->
            RL.Tag_and_index.Map.add (thing, ()) extension result)
          things_with_env_extensions
          RL.Tag_and_index.Map.empty
      in
      RL.create_exactly_multiple things_with_env_extensions

    let create things =
      let things_with_env_extensions =
        Thing_without_names.Map.of_set (fun _thing -> TEE.empty) things
      in
      create_with_equations things_with_env_extensions

    let create_bottom () =
      create Thing_without_names.Set.empty

    let create_unknown () =
      RL.create_at_least_multiple Unit.Map.empty

    let print = RL.print
    let equal = RL.equal

    let meet env t1 t2 = RL.meet env Fresh t1 t2
    let join env t1 t2 = RL.join env Fresh t1 t2

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

  module Flambda_type0_core = W.Flambda_type0_core
  module Join_env = W.Join_env
  module Meet_env = W.Meet_env
  module Typing_env = W.Typing_env
  module Typing_env_extension = W.Typing_env_extension
end
