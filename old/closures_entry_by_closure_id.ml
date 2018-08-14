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

  (* CR mshinwell: Share with closure_elements.ml *)
  module Var_within_closure_set = struct
    type t = Var_within_closure.Set.t

    include Hashtbl.Make_with_map (struct
      include Var_within_closure.Set
      let hash = Hashtbl.hash
    end)
  end

  module RL =
    Row_like.Make (Closure_id) (Var_within_closure_set)
      (Flambda_type0_core.Closures_entry)

  module Closure_id_and_var_within_closure_set = RL.Tag_and_index

  type t = RL.t

  let create_exactly_multiple closure_id_and_vars_within_closure_map =
    RL.create_exactly_multiple closure_id_and_vars_within_closure_map

  let create_at_least_multiple vars_within_closure_map =
    RL.create_at_least_multiple vars_within_closure_map

  let print ~cache ppf t = RL.print ~cache ppf t

  let meet env t1 t2 : _ Or_bottom.t =
    match RL.meet env Fresh t1 t2 with
    | Bottom -> Bottom
    | Ok (t, _closures_entry) -> Ok (t, Typing_env_extension.empty)

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
