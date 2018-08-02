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
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  module Flambda_type0_core = W.Flambda_type0_core
  module Join_env = W.Join_env
  module Relational_product = W.Relational_product
  module Typing_env = W.Typing_env
  module Typing_env_extension = W.Typing_env_extension

  module Var_within_closure = struct
    include Var_within_closure

    let free_names _t = Name_occurrences.create ()
    let bound_names _t = Name_occurrences.create ()

    let apply_name_permutation t _perm = t
    let freshen t _freshening = t
  end

  module RP =
    Relational_product.Make (Var_within_closure)
      (Logical_variable_component)

  module Var_within_closure_set = struct
    type t = Var_within_closure.Set.t

    (* CR mshinwell: Throughout, try to go back to Map.With_set rather than
       Hashtbl.With_map *)
    include Hashtbl.Make_with_map (struct
      include Var_within_closure.Set
      let hash = Hashtbl.hash
    end)

    let free_names _t = Name_occurrences.create ()
    let bound_names _t = Name_occurrences.create ()

    let apply_name_permutation t _perm = t
    let freshen t _freshening = t
  end

  module RL = Row_like.Make (Unit) (Var_within_closure_set) (RP)

  type t = RL.t

  type open_or_closed = Open | Closed

  let create closure_elements_to_tys open_or_closed =
    let closure_elements_to_logical_variables =
      Var_within_closure.Map.map (fun _ty ->
          Logical_variable.create (Flambda_kind.value ()))
        closure_elements_to_tys
    in
    let env_extension =
      Var_within_closure.Map.fold (fun var ty env_extension ->
          let logical_var =
            Var_within_closure.Map.find var
              closure_elements_to_logical_variables
          in
          Typing_env_extension.add_equation env_extension
            (Name.logical_var logical_var) ty)
        closure_elements_to_tys
        Typing_env_extension.empty
    in
    let product =
      RP.create [
        closure_elements_to_logical_variables, env_extension;
      ]
    in
    let closure_elements =
      Var_within_closure.Map.keys closure_elements_to_tys
    in
    match open_or_closed with
    | Open -> RL.create_at_least closure_elements product
    | Closed -> RL.create_exactly () closure_elements product

  let print = RL.print

  let meet env perm1 perm2 t1 t2 =
    (* CR mshinwell: think about env_extension *)
    let t, _env_extension =
      RL.meet env perm1 perm2 Fresh t1 t2
    in
    t

  let join env perm1 perm2 t1 t2 =
    RL.join env perm1 perm2 Fresh t1 t2

  let free_names = RL.free_names
  let bound_names = RL.bound_names
  let apply_name_permutation = RL.apply_name_permutation
  let freshen = RL.freshen
end
