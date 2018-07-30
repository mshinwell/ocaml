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

  module RP =
    Relational_product.Make (Var_within_closure) (Logical_variable) (T)
  module RL = Row_like.Make (Unit) (Var_within_closure.Set) (RP) (T)

  module TEE = Typing_env_extension

  type t = RL.t

  type open_or_closed = Open | Closed

  let create closure_elements_to_tys open_or_closed =
    let closure_elements_to_logical_variables =
      Var_within_closure.Map.map (fun _ty -> Logical_variable.create ())
        closure_elements
    in
    let env_extension =
      Var_within_closure.Map.fold (fun var ty env_extension ->
          let logical_var =
            Var_within_closure.Map.find var
              closure_elements_to_logical_variables
          in
          TEE.add_equation env_extension (Name.logical_var logical_var) ty)
        closure_elements
        TEE.empty
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

  let invariant = RL.invariant
  let meet = RL.meet
  let join = RL.join
  let apply_name_permutation t = RL.apply_name_permutation
  let freshen t = RL.freshen
end
