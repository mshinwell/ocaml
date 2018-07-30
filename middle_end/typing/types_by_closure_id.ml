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

  module RP = Relational_product.Make (Closure_id) (Logical_variable) (T)

  let create closure_ids_to_tys =
    let closure_ids_to_logical_variables =
      Closure_id.Map.map (fun _ty -> Logical_variable.create ())
        closure_ids
    in
    let env_extension =
      Closure_id.Map.fold (fun closure_id ty env_extension ->
          let logical_var =
            Closure_id.Map.find closure_id closure_ids_to_logical_variables
          in
          TEE.add_equation env_extension (Name.logical_var logical_var) ty)
        closure_ids
        TEE.empty
    in
    RP.create [
      closure_ids_to_logical_variables, env_extension;
    ]

  let invariant = RP.invariant
  let meet = RP.meet
  let join = RP.join
  let apply_name_permutation t = RP.apply_name_permutation
  let freshen t = RP.freshen
end
