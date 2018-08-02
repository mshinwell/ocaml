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

  module RP = Relational_product.Make (Targetint.OCaml) (Logical_variable) (T)
  module TEE = Typing_env_extension

  type t = RP.t

  let create ~parameters ~results =
    let assign_logical_variables tys =
      Targetint.OCaml.Map.of_list (
        List.mapi (fun index _ty ->
            let index = Targetint.OCaml.of_int index in
            let logical_var = Logical_variable.create () in
            index, logical_var)
          tys)
    in
    let create_equations tys indexes_to_vars =
      let env_extension, _index =
        List.fold_left (fun (env_extension, index) ty ->
            let logical_var = Targetint.OCaml.Map.find index indexes_to_vars in
            let env_extension =
              TEE.add_equation env_extension (Name.logical_var logical_var) ty
            in
            let next_index = Targetint.OCaml.add index Targetint.OCaml.one in
            env_extension, next_index)
          (TEE.empty, Targetint.OCaml.zero)
          tys
      in
      env_extension
    in
    let param_vars = assign_logical_variables parameters in
    let param_env_extension = create_equations parameters param_vars in
    let result_vars = assign_logical_variables results in
    let result_env_extension = create_equations results result_vars in
    RP.create [
      param_vars, param_env_extension;
      result_vars, result_env_extension;
    ]

  let invariant = RP.invariant
  let equal = RP.equal
  let meet = RP.meet
  let join = RP.join
  let bound_names = RP.bound_names
  let free_names = RP.free_names
  let introduce = RP.introduce
  let apply_name_permutation t = RP.apply_name_permutation
  let freshen t = RP.freshen
end
