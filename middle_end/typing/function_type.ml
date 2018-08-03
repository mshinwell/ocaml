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

  module RP = Relational_product.Make (Int_index) (Logical_variable_component)

  type t = RP.t

  let create ~parameters ~results =
    let assign_logical_variables tys =
      Targetint.OCaml.Map.of_list (
        List.mapi (fun index ty ->
            let index = Targetint.OCaml.of_int index in
            let kind = Flambda_type0_core.kind ty in
            let logical_var = Logical_variable.create kind in
            index, logical_var)
          tys)
    in
    let create_equations tys indexes_to_vars =
      let env_extension, _index =
        List.fold_left (fun (env_extension, index) ty ->
            let logical_var = Targetint.OCaml.Map.find index indexes_to_vars in
            let env_extension =
              Typing_env_extension.add_equation env_extension
                (Name.logical_var logical_var) ty
            in
            let next_index = Targetint.OCaml.add index Targetint.OCaml.one in
            env_extension, next_index)
          (Typing_env_extension.empty, Targetint.OCaml.zero)
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
  let apply_name_permutation = RP.apply_name_permutation
  let freshen = RP.freshen

  module Flambda_type0_core = W.Flambda_type0_core
  module Join_env = W.Join_env
  module Meet_env = W.Meet_env
  module Relational_product = W.Relational_product
  module Typing_env = W.Typing_env
  module Typing_env_extension = W.Typing_env_extension
end
