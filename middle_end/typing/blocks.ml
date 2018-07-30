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
  module RL = Row_like.Make (Tag) (Targetint.OCaml) (RP) (T)

  module TEE = Typing_env_extension

  type t = RL.t

  type open_or_closed = Open | Closed of Tag.t

  let create field_tys open_or_closed =
    (* CR mshinwell: This code is very similar to some in [Function_type]. *)
    let indexes_to_vars =
      Targetint.OCaml.Map.of_list (
        List.mapi (fun index _field_ty ->
            let index = Targetint.OCaml.of_int index in
            let logical_var = Logical_variable.create () in
            index, logical_var)
          field_tys
    in
    let env_extension, _index =
      List.fold_left (fun (env_extension, index) field_ty ->
          let logical_var = Targetint.OCaml.Map.find index indexes_to_vars in
          let env_extension =
            TEE.add_equation env_extension (Name.logical_var logical_var) ty
          in
          let next_index = Targetint.OCaml.add index Targetint.OCaml.one in
          env_extension, next_index)
        (TEE.empty, Targetint.OCaml.zero)
        field_tys
    in
    let product =
      RP.create [
        indexes_to_vars, env_extension;
      ]
    in
    let size = List.length field_tys in
    match open_or_closed with
    | Open -> RL.create_at_least size product
    | Closed tag -> RL.create_exactly tag size product

  let invariant = RL.invariant
  let meet = RL.meet
  let join = RL.join
  let apply_name_permutation t = RL.apply_name_permutation
  let freshen t = RL.freshen
end
