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
module Join_env = struct end
module Relational_product = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  module RP = Relational_product.Make (Int_index) (Logical_variable_component)
  module RL = Row_like.Make (Tag_index) (Int_index) (RP)

  type t = RL.t

  type open_or_closed = Open | Closed of Tag.t

  let create field_tys open_or_closed =
    (* CR mshinwell: This code is very similar to some in [Function_type]. *)
    let indexes_to_vars =
      Targetint.OCaml.Map.of_list (
        List.mapi (fun index _field_ty ->
            let index = Targetint.OCaml.of_int index in
            let logical_var = Logical_variable.create (Flambda_kind.value ()) in
            index, logical_var)
          field_tys)
    in
    let env_extension, _index =
      List.fold_left (fun (env_extension, index) field_ty ->
          let logical_var = Targetint.OCaml.Map.find index indexes_to_vars in
          let env_extension =
            Typing_env_extension.add_equation env_extension
              (Name.logical_var logical_var) field_ty
          in
          let next_index = Targetint.OCaml.add index Targetint.OCaml.one in
          env_extension, next_index)
        (Typing_env_extension.empty, Targetint.OCaml.zero)
        field_tys
    in
    let product =
      RP.create [
        indexes_to_vars, env_extension;
      ]
    in
    let size = Targetint.OCaml.of_int (List.length field_tys) in
    match open_or_closed with
    | Open -> RL.create_at_least size product
    | Closed tag -> RL.create_exactly tag size product

  let invariant _t = () (* CR mshinwelL: RL.invariant *)
  let meet = RL.meet
  let join = RL.join
  let free_names = RL.free_names
  let bound_names = RL.bound_names
  let apply_name_permutation t = RL.apply_name_permutation
  let freshen t = RL.freshen
end
