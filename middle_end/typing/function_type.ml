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

  type t =
    | Unknown
    | Product of RP.t
    | Bottom

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
    Product (RP.create [
      param_vars, param_env_extension;
      result_vars, result_env_extension;
    ])

  let create_unknown () = Unknown
  let create_bottom () = Bottom

  let invariant t =
    match t with
    | Product rp -> RP.invariant rp
    | Unknown | Bottom -> ()

  let print_with_cache ~cache ppf t =
    match t with
    | Unknown -> Format.pp_print_string ppf "Unknown"
    | Product rp -> RP.print_with_cache ~cache ppf rp
    | Bottom -> Format.pp_print_string ppf "Bottom"

  let equal env t1 t2 =
    match t1, t2 with
    | Product rp1, Product rp2 -> RP.equal env rp1 rp2
    | Unknown, Unknown -> true
    | Bottom, Bottom -> true
    | (Product _ | Unknown | Bottom), _ -> false

  let meet env fresh t1 t2 : _ Or_bottom.t =
    match t1, t2 with
    | Product rp1, Product rp2 ->
      begin match RP.meet env fresh rp1 rp2 with
      | Bottom -> Bottom
      | Ok (rp, env_extension) -> Ok (Product rp, env_extension)
      end
    | Product _, Unknown -> Ok (t1, Typing_env_extension.empty)
    | Unknown, Product _ -> Ok (t2, Typing_env_extension.empty)
    | Unknown, Unknown -> Ok (Unknown, Typing_env_extension.empty)
    | Bottom, (Product _ | Bottom | Unknown)
    | (Product _ | Unknown), Bottom -> Bottom

  let meet_fresh env t1 t2 = meet env Fresh t1 t2

  let join env fresh t1 t2 =
    match t1, t2 with
    | Product rp1, Product rp2 -> Product (RP.join env fresh rp1 rp2)
    | Bottom, Product _ -> t2
    | Product _, Bottom -> t1
    | Bottom, Bottom -> Bottom
    | Unknown, (Product _ | Bottom | Unknown)
    | (Product _ | Bottom), Unknown -> Unknown

  let join_fresh env t1 t2 = join env Fresh t1 t2

  let bound_names t =
    match t with
    | Product rp -> RP.bound_names rp
    | Unknown | Bottom -> Name_occurrences.create ()

  let free_names t =
    match t with
    | Product rp -> RP.free_names rp
    | Unknown | Bottom -> Name_occurrences.create ()

  let introduce t env =
    match t with
    | Product rp -> RP.introduce rp env
    | Unknown | Bottom -> env

  let apply_name_permutation t perm =
    match t with
    | Product rp ->
      let rp' = RP.apply_name_permutation rp perm in
      if rp == rp' then t
      else Product rp'
    | Unknown | Bottom -> Unknown

  let freshen t freshening =
    apply_name_permutation t (Freshening.name_permutation freshening)

  module Flambda_type0_core = W.Flambda_type0_core
  module Join_env = W.Join_env
  module Meet_env = W.Meet_env
  module Relational_product = W.Relational_product
  module Typing_env = W.Typing_env
  module Typing_env_extension = W.Typing_env_extension
end
