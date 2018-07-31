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

  module RP = Relational_product.Make (Targetint.OCaml) (Kinded_parameter) (T)
  module TEE = Typing_env_extension

  type t = RP.t

  let invariant = RP.invariant
  let print = RP.print
  let equal = RP.equal
  let meet = RP.meet
  let join = RP.join
  let introduce = RP.introduce
  let apply_name_permutation t = RP.apply_name_permutation
  let freshen t = RP.freshen

  let create parameters =
    let indexes_to_parameters =
      Targetint.OCaml.Map.of_list (
        List.mapi (fun index (param, _ty) ->
            Targetint.OCaml.of_int index, param)
          tys)
    in
    if List.length parameters
         <> Kinded_parameter.Set.cardinal indexes_to_parameters
    then begin
      Misc.fatal_errorf "Duplicate parameter(s):@ %a"
        (Targetint.OCaml.Map.print Kinded_parameter.print)
        indexes_to_parameters
    end;
    let env_extension =
      List.fold_left (fun env_extension (param, ty) ->
          TEE.add_equation env_extension (Kinded_parameter.name param) ty)
        TEE.empty
        tys
    in
    RP.create [
      indexes_to_parameters, env_extension
    ]

  let to_list t =
    match RP.components t with
    | [params] -> params
    | _ -> Misc.fatal_errorf "Wrong form of relational product:@ %a" print t
end
