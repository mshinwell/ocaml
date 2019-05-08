(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = Flambda_types.t Closure_id.Map.t

(* CR mshinwell: Any line of the following form should be removed *)
let print = print_with_cache

let create closure_ids_to_tys =
  let closure_ids_to_logical_variables =
    Closure_id.Map.map (fun _ty ->
        Logical_variable.create (Flambda_kind.value ()))
      closure_ids_to_tys
  in
  let env_extension =
    Closure_id.Map.fold (fun closure_id ty env_extension ->
        let logical_var =
          Closure_id.Map.find closure_id closure_ids_to_logical_variables
        in
        Typing_env_extension.add_equation env_extension
          (Name.logical_var logical_var) ty)
      closure_ids_to_tys
      (Typing_env_extension.empty ())
  in
  create closure_ids_to_logical_variables env_extension
