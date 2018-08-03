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

  (* CR mshinwell: Share with closures_entry_by_closure_id.ml *)
  module Closure_id = struct
    include Closure_id

    let free_names _t = Name_occurrences.create ()
    let bound_names _t = Name_occurrences.create ()

    let apply_name_permutation t _perm = t
    let freshen t _freshening = t
  end

  module RP = Relational_product.Make (Closure_id) (Logical_variable_component)

  type t = RP.t

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
        Typing_env_extension.empty
    in
    RP.create [
      closure_ids_to_logical_variables, env_extension;
    ]

  let print ~cache ppf t = RP.print_with_cache ~cache ppf t

  let meet env t1 t2 = RP.meet env Fresh t1 t2
  let join env t1 t2 = RP.join env Fresh t1 t2

  let bound_names = RP.bound_names
  let free_names = RP.free_names
  let apply_name_permutation = RP.apply_name_permutation
  let freshen = RP.freshen

  module Flambda_type0_core = W.Flambda_type0_core
  module Join_env = W.Join_env
  module Meet_env = W.Meet_env
  module Relational_product = W.Relational_product
  module Typing_env = W.Typing_env
  module Typing_env_extension = W.Typing_env_extension
end
