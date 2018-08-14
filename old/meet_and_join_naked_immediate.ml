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

module K = Flambda_kind

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Flambda_type0_core = struct end
module Flambda_types = struct end
module Join_env = struct end
module Meet_env = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  module Make
    (E : Either_meet_or_join_intf.S
      with module Join_env = Join_env
      with module Meet_env = Meet_env
      with module Typing_env = Typing_env
      with module Typing_env_extension = Typing_env_extension) =
  struct
    type of_kind_foo = Immediate.Set.t Flambda_types.of_kind_naked_number

    let kind = K.naked_immediate ()

    let to_type ty : Flambda_types.t = Naked_number (ty, Naked_immediate)

    let force_to_kind = Flambda_type0_core.force_to_kind_naked_immediate
    let print_ty = Type_printers.print_ty_naked_immediate_with_cache

    let meet_or_join_of_kind_foo _meet_or_join_env
          (of_kind1 : Immediate.Set.t Flambda_types.of_kind_naked_number)
          (of_kind2 : Immediate.Set.t Flambda_types.of_kind_naked_number)
          : (Immediate.Set.t Flambda_types.of_kind_naked_number
              * Typing_env_extension.t) Or_absorbing.t =
      match of_kind1, of_kind2 with
      | Immediate fs1, Immediate fs2 ->
        let fs = E.Immediate.Set.union_or_inter fs1 fs2 in
        if Immediate.Set.is_empty fs then Absorbing
        else Ok (Immediate fs, Typing_env_extension.empty)
      | _, _ -> Absorbing
  end
end
