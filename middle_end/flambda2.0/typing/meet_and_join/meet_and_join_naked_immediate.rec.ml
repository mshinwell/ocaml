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

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  type of_kind_foo = K.naked_immediate Flambda_types.of_kind_naked_number

  let kind = K.naked_immediate
  let to_type ty : Flambda_types.t = Naked_number (ty, Naked_immediate)
  let force_to_kind = Flambda_type0_core.force_to_kind_naked_immediate
  let print_ty = Type_printers.print_ty_naked_immediate_with_cache
  let apply_rec_info = Flambda_type0_core.apply_rec_info_of_kind_naked_number

  let meet_or_join_of_kind_foo _meet_or_join_env
        (of_kind1 : K.naked_immediate Flambda_types.of_kind_naked_number)
        (of_kind2 : K.naked_immediate Flambda_types.of_kind_naked_number)
        : (K.naked_immediate Flambda_types.of_kind_naked_number
            * Typing_env_extension.t) Or_bottom_or_absorbing.t =
    match of_kind1, of_kind2 with
    | Immediate fs1, Immediate fs2 ->
      let fs = E.Immediate.Set.union_or_inter fs1 fs2 in
      if Immediate.Set.is_empty fs then Bottom
      else Ok (Immediate fs, Typing_env_extension.empty)
end
