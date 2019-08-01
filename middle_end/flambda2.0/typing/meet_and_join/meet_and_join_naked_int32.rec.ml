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

module TEE = Typing_env_extension

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  type of_kind_foo = K.naked_int32 Type_grammar.of_kind_naked_number

  let kind = K.naked_int32
  let to_type ty : Type_grammar.t = Naked_number (ty, Naked_int32)
  let force_to_kind = Basic_type_ops.force_to_kind_naked_int32
  let print_ty = Type_printers.print_ty_naked_int32_with_cache
  let apply_rec_info = Basic_type_ops.apply_rec_info_of_kind_naked_number

  let meet_or_join_of_kind_foo _meet_or_join_env ~meet_or_join_ty:_
        (of_kind1 : K.naked_int32 Type_grammar.of_kind_naked_number)
        (of_kind2 : K.naked_int32 Type_grammar.of_kind_naked_number)
        : (K.naked_int32 Type_grammar.of_kind_naked_number * TEE.t)
            Or_bottom_or_absorbing.t =
    match of_kind1, of_kind2 with
    | Int32 fs1, Int32 fs2 ->
      let fs = E.Int32.Set.union_or_inter fs1 fs2 in
      if Int32.Set.is_empty fs then Bottom
      else Ok (Int32 fs, TEE.empty ())
end
