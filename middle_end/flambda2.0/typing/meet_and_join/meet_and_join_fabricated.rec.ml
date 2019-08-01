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

module T = Type_grammar

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  type of_kind_foo = T.of_kind_fabricated

  let kind = K.fabricated
  let to_type ty : T.t = Fabricated ty
  let force_to_kind = Basic_type_ops.force_to_kind_fabricated
  let print_ty = Type_printers.print_ty_fabricated_with_cache
  let apply_rec_info = Basic_type_ops.apply_rec_info_of_kind_fabricated

  let meet_or_join_of_kind_foo env ~meet_or_join_ty:_
        (of_kind1 : T.of_kind_fabricated) (of_kind2 : T.of_kind_fabricated) =
    match of_kind1, of_kind2 with
    | Discriminants discrs1, Discriminants discrs2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch Discriminants.meet Discriminants.join env discrs1 discrs2)
        ~f:(fun (discrs, env_extension) : (T.of_kind_fabricated * _) ->
          Discriminants discrs, env_extension)
end
