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

module T = Flambda_types

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  type of_kind_foo = T.of_kind_fabricated

  let kind = K.fabricated
  let to_type ty : T.t = Fabricated ty
  let force_to_kind = Flambda_type0_core.force_to_kind_fabricated
  let print_ty = Type_printers.print_ty_fabricated_with_cache

  let meet_or_join_set_of_closures_entry env
          ({ by_closure_id = by_closure_id1; } : T.set_of_closures_entry)
          ({ by_closure_id = by_closure_id2; } : T.set_of_closures_entry) =
    let meet_or_join =
      E.switch Types_by_closure_id.meet Types_by_closure_id.join
        env by_closure_id1 by_closure_id2
    in
    Or_bottom.map meet_or_join ~f:(fun (by_closure_id, env_extension) ->
      let set_of_closures_entry : T.set_of_closures_entry =
        { by_closure_id; }
      in
      set_of_closures_entry, env_extension)

  let meet_or_join_of_kind_foo env
        (of_kind1 : T.of_kind_fabricated) (of_kind2 : T.of_kind_fabricated) =
    let or_bottom =
      match of_kind1, of_kind2 with
      | Discriminants discrs1, Discriminants discrs2 ->
        Or_bottom.map
          (E.switch Discriminants.meet Discriminants.join env discrs1 discrs2)
          ~f:(fun (discrs, env_extension) ->
            Discriminants discrs, env_extension)
      | Set_of_closures { closures = closures1; },
          Set_of_closures { closures = closures2; } ->
        Or_bottom.map
          (E.switch Closure_ids.meet Closure_ids.join env closures1 closures2)
          ~f:(fun (closures, env_extension) ->
            Set_of_closures { closures; }, env_extension)
      | (Discriminants _ | Set_of_closures _), _ -> Absorbing
    in
    Or_absorbing_or_bottom.of_or_bottom or_bottom
end
