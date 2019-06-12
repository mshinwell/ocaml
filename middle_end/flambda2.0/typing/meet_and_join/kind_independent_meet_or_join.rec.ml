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

(* CR mshinwell: Work out which properties we need to prove, e.g.
   Distributivity of meet over join:
     X n (X' u Y') == (X n X') u (X n Y'). *)

module T = Flambda_types
module TEE = Typing_env_extension

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t)
  (S : Meet_and_join_spec_intf.S
    with type flambda_type := Flambda_types.t
    with type 'a ty := 'a Flambda_types.ty
    with type meet_env := Meet_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  let print_ty ppf ty =
    S.print_ty ~cache:(Printing_cache.create ()) ppf ty

  let add_equation _env (simple : Simple.t) ty env_extension =
    match simple with
    (* CR mshinwell: Does this need to use some kind of [meet_equation]? *)
    | Name name -> TEE.add_or_replace_equation env_extension name ty
    | Const _ | Discriminant _ -> env_extension

  let all_aliases_of env simple_opt =
    match simple_opt with
    | None -> Name.Set.empty
    | Some simple -> Typing_env.aliases_of_simple env simple

  let meet_on_unknown_or_join env
        (ou1 : S.of_kind_foo T.unknown_or_join)
        (ou2 : S.of_kind_foo T.unknown_or_join)
        : S.of_kind_foo T.unknown_or_join * TEE.t =
    match ou1, ou2 with
    | Unknown, ou2 -> ou2, TEE.empty
    | ou1, Unknown -> ou1, TEE.empty
    | Bottom, _ | _, Bottom -> Bottom, TEE.empty
    | Ok of_kind_foo1, Ok of_kind_foo2 ->
      match S.meet_or_join_of_kind_foo env of_kind_foo1 of_kind_foo2 with
      | Ok (of_kind_foo, env_extension) -> Ok of_kind_foo, env_extension
      | Absorbing | Bottom -> Bottom, TEE.empty

  let join_on_unknown_or_join env
        (uj1 : S.of_kind_foo T.unknown_or_join)
        (uj2 : S.of_kind_foo T.unknown_or_join)
        : S.of_kind_foo T.unknown_or_join =
    match uj1, uj2 with
    | Bottom, _ -> uj2
    | _, Bottom -> uj1
    | Unknown, _ | _, Unknown -> Unknown
    | Ok of_kind_foo1, Ok of_kind_foo2 ->
      let env = Meet_env.create env in
      match S.meet_or_join_of_kind_foo env of_kind_foo1 of_kind_foo2 with
      | Ok (of_kind_foo, _env_extension) -> Ok of_kind_foo
      | Bottom -> Bottom
      | Absorbing -> Unknown

  let meet_ty env
        (or_alias1 : S.of_kind_foo T.ty) (or_alias2 : S.of_kind_foo T.ty)
        : S.of_kind_foo T.ty * TEE.t =
    let unknown_or_join1, alias1 =
      Typing_env.resolve_any_toplevel_alias_on_ty0 (Meet_env.env env)
        ~force_to_kind:S.force_to_kind ~print_ty or_alias1
    in
    let unknown_or_join2, alias2 =
      Typing_env.resolve_any_toplevel_alias_on_ty0 (Meet_env.env env)
        ~force_to_kind:S.force_to_kind ~print_ty or_alias2
    in
    match alias1, alias2 with
    | None, None ->
      let unknown_or_join, env_extension =
        meet_on_unknown_or_join env unknown_or_join1 unknown_or_join2
      in
      No_alias unknown_or_join, env_extension
    | Some simple1, Some simple2
        when Simple.equal simple1 simple2
               || Meet_env.already_meeting env simple1 simple2 ->
      Equals simple1, TEE.empty
    | Some simple1, Some simple2 ->
(*
Format.eprintf "Meeting simples: %a and %a\n%!"
  Simple.print simple1
  Simple.print simple2;
*)
      let unknown_or_join, env_extension =
        let env = Meet_env.now_meeting env simple1 simple2 in
        meet_on_unknown_or_join env unknown_or_join1 unknown_or_join2
      in
(*
Format.eprintf "TEE from meeting simples (1): %a\n%!"
  Typing_env_extension.print env_extension;
*)
      let env_extension =
        if Typing_env.defined_earlier (Meet_env.env env) simple1 ~than:simple2
        then
          env_extension
          |> add_equation env simple1 (S.to_type (No_alias unknown_or_join))
          |> add_equation env simple2 (S.to_type (Equals simple1))
        else
          env_extension
          |> add_equation env simple2 (S.to_type (No_alias unknown_or_join))
          |> add_equation env simple1 (S.to_type (Equals simple2))
      in
(*
Format.eprintf "TEE from meeting simples (2): %a\n%!"
  Typing_env_extension.print env_extension;
*)
      (* It doesn't matter whether [simple1] or [simple2] is returned here. *)
      Equals simple1, env_extension
    | Some simple, None | None, Some simple ->
      let unknown_or_join, env_extension =
        meet_on_unknown_or_join env unknown_or_join1 unknown_or_join2
      in
      let env_extension =
        env_extension
        |> add_equation env simple (S.to_type (No_alias unknown_or_join))
      in
      Equals simple, env_extension

  let join_ty ?bound_name env
        (or_alias1 : S.of_kind_foo T.ty) (or_alias2 : S.of_kind_foo T.ty)
        : S.of_kind_foo T.ty =
    let unknown_or_join1, canonical_simple1 =
      Typing_env.resolve_any_toplevel_alias_on_ty0 env
        ~force_to_kind:S.force_to_kind ~print_ty or_alias1
    in
    let unknown_or_join2, canonical_simple2 =
      Typing_env.resolve_any_toplevel_alias_on_ty0 env
        ~force_to_kind:S.force_to_kind ~print_ty or_alias2
    in
    (* CR mshinwell: Think further about this "bound name" stuff. *)
    let shared_aliases_not_aliasing_bound_name =
      Name.Set.diff
        (Name.Set.inter (all_aliases_of env canonical_simple1)
          (all_aliases_of env canonical_simple2))
        (all_aliases_of env (Option.map Simple.name bound_name))
    in
    match Name.Set.choose_opt shared_aliases_not_aliasing_bound_name with
    | Some name -> Equals (Simple.name name)
    | None ->
      No_alias (join_on_unknown_or_join env unknown_or_join1 unknown_or_join2)

  let meet_or_join_ty ?bound_name env
        (or_alias1 : S.of_kind_foo T.ty)
        (or_alias2 : S.of_kind_foo T.ty) : _ Or_bottom.t =
    let ty, env_extension =
      E.switch_no_bottom meet_ty (join_ty ?bound_name) env or_alias1 or_alias2
    in
    if Flambda_type0_core.ty_is_obviously_bottom ty then Bottom
    else Ok (ty, env_extension)
end
