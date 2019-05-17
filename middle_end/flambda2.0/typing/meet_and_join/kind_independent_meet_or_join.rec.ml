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
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension)
  (S : Meet_and_join_spec_intf.S
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Typing_env_extension := Typing_env_extension) =
struct
  let print_ty ppf ty =
    S.print_ty ~cache:(Printing_cache.create ()) ppf ty

  let add_equation env (simple : Simple.t) ty env_extension =
    match simple with
    | Name name -> TEE.meet_equation env_extension (Meet_env.env env) name ty
    | Const _ | Discriminant _ -> env_extension

  let rec meet_on_unknown_or_join env
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

  and meet_ty env
        (or_alias1 : S.of_kind_foo T.ty) (or_alias2 : S.of_kind_foo T.ty)
        : S.of_kind_foo T.ty * TEE.t =
    let unknown_or_join1, canonical_simple1 =
      Typing_env.resolve_aliases_on_ty (Meet_env.env env)
        ~force_to_kind:S.force_to_kind ~print_ty or_alias1
    in
    let unknown_or_join2, canonical_simple2 =
      Typing_env.resolve_aliases_on_ty (Meet_env.env env)
        ~force_to_kind:S.force_to_kind ~print_ty or_alias2
    in
    match canonical_simple1, canonical_simple2 with
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
      let unknown_or_join, env_extension =
        let env = Meet_env.now_meeting env simple1 simple2 in
        meet_on_unknown_or_join env unknown_or_join1 unknown_or_join2
      in
      let env_extension =
        env_extension
        |> add_equation env simple1 (S.to_type (No_alias unknown_or_join))
        |> add_equation env simple2 (S.to_type (Equals simple1))
      in
      Equals simple1, env_extension
    | Some simple, None | None, Some simple ->
      let unknown_or_join, env_extension =
        meet_on_unknown_or_join env unknown_or_join1 unknown_or_join2
      in
      let env_extension =
        add_equation simple (S.to_type (No_alias unknown_or_join)) env_extension
      in
      Equals simple, env_extension

  let rec join_on_unknown_or_join env
        (uj1 : S.of_kind_foo T.unknown_or_join)
        (uj2 : S.of_kind_foo T.unknown_or_join)
        : S.of_kind_foo T.unknown_or_join =
    match uj1, uj2 with
    | Unknown, _ | _, Unknown -> Unknown
    | Join Bottom, Join _ -> uj2
    | Join _, Join Bottom -> uj1
    | Join (Ok of_kind_foo1), Join (Ok of_kind_foo2) ->
      (* CR mshinwell: What happens if one of the [of_kind_foo]s is actually
         bottom even if it says [Ok]? *)
      (* N.B. If we are here, [S.meet_or_join_of_kind_foo]
         must be a "join" operation. *)
      let join =
        S.meet_or_join_of_kind_foo env of_kind_foo1 of_kind_foo2
      in
      match join with
      | Ok (of_kind_foo, _env_extension) -> Join (Ok of_kind_foo)
      | Absorbing -> Unknown

  and join_ty ?bound_name env
        (or_alias1 : S.of_kind_foo T.ty)
        (or_alias2 : S.of_kind_foo T.ty)
        : S.of_kind_foo T.ty =
    let unknown_or_join1, canonical_simple1 =
      Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
        (Join_env.environment_on_left env)
        ~force_to_kind:S.force_to_kind
        ~print_ty
        ?bound_name
        or_alias1
    in
    let unknown_or_join2, canonical_simple2 =
      Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
        (Join_env.environment_on_right env)
        ~force_to_kind:S.force_to_kind
        ~print_ty
        ?bound_name
        or_alias2
    in
    let all_aliases1 =
      match canonical_simple1 with
      | None -> Name.Set.empty
      | Some canonical_simple ->
        Typing_env.aliases_of_simple (Join_env.environment_on_left env)
          canonical_simple
    in
    let all_aliases2 =
      match canonical_simple2 with
      | None -> Name.Set.empty
      | Some canonical_simple ->
        Typing_env.aliases_of_simple (Join_env.environment_on_right env)
          canonical_simple
    in
    let all_aliases = Name.Set.inter all_aliases1 all_aliases2 in
    let all_aliases =
      match bound_name with
      | None -> all_aliases
      | Some bound_name ->
        let all_aliases_of_bound_name =
          Typing_env.aliases_of_simple
            (Join_env.central_typing_environment env)
            (Simple.name bound_name)
        in
        Name.Set.diff all_aliases all_aliases_of_bound_name
    in
    let alias_both_sides = Name.Set.choose_opt all_aliases in
    match alias_both_sides with
    | Some name -> Equals (Simple.name name)
    | None ->
      let alias1 = Name.Set.choose_opt all_aliases1 in
      let alias2 = Name.Set.choose_opt all_aliases2 in
      match alias1, alias2 with
      | None, None ->
        let unknown_or_join =
          join_on_unknown_or_join env
            unknown_or_join1 unknown_or_join2
        in
        if unknown_or_join == unknown_or_join1 then begin
          assert (match or_alias1 with No_alias _ -> true | _ -> false);
          or_alias1
        end else if unknown_or_join == unknown_or_join2 then begin
          assert (match or_alias2 with No_alias _ -> true | _ -> false);
          or_alias2
        end else begin
          No_alias unknown_or_join
        end
      | _, _ ->
        let unknown_or_join =
          join_on_unknown_or_join env unknown_or_join1 unknown_or_join2
        in
        No_alias unknown_or_join

  let meet_or_join_ty ?bound_name env
        (or_alias1 : S.of_kind_foo T.ty)
        (or_alias2 : S.of_kind_foo T.ty) : _ Or_bottom.t =
    let ty, env_extension =
      E.switch_no_bottom meet_ty (join_ty ?bound_name) env or_alias1 or_alias2
    in
    if Flambda_type0_core.ty_is_bottom env ty then Bottom
    else Ok (ty, env_extension)
end
