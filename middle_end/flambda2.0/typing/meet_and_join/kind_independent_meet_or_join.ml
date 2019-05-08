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
  (E : Either_meet_or_join_intf
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension)
  (S : Meet_and_join_spec_intf
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Typing_env_extension := Typing_env_extension) =
struct
  (* CR mshinwell: Work out which properties we need to prove, e.g.
     Distributivity of meet over join:
       X n (X' u Y') == (X n X') u (X n Y'). *)

  let _unknown_or_join_is_bottom (uj : _ Flambda_types.unknown_or_join) =
    match uj with
    | Join Bottom -> true
    | Unknown | Join (Ok _) -> false

  let _unknown_or_join_is_unknown (uj : _ Flambda_types.unknown_or_join) =
    match uj with
    | Join _ -> false
    | Unknown -> true

  let print_ty ppf ty =
    S.print_ty ~cache:(Printing_cache.create ()) ppf ty

  let rec meet_on_unknown_or_join env
        (ou1 : S.of_kind_foo Flambda_types.unknown_or_join)
        (ou2 : S.of_kind_foo Flambda_types.unknown_or_join)
        : S.of_kind_foo Flambda_types.unknown_or_join
            * Typing_env_extension.t =
    if Meet_env.shortcut_precondition env && ou1 == ou2 then
      ou1, Typing_env_extension.empty ()
    else
      match ou1, ou2 with
      | Unknown, ou2 -> ou2, Typing_env_extension.empty ()
      | ou1, Unknown -> ou1, Typing_env_extension.empty ()
      | Join Bottom, Join _ -> ou1, Typing_env_extension.empty ()
      | Join _, Join Bottom -> ou2, Typing_env_extension.empty ()
      | Join of_kind_foo1, Join of_kind_foo2 ->
        let meet = S.meet_or_join_of_kind_foo env of_kind_foo1 of_kind_foo2 in
        match meet with
        | Ok (of_kind_foo, env_extension) ->
          Join (Ok of_kind_foo), env_extension
        | Absorbing -> Join Bottom, env_extension

  and meet_ty env
        (or_alias1 : S.of_kind_foo Flambda_types.ty)
        (or_alias2 : S.of_kind_foo Flambda_types.ty)
        : S.of_kind_foo Flambda_types.ty * Typing_env_extension.t =
    if Meet_env.shortcut_precondition env && or_alias1 == or_alias2
    then begin
      or_alias1, Typing_env_extension.empty ()
    end else begin
      let unknown_or_join1, canonical_simple1 =
        Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
          (Meet_env.env env)
          ~force_to_kind:S.force_to_kind
          ~print_ty
          or_alias1
      in
      let unknown_or_join2, canonical_simple2 =
        Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
          (Meet_env.env env)
          ~force_to_kind:S.force_to_kind
          ~print_ty
          or_alias2
      in
      let add_equation_if_on_a_name env_extension (simple : Simple.t) ty =
        match simple with
        | Name name ->
          Typing_env_extension.meet_equation env_extension
            (Meet_env.env env) name ty
        | Const _ | Discriminant _ -> env_extension
      in
Format.eprintf "CS1 %a, CS2 %a\n%!"
(Misc.Stdlib.Option.print Simple.print) canonical_simple1
(Misc.Stdlib.Option.print Simple.print) canonical_simple2;
      match canonical_simple1, canonical_simple2 with
      | Some simple1, Some simple2
          when Simple.equal simple1 simple2
                 || Meet_env.already_meeting env simple1 simple2 ->
        Equals simple1, Typing_env_extension.empty ()
(*
      | Some simple1, _ when unknown_or_join_is_unknown unknown_or_join2 ->
        Equals simple1, Typing_env_extension.empty ()
      | _, Some simple2 when unknown_or_join_is_unknown unknown_or_join1 ->
        Equals simple2, Typing_env_extension.empty ()
*)
      | Some simple1, Some simple2 ->
Format.eprintf "*** env:@ %a\n%!" Meet_env.print env;
        let meet_unknown_or_join, env_extension_from_meet =
          let env = Meet_env.now_meeting env simple1 simple2 in
          meet_on_unknown_or_join env
            unknown_or_join1 unknown_or_join2
        in
        let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
Format.eprintf "meet_ty %a\n%!" Type_printers.print meet_ty;
        let env_extension_from_meet =
          add_equation_if_on_a_name env_extension_from_meet
            simple1 meet_ty
        in
        let env_extension_from_meet =
          add_equation_if_on_a_name env_extension_from_meet
            simple2 (S.to_type (Equals simple1))
        in
Format.eprintf "Returning =%a, env_extension:@ %a\n%!"
Simple.print simple1
Typing_env_extension.print env_extension_from_meet;
        Equals simple1, env_extension_from_meet
      | Some simple1, None ->
        let meet_unknown_or_join, env_extension_from_meet =
          meet_on_unknown_or_join env
            unknown_or_join1 unknown_or_join2
        in
        let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
        let env_extension_from_meet =
          add_equation_if_on_a_name env_extension_from_meet
            simple1 meet_ty
        in
Format.eprintf "Returning =%a, env_extension:@ %a\n%!"
Simple.print simple1
Typing_env_extension.print env_extension_from_meet;
        Equals simple1, env_extension_from_meet
      | None, Some simple2 ->
        let meet_unknown_or_join, env_extension_from_meet =
          meet_on_unknown_or_join env
            unknown_or_join1 unknown_or_join2
        in
        let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
        let env_extension_from_meet =
          add_equation_if_on_a_name env_extension_from_meet
            simple2 meet_ty
        in
        Equals simple2, env_extension_from_meet
      | None, None ->
        let unknown_or_join, env_extension_from_meet =
          meet_on_unknown_or_join env
            unknown_or_join1 unknown_or_join2
        in
        if unknown_or_join == unknown_or_join1 then begin
          assert (match or_alias1 with No_alias _ -> true | _ -> false);
          or_alias1, env_extension_from_meet
        end else if unknown_or_join == unknown_or_join2 then begin
          assert (match or_alias2 with No_alias _ -> true | _ -> false);
          or_alias2, env_extension_from_meet
        end else begin
          No_alias unknown_or_join, env_extension_from_meet
        end
    end

  let rec join_on_unknown_or_join env
        (uj1 : S.of_kind_foo Flambda_types.unknown_or_join)
        (uj2 : S.of_kind_foo Flambda_types.unknown_or_join)
        : S.of_kind_foo Flambda_types.unknown_or_join =
    if Join_env.shortcut_precondition env && uj1 == uj2 then uj1
    else
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
        (or_alias1 : S.of_kind_foo Flambda_types.ty)
        (or_alias2 : S.of_kind_foo Flambda_types.ty)
        : S.of_kind_foo Flambda_types.ty =
    if Join_env.shortcut_precondition env && or_alias1 == or_alias2
    then or_alias1
    else
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
(*
        | Some name1, _ when unknown_or_join_is_bottom unknown_or_join2 ->
          Equals (Simple.name name1)
        | _, Some name2 when unknown_or_join_is_bottom unknown_or_join1 ->
          Equals (Simple.name name2)
*)
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
        (or_alias1 : S.of_kind_foo Flambda_types.ty)
        (or_alias2 : S.of_kind_foo Flambda_types.ty) =
    E.switch_no_bottom meet_ty (join_ty ?bound_name) env or_alias1 or_alias2
end
