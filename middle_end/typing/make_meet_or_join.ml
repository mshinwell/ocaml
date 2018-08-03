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

module Make (T : Typing_world.S)
    (E : Either_meet_or_join_intf.S with module T := T)
    (S : Meet_and_join_spec_intf.S with module T := T) =
struct
  (* CR mshinwell: Work out which properties we need to prove, e.g.
     Distributivity of meet over join:
       X n (X' u Y') == (X n X') u (X n Y'). *)

  type of_kind_foo = S.of_kind_foo

  let unknown_or_join_is_bottom (uj : _ unknown_or_join) =
    match uj with
    | Join [] -> true
    | Unknown | Join _ -> false

  let unknown_or_join_is_unknown (uj : _ unknown_or_join) =
    match uj with
    | Join _ -> false
    | Unknown -> true

  let rec meet_on_unknown_or_join env perm1 perm2
        (ou1 : S.of_kind_foo unknown_or_join)
        (ou2 : S.of_kind_foo unknown_or_join)
        : S.of_kind_foo unknown_or_join * env_extension =
    if ou1 == ou2 then
      ou1, Typing_env_extension.empty
    else
      match ou1, ou2 with
      | Unknown, ou2 -> ou2, Typing_env_extension.empty
      | ou1, Unknown -> ou1, Typing_env_extension.empty
      | Join of_kind_foos1, Join of_kind_foos2 ->
        let of_kind_foos, env_extension_from_meet =
          List.fold_left
            (fun (of_kind_foos, env_extension_from_meet) of_kind_foo ->
              let new_env_extension_from_meet =
                ref (Typing_env_extension.empty)
              in
              let of_kind_foos =
                Misc.Stdlib.List.filter_map (fun of_kind_foo' ->
                    let meet =
                      let env = Join_env.create env in
                      S.meet_or_join_of_kind_foo env perm1 perm2
                        of_kind_foo of_kind_foo'
                    in
                    match meet with
                    | Ok (of_kind_foo, new_env_extension_from_meet') ->
                      new_env_extension_from_meet :=
                        Typing_env_extension.meet env
                          new_env_extension_from_meet'
                            !new_env_extension_from_meet;
                      Some of_kind_foo
                    | Absorbing -> None)
                  of_kind_foos
              in
              let env_extension_from_meet =
                Typing_env_extension.meet env
                  env_extension_from_meet !new_env_extension_from_meet;
              in
              of_kind_foos, env_extension_from_meet)
            (of_kind_foos2, Typing_env_extension.empty)
            of_kind_foos1
        in
        let same_as input_of_kind_foos =
          List.compare_lengths input_of_kind_foos of_kind_foos = 0
            && List.for_all2 (fun input_of_kind_foo of_kind_foo ->
                   input_of_kind_foo == of_kind_foo)
                 input_of_kind_foos of_kind_foos
        in
        if same_as of_kind_foos1 then ou1, env_extension_from_meet
        else if same_as of_kind_foos2 then ou2, env_extension_from_meet
        else Join of_kind_foos, env_extension_from_meet

  and meet_ty env
        (or_alias1 : S.of_kind_foo ty)
        (or_alias2 : S.of_kind_foo ty)
        : S.of_kind_foo ty * env_extension =
    if Meet_env.check_name_permutations_same_both_sides env
         && or_alias1 == or_alias2
    then begin
      or_alias1, Typing_env_extension.empty
    end else begin
      let unknown_or_join1, canonical_simple1 =
        Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
          (Meet_env.env env)
          ~force_to_kind:S.force_to_kind
          ~print_ty:S.print_ty
          or_alias1
      in
      let unknown_or_join2, canonical_simple2 =
        Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
          (Meet_env.env env)
          ~force_to_kind:S.force_to_kind
          ~print_ty:S.print_ty
          or_alias2
      in
      let add_equation_if_on_a_name env_extension (simple : Simple.t) ty =
        match simple with
        | Name name ->
          Typing_env_extension.add_equation env_extension name ty
        | Const _ | Discriminant _ -> env_extension
      in
      match canonical_simple1, canonical_simple2 with
      | Some simple1, Some simple2
          when Simple.equal simple1 simple2
                 || Meet_env.already_meeting env simple1 simple2 ->
        Equals simple1, Typing_env_extension.empty
      | Some simple1, _ when unknown_or_join_is_unknown unknown_or_join2 ->
        Equals simple1, Typing_env_extension.empty
      | _, Some simple2 when unknown_or_join_is_unknown unknown_or_join1 ->
        Equals simple2, Typing_env_extension.empty
      | Some simple1, Some simple2 ->
        let meet_unknown_or_join, env_extension_from_meet =
          let env = Meet_env.now_meeting env simple1 simple2 in
          meet_on_unknown_or_join env perm1 perm2
            unknown_or_join1 unknown_or_join2
        in
        let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
        let env_extension_from_meet =
          add_equation_if_on_a_name env_extension_from_meet
            simple1 meet_ty
        in
        let env_extension_from_meet =
          add_equation_if_on_a_name env_extension_from_meet
            simple2 (S.to_type (Equals simple1))
        in
        Equals simple1, env_extension_from_meet
      | Some simple1, None ->
        let meet_unknown_or_join, env_extension_from_meet =
          meet_on_unknown_or_join env perm1 perm2
            unknown_or_join1 unknown_or_join2
        in
        let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
        let env_extension_from_meet =
          add_equation_if_on_a_name env_extension_from_meet
            simple1 meet_ty
        in
        Equals simple1, env_extension_from_meet
      | None, Some simple2 ->
        let meet_unknown_or_join, env_extension_from_meet =
          meet_on_unknown_or_join env perm1 perm2
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
          meet_on_unknown_or_join env perm1 perm2
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
        (uj1 : S.of_kind_foo unknown_or_join)
        (uj2 : S.of_kind_foo unknown_or_join)
        : S.of_kind_foo unknown_or_join =
    if uj1 == uj2 then uj1
    else
      match uj1, uj2 with
      | Unknown, _ | _, Unknown -> Unknown
      | Join [], Join [] -> Join []
      | Join of_kind_foos1, Join of_kind_foos2 ->
        (* We rely on the invariant in flambda_type0_intf.ml.
           Everything in [of_kind_foos1] is mutually incompatible with each
           other; likewise in [of_kind_foos2]. *)
        let of_kind_foos =
          List.fold_left (fun of_kind_foos (of_kind_foo, perm) ->
              (* [of_kind_foo] can be compatible with at most one of the
                 elements of [of_kind_foos]. *)
              let found_one = ref false in
              let joined =
                List.map (fun (of_kind_foo', perm') ->
                    let join =
                      (* N.B. If we are here, [S.meet_or_join_of_kind_foo]
                         must be a "join" operation. *)
                      S.meet_or_join_of_kind_foo env
                        of_kind_foo of_kind_foo'
                    in
                    match join with
                    | Ok (of_kind_foo, _env_extension) ->
                      if !found_one then begin
                        (* CR mshinwell: Add detail showing what was
                           wrong. *)
                        Misc.fatal_errorf "Invariant broken for [Join]"
                      end;
                      found_one := true;
                      of_kind_foo, Name_permutation.create ()
                    | Absorbing -> of_kind_foo', perm')
                  of_kind_foos
              in
              if not !found_one then (of_kind_foo, perm) :: of_kind_foos
              else joined)
            of_kind_foos2
            of_kind_foos1
        in
        Join of_kind_foos

  and join_ty env
        (or_alias1 : S.of_kind_foo ty) (or_alias2 : S.of_kind_foo ty)
        : S.of_kind_foo ty =
    if Join_env.fast_check_extensions_same_both_sides env
      && or_alias1 == or_alias2
    then or_alias1
    else
      let unknown_or_join1, canonical_simple1 =
        Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
          (Join_env.environment_on_left env)
          ~force_to_kind:S.force_to_kind
          ~print_ty:S.print_ty
          or_alias1
      in
      let unknown_or_join2, canonical_simple2 =
        Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
          (Join_env.environment_on_right env)
          ~force_to_kind:S.force_to_kind
          ~print_ty:S.print_ty
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
      let alias_both_sides = Name.Set.choose_opt all_aliases in
      match alias_both_sides with
      | Some name -> Equals (Simple.name name)
      | None ->
        let alias1 = Name.Set.choose_opt all_aliases1 in
        let alias2 = Name.Set.choose_opt all_aliases2 in
        match alias1, alias2 with
        | Some name1, _ when unknown_or_join_is_bottom unknown_or_join2 ->
          Equals (Simple.name name1)
        | _, Some name2 when unknown_or_join_is_bottom unknown_or_join1 ->
          Equals (Simple.name name2)
        | None, None ->
          let unknown_or_join =
            join_on_unknown_or_join env perm1 perm2
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
            join_on_unknown_or_join env perm1 perm2
              unknown_or_join1 unknown_or_join2
          in
          No_alias unknown_or_join

  let meet_or_join_ty env or_alias1 or_alias2 =
    E.switch meet_ty join_ty env or_alias1 or_alias2
end
