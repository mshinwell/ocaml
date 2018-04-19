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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Make (T : sig
  include Flambda_type0_internal_intf.S
end) (Typing_env0 : sig
  include Typing_env0_intf.S
    with type typing_environment := T.typing_environment
    with type env_extension := T.env_extension
    with type flambda_type := T.flambda_type
    with type t_in_context := T.t_in_context
    with type 'a ty = 'a T.ty
    with type 'a unknown_or_join = 'a T.unknown_or_join
end) (Meet_and_join : sig
  include Meet_and_join_intf.S_for_types
    with type t_in_context := T.t_in_context
    with type env_extension := T.env_extension
    with type flambda_type := T.flambda_type
end) (Type_equality : sig
  include Type_equality_intf.S
    with type flambda_type := T.flambda_type
end) = struct
  type t = T.env_extension

  open T

  module TE = Typing_env0

  let print ppf t = print_typing_env_extension ppf t

  let fast_equal t1 t2 = (t1 == t2)

  let is_empty t = Scope_level.Map.is_empty t.at_or_after_cut_point

  let meet (env : typing_environment) (t1 : t) (t2 : t) : t =
    if fast_equal t1 t2 then t1
    else if is_empty t1 then t2
    else if is_empty t2 then t1
    else
      let scope_level = TE.max_level env in
      let env = TE.add_or_meet_env_extension env t1 scope_level in
      let env = TE.add_or_meet_env_extension env t2 scope_level in
      TE.cut env_with_t1_and_t2
        ~existential_if_defined_at_or_later_than:scope_level

  let join (t1 : typing_environment) (t2 : typing_environment)
        join_scope_level : typing_environment =
    assert (Scope_level.(>=) (max_level t1) join_scope_level);
    assert (Scope_level.(>=) (max_level t2) join_scope_level);
    if fast_equal t1 t2 then t1
    else if is_empty t1 then create_using_resolver_from t1
    else if is_empty t2 then create_using_resolver_from t1
    else
      let t =
        fold t2 ~init:(create_using_resolver_from t1)
          ~f:(fun t name binding_type level
                  (entry : typing_environment_entry0) ->
            match find0_opt t name with
            | Some (existing_binding_type, existing_level, existing_entry) ->
              assert (existing_binding_type = binding_type);
              begin match existing_entry, entry with
              | Definition ty1, Definition ty2 ->
                assert (Type_equality.equal ty1 ty2);
                assert (Scope_level.With_sublevel.equal existing_level level);
                t
              | Definition ty1, Equation ty2
              | Equation ty1, Equation ty2 ->
                let join_ty = Meet_and_join.join (t, ty1) (t2, ty2) in
                add_with_binding_type t name join_scope_level binding_type
                  (Equation join_ty)
              | Equation _, Definition _ ->
                Misc.fatal_errorf "Environments disagree on the definition \
                    point of %a:@ %a@ versus:@ %a"
                  Name.print name
                  print t1
                  print t2
              end
            | None -> t)
      in
      let cse_to_names =
        Flambda_primitive.With_fixed_value.Map.inter_merge
          (fun name1 name2 ->
            let level1 = scope_level_exn t1 name1 in
            let level2 = scope_level_exn t2 name2 in
            (* Keep the outermost binding. *)
            if Scope_level.With_sublevel.(>) level1 level2 then name2
            else name1)
          t1.cse_to_names t2.cse_to_names
      in
      let t =
        { t with
          cse_to_names;
        }
      in
      invariant t;
      t

(*
    type t = env_extension

    let create = create_env_extension

    let singleton_env_extension ~resolver name scope_level ty =
      { typing_judgements =
          Some (Typing_env0.singleton0 ~resolver name scope_level ty
            ~must_be_closed:false);
      }

    let add_or_replace ~resolver t name scope_level ty =
      match t.typing_judgements with
      | None -> singleton_env_extension ~resolver name scope_level ty
      | Some typing_judgements ->
        { typing_judgements =
            Some (Typing_env0.add_or_replace typing_judgements name
              scope_level ty);
        }

    let invariant t =
      match t.typing_judgements with
      | None -> ()
      | Some typing_judgements ->
        assert (not typing_judgements.must_be_closed);
        Typing_env0.invariant typing_judgements

    let singleton = singleton_env_extension

    let add ~resolver t name scope_level ty =
      match t.typing_judgements with
      | None -> singleton ~resolver name scope_level ty
      | Some typing_judgements ->
        { typing_judgements =
            Some (Typing_env0.add typing_judgements name scope_level ty);
        }

    let add_or_replace_meet ~resolver t name scope_level ty =
      match t.typing_judgements with
      | None -> singleton ~resolver name scope_level ty
      | Some typing_judgements ->
        { typing_judgements =
            Some (Typing_env0.add_or_replace_meet typing_judgements
              name scope_level ty);
        }

    let meet = Meet_and_join.meet_env_extension

    let equal ~equal_type
          { typing_judgements = typing_judgements1;
          }
          { typing_judgements = typing_judgements2;
          } =
      Misc.Stdlib.Option.equal (Typing_env0.equal ~equal_type)
        typing_judgements1 typing_judgements2

    let phys_equal { typing_judgements = typing_judgements1; }
          { typing_judgements = typing_judgements2; } =
      typing_judgements1 == typing_judgements2
        || match typing_judgements1, typing_judgements2 with
           | None, None -> true
           | None, Some _ | Some _, None -> false
           | Some env1, Some env2 ->
             Typing_env0.phys_equal env1 env2

    let print = print_env_extension

    let remove ({ typing_judgements; } as t) name =
      match typing_judgements with
      | None -> t
      | Some typing_judgements ->
        let typing_judgements =
          Typing_env0.remove typing_judgements name
        in
        { typing_judgements = Some typing_judgements; }

    let to_typing_environment ~resolver { typing_judgements; } =
      match typing_judgements with
      | None -> Typing_env0.create ~resolver
      | Some typing_judgements -> typing_judgements

    let domain { typing_judgements; } =
      match typing_judgements with
      | None -> Name.Set.empty
      | Some typing_judgements ->
        Name.Map.keys typing_judgements.names_to_types

    let fold t ~init ~f =
      match t.typing_judgements with
      | None -> init
      | Some typing_judgements ->
        Name.Map.fold (fun name (level, ty) acc ->
            f acc name level ty)
          typing_judgements.names_to_types
          init
*)
end
