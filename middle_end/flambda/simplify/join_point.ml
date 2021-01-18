(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

module U = One_continuation_use

let simple_join typing_env uses ~params =
  (* This join is intended to be sufficient to match Closure + Cmmgen
     on unboxing, but not really anything more. *)
  let bottom_types =
    ListLabels.map params ~f:(fun param ->
      Kinded_parameter.kind param
      |> Flambda_kind.With_subkind.kind
      |> T.bottom)
  in
  let joined_types =
    ListLabels.fold_left uses ~init:bottom_types ~f:(fun joined_types use ->
      ListLabels.map2 joined_types (U.arg_types use)
        ~f:(fun joined_type arg_type ->
          let arg_type =
            T.eviscerate arg_type (DE.typing_env (U.env_at_use use))
          in
          (* The only names left in [arg_type] will be symbols; they will
             always be defined in [typing_env].  So we can use the same
             environment throughout the join. *)
          T.join typing_env ~left_env:typing_env ~left_ty:joined_type
            ~right_env:typing_env ~right_ty:arg_type))
  in
  ListLabels.fold_left2 params joined_types ~init:typing_env
    ~f:(fun handler_env param joined_type ->
      let name = Kinded_parameter.name param in
      TE.add_equation handler_env name joined_type)

let compute_handler_env cont (cont_uses_env : CUE.t) dacc
      ~env_at_fork_plus_params_and_consts
      ~consts_lifted_during_body
      ~params
      ~code_age_relation_after_body : Continuation_env_and_param_types.t =
(*
Format.eprintf "%d uses for %a\n%!"
  (List.length t.uses)
  Continuation.print t.continuation;
*)
  (* CR mshinwell: improve interface for this in CUE *)
  match Continuation.Map.find cont (CUE.get_uses cont_uses_env) with
  | exception Not_found -> No_uses
  | uses when Continuation_uses.get_uses uses = [] -> No_uses
  | uses ->
    let uses = Continuation_uses.get_uses uses in
    let definition_scope_level =
      DE.get_continuation_scope_level env_at_fork_plus_params_and_consts
    in
    let need_to_meet_param_types =
      (* CR mshinwell: Unsure if this is worth doing. *)
      (* If there is information available from the subkinds of the
         parameters, we will need to meet the existing parameter types
         (e.g. "unknown boxed float") with the argument types at each
         use. *)
      List.exists (fun param ->
          Kinded_parameter.kind param
          |> Flambda_kind.With_subkind.has_useful_subkind_info)
        params
    in
    let use_envs_with_ids =
      List.map (fun use ->
      (*
          Format.eprintf "Use: parameters: %a,@ arg types: %a,@ env:@ %a\n%!"
            Kinded_parameter.List.print params
            (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
            (U.arg_types use) DE.print (U.env_at_use use);
        *)
          let use_env =
            DE.map_typing_env (U.env_at_use use) ~f:(fun typing_env ->
              if need_to_meet_param_types then
                TE.meet_equations_on_params typing_env
                  ~params ~param_types:(U.arg_types use)
              else
                TE.add_equations_on_params typing_env
                  ~params ~param_types:(U.arg_types use))
          in
          use_env, U.id use, U.use_kind use, U.arg_types use)
        uses
    in
(*
Format.eprintf "Unknown at or later than %a\n%!"
  Scope.print (Scope.next definition_scope_level);
*)
    let handler_env, extra_params_and_args, is_single_inlinable_use,
        is_single_use, dacc =
      match use_envs_with_ids with
      | [use_env, _, Inlinable, _]
          when not (Continuation.is_exn cont) ->
        (* We need to make sure any lifted constants generated during the
           simplification of the body are in the environment.  Otherwise
           we might share a constant based on information in [DA] but then
           find the definition of the corresponding constant isn't in [DE].
           Note that some of the constants may already be defined. *)
        let use_env =
          DE.add_lifted_constants ~maybe_already_defined:() use_env
            consts_lifted_during_body
        in
        use_env, Continuation_extra_params_and_args.empty, true, true, dacc
      | [] | [_, _, (Inlinable | Non_inlinable), _]
      | (_, _, (Inlinable | Non_inlinable), _) :: _ ->
        (* The lifted constants are put into the fork environment now because
           it overall makes things easier; the join operation can just discard
           any equation about a lifted constant (any such equation could not be
           materially more precise anyway). *)
        let denv =
          DE.add_lifted_constants env_at_fork_plus_params_and_consts
            consts_lifted_during_body
        in
        let extra_lifted_consts_in_use_envs =
          LCS.all_defined_symbols consts_lifted_during_body
        in
        let use_envs_with_ids' =
          (* CR mshinwell: Stop allocating this *)
          List.map (fun (use_env, id, use_kind) ->
              DE.typing_env use_env, id, use_kind)
            use_envs_with_ids
        in
        let typing_env = DE.typing_env denv in
        let handler_env, extra_params_and_args, dacc =
          if not (Flambda_features.join_points ()) then
            let handler_env = simple_join typing_env uses ~params in
            let denv = DE.with_typing_env denv handler_env in
            denv, Continuation_extra_params_and_args.empty, dacc
          else
            let module CSE = Common_subexpression_elimination in
            let cse_join_result =
              assert (Scope.equal definition_scope_level
                (TE.current_scope typing_env));
              CSE.join ~typing_env_at_fork:typing_env
                ~cse_at_fork:(DE.cse denv)
                ~use_info:use_envs_with_ids
                ~get_typing_env:(fun (use_env, _, _, _) ->
                  DE.typing_env use_env)
                ~get_rewrite_id:(fun (_, id, _, _) -> id)
                ~get_cse:(fun (use_env, _, _, _) -> DE.cse use_env)
                ~params
            in
            let extra_params_and_args =
              match cse_join_result with
              | None -> Continuation_extra_params_and_args.empty
              | Some cse_join_result -> cse_join_result.extra_params
            in
            let extra_allowed_names =
              match cse_join_result with
              | None -> Name_occurrences.empty
              | Some cse_join_result -> cse_join_result.extra_allowed_names
            in
            let module PDCE = Partial_dead_code_elimination in
            let module PDCE_join_result = PDCE.For_downwards_env.Join_result in
            let pdce_at_fork = DE.pdce denv in
            let pdce_join_result =
              PDCE.For_downwards_env.join ~typing_env_at_fork:typing_env
                ~pdce_at_fork
                ~use_info:use_envs_with_ids
                ~get_typing_env:(fun (use_env, _, _, _) ->
                  DE.typing_env use_env)
                ~get_rewrite_id:(fun (_, id, _, _) -> id)
                ~get_arg_types:(fun (_, _, _, arg_types) -> arg_types)
                ~get_pdce:(fun (use_env, _, _, _) -> DE.pdce use_env)
                ~params
            in
            let extra_params_and_args =
              match pdce_join_result with
              | None -> Continuation_extra_params_and_args.empty
              | Some pdce_join_result ->
                PDCE_join_result.extra_params pdce_join_result
            in
            let extra_allowed_names =
              match pdce_join_result with
              | None -> Name_occurrences.empty
              | Some pdce_join_result ->
                PDCE_join_result.extra_allowed_names pdce_join_result
            in
            let env_extension =
              TE.cut_and_n_way_join typing_env
                use_envs_with_ids'
                ~params
                (* CR mshinwell: If this didn't do Scope.next then TE could
                   probably be slightly more efficient, as it wouldn't need
                   to look at the middle of the three return values from
                   Scope.Map.Split. *)
                ~unknown_if_defined_at_or_later_than:
                  (Scope.next definition_scope_level)
                ~extra_lifted_consts_in_use_envs
                ~extra_allowed_names
            in
(*
Format.eprintf "handler env extension for %a is:@ %a\n%!"
  Continuation.print t.continuation
  T.Typing_env_extension.print env_extension;
Format.eprintf "The extra params and args are:@ %a\n%!"
  Continuation_extra_params_and_args.print extra_params_and_args;
*)
            let handler_env =
              typing_env
              |> TE.add_definitions_of_params
                ~params:extra_params_and_args.extra_params
            in
            let handler_env =
              match cse_join_result with
              | None -> handler_env
              | Some cse_join_result ->
                Name.Map.fold (fun name ty handler_env ->
                    TE.add_equation handler_env name ty)
                  cse_join_result.extra_equations
                  handler_env
            in
            let handler_env =
              match pdce_join_result with
              | None -> handler_env
              | Some pdce_join_result ->
                Name.Map.fold (fun name ty handler_env ->
                    TE.add_equation handler_env name ty)
                  (PDCE_join_result.extra_equations pdce_join_result)
                  handler_env
            in
            let handler_env =
              TE.add_env_extension handler_env env_extension
            in
            let denv = DE.with_typing_env denv handler_env in
            let denv =
              match cse_join_result with
              | None -> denv
              | Some cse_join_result ->
                DE.with_cse denv cse_join_result.cse_at_join_point
            in
            let denv, dacc =
              match pdce_join_result with
              | None -> denv, dacc
              | Some pdce_join_result ->
                let pdce, pdce_acc =
                  PDCE.For_downwards_env.post_join ~pdce_at_fork
                    (DA.pdce_acc dacc) pdce_join_result
                    ~typing_env_at_join:handler_env ~params
                in
                let denv = DE.with_pdce denv pdce in
                let dacc = DA.with_pdce_acc dacc pdce_acc in
                denv, dacc
            in
            denv, extra_params_and_args, dacc
        in
        let denv =
          DE.map_typing_env handler_env ~f:(fun handler_env ->
            TE.with_code_age_relation handler_env
              code_age_relation_after_body)
        in
        let is_single_use =
          match uses with
          | [_] -> true
          | [] | _::_::_ -> false
        in
        match use_envs_with_ids with
        | [_, _, Inlinable] -> assert false  (* handled above *)
        | [] | [_, _, Non_inlinable]
        | (_, _, (Inlinable | Non_inlinable)) :: _ ->
          denv, extra_params_and_args, false, is_single_use, dacc
    in
    let arg_types_by_use_id =
      List.fold_left (fun args use ->
          List.map2 (fun arg_map arg_type ->
              Apply_cont_rewrite_id.Map.add (U.id use)
                (DE.typing_env (U.env_at_use use), arg_type)
                arg_map)
            args
            (U.arg_types use))
        (List.map (fun _ -> Apply_cont_rewrite_id.Map.empty) t.arity)
        uses
    in
    let result =
      Uses {
        handler_env;
        arg_types_by_use_id;
        extra_params_and_args;
        is_single_inlinable_use;
        is_single_use;
      }
    in
    result, dacc
