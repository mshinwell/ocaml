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

[@@@ocaml.warning "+a-30-40-41-42"]

module DE = Simplify_envs.Downwards_env
module LCS = Simplify_envs.Lifted_constant_state
module T = Flambda_type
module TE = Flambda_type.Typing_env
module U = One_continuation_use

type t = {
  continuation : Continuation.t;
  arity : Flambda_arity.t;
  uses : U.t list;
}

let create continuation arity =
  { continuation;
    arity;
    uses = [];
  }

let print ppf { continuation; arity; uses; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuation@ %a)@]@ \
      @[<hov 1>(arity@ %a)@]@ \
      @[<hov 1>(uses@ %a)@]\
      )@]"
    Continuation.print continuation
    Flambda_arity.print arity
    (Format.pp_print_list ~pp_sep:Format.pp_print_space U.print) uses

let add_use t kind ~env_at_use id ~arg_types =
  try
    let arity = T.arity_of_list arg_types in
    if not (Flambda_arity.equal arity t.arity) then begin
      Misc.fatal_errorf "Arity of use (%a) doesn't match continuation's \
          arity (%a)"
        Flambda_arity.print arity
        Flambda_arity.print t.arity
    end;
    let use = U.create kind ~env_at_use id ~arg_types in
    { t with
      uses = use :: t.uses;
    }
  with Misc.Fatal_error -> begin
    if !Clflags.flambda_context_on_error then begin
      Format.eprintf "\n%sContext is:%s adding use of %a with \
            arg types@ (%a);@ existing uses:@ %a; environment:@ %a"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Continuation.print t.continuation
        (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) arg_types
        print t
        DE.print env_at_use
    end;
    raise Misc.Fatal_error
  end

let number_of_uses t = List.length t.uses

let arity t = t.arity

let get_uses t = t.uses

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

let compute_handler_env t
      ~env_at_fork_plus_params_and_consts
      ~consts_lifted_during_body
      ~params
      ~code_age_relation_after_body : Continuation_env_and_param_types.t =
(*
Format.eprintf "%d uses for %a\n%!"
  (List.length t.uses)
  Continuation.print t.continuation;
*)
  match t.uses with
  | [] -> No_uses
  | uses ->
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
          use_env, U.id use, U.use_kind use)
        uses
    in
(*
Format.eprintf "Unknown at or later than %a\n%!"
  Scope.print (Scope.next definition_scope_level);
*)
    let handler_env, extra_params_and_args, is_single_inlinable_use,
        is_single_use =
      match use_envs_with_ids with
      (* CR mshinwell: This [when] guard must match up with [Simplify_expr]'s
         inlinability criterion.  Remove the duplication. *)
      | [use_env, _, Inlinable]
          when not (Continuation.is_exn t.continuation) ->
        (* We need to make sure any lifted constants generated during the
           simplification of the body are in the environment.  Otherwise
           we might share a constant based on information in [DA] but then
           find the definition of the corresponding constant isn't in [DE].
           Note that some of the constants may already be defined. *)
        let use_env =
          DE.add_lifted_constants ~maybe_already_defined:() use_env
            consts_lifted_during_body
        in
        use_env, Continuation_extra_params_and_args.empty, true, true
      | [] | [_, _, (Inlinable | Non_inlinable)]
      | (_, _, (Inlinable | Non_inlinable)) :: _ ->
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
        let use_envs_with_ids =
          List.map (fun (use_env, id, use_kind) ->
              DE.typing_env use_env, id, use_kind)
            use_envs_with_ids
        in
        let typing_env = DE.typing_env denv in
        let handler_env, extra_params_and_args =
          if not (Flambda_features.join_points ()) then
            let handler_env = simple_join typing_env uses ~params in
            handler_env, Continuation_extra_params_and_args.empty
          else
            let env_extension, extra_params_and_args =
              TE.cut_and_n_way_join typing_env
                use_envs_with_ids
                ~params
                ~unknown_if_defined_at_or_later_than:
                  (Scope.next definition_scope_level)
                ~extra_lifted_consts_in_use_envs
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
              TE.add_env_extension handler_env env_extension
            in
            handler_env, extra_params_and_args
        in
        let handler_env =
          TE.with_code_age_relation handler_env
            code_age_relation_after_body
        in
        let handler_env = DE.with_typing_env denv handler_env in
        let is_single_use =
          match uses with
          | [_] -> true
          | [] | _::_::_ -> false
        in
        match use_envs_with_ids with
        | [_, _, Inlinable] ->
          handler_env, extra_params_and_args, true, true
        | [] | [_, _, Non_inlinable]
        | (_, _, (Inlinable | Non_inlinable)) :: _ ->
          handler_env, extra_params_and_args, false, is_single_use
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
    Uses {
      handler_env;
      arg_types_by_use_id;
      extra_params_and_args;
      is_single_inlinable_use;
      is_single_use;
    }

let get_typing_env_no_more_than_one_use t =
  match t.uses with
  | [] -> None
  | [use] -> Some (DE.typing_env (U.env_at_use use))
  | _::_ ->
    Misc.fatal_errorf "Only zero or one continuation use(s) expected:@ %a"
      print t
