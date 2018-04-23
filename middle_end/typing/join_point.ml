(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module T = Flambda_type
module TE = Flambda_type.Typing_env
module TEE = Flambda_type.Typing_env_extension

(* CR mshinwell: Move to stdlib *)
let heads_and_tails_of_lists ls =
  let heads = List.map List.hd ls in
  let tails = List.map List.tl ls in
  heads, tails

let param_types_and_body_env_opt cont_uses freshening ~default_env =
  match Continuation_uses.uses cont_uses with
  | [] -> None
  | uses ->
    let scope_level = Continuation_uses.definition_scope_level cont_uses in
    let params = Continuation_uses.params cont_uses in
    let arity = Flambda.Typed_parameter.List.arity params in
    let arg_tys_with_env_extensions, joined_env_extension =
      List.fold_left
        (fun (arg_tys_with_env_extensions, joined_env_extension) use ->
          let use_env = Use.Kind.typing_env use.kind in
          let args_with_tys_this_use = Use.Kind.args_with_tys use.kind in
          let canonical_names_and_resolved_types_for_args =
            List.map (fun ((arg : Simple.t), ty) ->
                let ty, canonical_name = TE.resolve_aliases use_env ty in
                match canonical_name with
                | None ->
                  begin match arg with
                  | Name (Var var) -> Some var, ty
                  | Name (Symbol _) | Const _ -> None, ty
                  end
                | Some (Var var) -> Some var, ty
                | Some (Symbol _) -> None, ty)
              args_with_tys_this_use
          in
          let canonical_names_for_args_to_params =
            List.fold_left (fun acc ((canonical_name, _ty), param) ->
                match canonical_name with
                | None -> acc
                | Some canonical_name ->
                  Variable.Map.add canonical_name param acc)
              Variable.Map.empty
              (List.combine canonical_names_and_resolved_types_for_args params)
          in
          let arg_tys =
            List.map (fun (_canonical_name, ty) ->
                T.rename_variables ty canonical_names_for_args_to_params)
              canonical_names_and_resolved_types_for_args
          in
          let use_env_extension =
            TE.cut (Continuation_uses.Use.typing_env use)
              ~existential_if_defined_at_or_later_than:
                (Scope_level.next scope_level)
          in
          let joined_env_extension =
            match joined_env_extension with
            | None -> use_env_extension
            | Some joined_env_extension ->
              TEE.join default_env TEE.empty TEE.empty
                use_env_extension joined_env_extension
          in
          (arg_tys, use_env_extension) :: arg_tys_with_env_extensions,
            Some joined_env_extension)
        ([], None)
        uses
    in
    let joined_env =
      match joined_env_extension with
      | None -> default_env
      | Some joined_env_extension ->
        TE.add_or_meet_env_extension default_env joined_env_extension
          scope_level
    in
    let arg_tys_with_env_extensions =
      List.map (fun (arg_tys, env_extension) ->
          let env_extension = TE.diff joined_env env_extension in
          arg_tys, env_extension)
        arg_tys_with_env_extensions
    in
    let transpose arg_tys_with_env_extensions =
      match arg_tys_with_env_extensions with
      | [] -> []
      | (arg_tys, env_extension) ->
        let rec inner_loop for_counting =
          match arg_tys with
          | [] -> ...
          | _ :: arg_tys ->
            let heads, tails = heads_and_tails_of_lists arg_tys
        in
        inner_loop arg_tys
    in
    let arg_tys_with_env_extensions_transpose =
      transpose arg_tys_with_env_extensions
    in
    let joined_arg_tys_rev, joined_env =
      List.fold_left
        (* XXX the env extension differs for each arg *)
        (fun (joined_arg_tys, joined_env) (param, (arg_tys, env_extension)) ->
          let joined_ty =
            List.fold_left (fun joined_ty ty ->
                let free_names_this_ty =
                  TEE.free_names_transitive env_extension ty
                in
                let env_extension =
                  TEE.restrict_names_to_those_occurring_in_types
                    env_extension [ty]
                in
                let joined_ty =
                  try T.join joined_env TEE.empty env_extension joined_ty ty
                  with Misc.Fatal_error -> begin
                    Format.eprintf "\n%sContext is: parameter %a%s\n"
                      (Misc_color.bold_red ())
                      Parameter.print param
                      (Misc_color.reset ());
                    raise Misc.Fatal_error
                  end
                in
                join_ty :: joined_arg_tys, joined_env)
              joined_arg_tys
              arg_tys)
          in
          let joined_env =
            TE.add joined_env (Parameter.name param) scope_level
              (Definition joined_ty)
          in
          joined_ty :: joined_arg_tys, joined_env)
        bottom_arg_tys
        (List.combine params arg_tys_with_env_extensions)
    in
    Some (List.rev joined_arg_tys, joined_env)

let param_types_and_body_env uses freshening ~arity ~default_env =
  match param_types_and_body_env_opt uses freshening ~default_env with
  | None -> T.bottom_types_from_arity arity, default_env
  | Some (arg_tys, env) -> arg_tys, env

(*
(* CR mshinwell: Use lwhite's suggestion of doing the existential
   introduction at [Switch] time *)
let param_types_and_body_env_opt cont_uses freshening ~default_env =
  match Continuation_uses.uses cont_uses with
  | [] -> None
  | uses ->
    let params = Continuation_uses.params cont_uses in
    let arity = Flambda.Typed_parameter.List.arity params in
    let param_tys =
      List.map (fun param ->
          let var = Flambda.Typed_parameter.var param in
          let var = Freshening.apply_variable freshening var in
          let kind = Flambda.Typed_parameter.kind param in
          T.alias_type_of kind (Name.var var))
        params
    in
    let bottom_arg_tys = T.bottom_types_from_arity arity in
    let arg_tys_with_envs_rev, joined_env =
      List.fold_left
        (fun (arg_tys_with_envs_rev, joined_env) use ->
Format.eprintf "Cutting environment for %a, level %a, freshening is:@ %a\n%!"
Continuation.print (Continuation_uses.continuation cont_uses)
Scope_level.print (Continuation_uses.definition_scope_level cont_uses)
Freshening.print freshening;
          let use_env_extension =
            TE.cut (Continuation_uses.Use.typing_env use)
              ~existential_if_defined_at_or_later_than:
                (Scope_level.next (
                  Continuation_uses.definition_scope_level cont_uses))
          in
(*
Format.eprintf "...result of cut is %a\n%!" TE.print this_env;
*)
          (* XXX This is kind of gross, and is putting out-of-scope 
             variables in the use environments. *)
          let use_env =
            List.fold_left (fun use_env param ->
                let var = Flambda.Typed_parameter.var param in
                let var = Freshening.apply_variable freshening var in
                let name = Name.var var in
                let ty, scope_level, _binding_type =
                  TE.find_with_scope_level default_env name
                in
                Format.eprintf "Copying type for param %a level %a\n%!"
                  Variable.print var
                  Scope_level.With_sublevel.print scope_level;
                TE.add_or_replace_meet use_env name scope_level ty)
              use_env
              t.params
          in
          let arg_tys_this_use = Use.Kind.arg_tys use.kind in
          (* CR mshinwell: Add [List.map2i]. *)
          let use_env, arg_tys_this_use_rev =
            List.fold_left2
              (fun (use_env, arg_tys_this_use_rev) param_ty arg_ty ->
                let arg_ty, new_env_extension =
                  T.meet ~bias_towards:(use_env, param_ty)
                    (use_env, arg_ty)
                in
Format.eprintf "meet new env_extension:@ %a\n%!"
T.Typing_env_extension.print new_env_extension;
                let use_env = TE.add_env_extension use_env new_env_extension in
Format.eprintf "New use_env after meet:@ %a\n%!"
TE.print use_env;
                use_env, arg_ty :: arg_tys_this_use_rev)
              (use_env, [])
              param_tys
              arg_tys_this_use
          in
          let arg_tys_this_use = List.rev arg_tys_this_use_rev in
          let joined_env =
            match joined_env with
            | None -> use_env
            | Some joined_env -> TE.join joined_env use_env
          in
          (arg_tys_this_use, use_env) :: arg_tys_with_envs_rev,
            Some joined_env)
        ([], None)
        uses
    in
    let joined_env =
      match joined_env with
      | None -> default_env
      | Some joined_env -> joined_env
    in
Format.eprintf "The joined environment for %a is:@ %a\n%!"
Continuation.print t.continuation
TE.print joined_env;
    let joined_arg_tys =
      List.fold_left (fun joined_arg_tys (arg_tys, use_env) ->
          let arg_number = ref 0 in
          List.map2 (fun joined_ty (param, this_ty) ->
              let free_names_this_ty =
                TE.free_names_transitive use_env this_ty
              in
              Format.eprintf "Argument for %a:@ Type:@ %a@ \
                  Free names:@ %a@ Env:@ %a\n%!"
                Continuation.print t.continuation
                T.print this_ty
                Name_occurrences.print free_names_this_ty
                TE.print use_env;
              (* XXX We should presumably allow things from outer
                 levels so long as our types for them are more
                 precise. *)
              let restricted_use_env =
                TE.restrict_names_to_those_occurring_in_types use_env
                  [this_ty]
              in
              Format.eprintf "Restricted env before diff:@ %a\n%!"
                TE.print restricted_use_env;
              let env_extension = TE.diff restricted_use_env joined_env in
              Format.eprintf "Typing_env_extension after diff:@ %a\n%!"
                T.Typing_env_extension.print env_extension;
              (* XXX should take into account aliases of [param] too *)
              let param = Flambda.Typed_parameter.name param in
              let env_extension =
                T.Typing_env_extension.remove env_extension param
              in
              Format.eprintf "Final env_extension:@ %a\n%!"
                T.Typing_env_extension.print env_extension;
              let this_ty =
                T.add_env_extension (joined_env, this_ty) env_extension
              in
              let joined_ty =
                try T.join (joined_env, joined_ty) (use_env, this_ty)
                with Misc.Fatal_error -> begin
                  Format.eprintf "\n%sContext is: argument number %d \
                      (0 is the first argument)%s\n"
                    (Misc_color.bold_red ())
                    !arg_number
                    (Misc_color.reset ());
                  raise Misc.Fatal_error
                end
              in
              incr arg_number;
              joined_ty)
            joined_arg_tys (List.combine t.params arg_tys))
        bottom_arg_tys
        (List.rev arg_tys_with_envs_rev)
    in
Format.eprintf "The joined arg tys for %a are:@ %a\n%!"
Continuation.print t.continuation
(Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) joined_arg_tys;
(*
    let free_names_in_joined_arg_tys =
      List.fold_left (fun free_names_in_joined_arg_tys joined_arg_ty ->
          let names = T.free_names_transitive joined_env joined_arg_ty in
          Name_occurrences.union names free_names_in_joined_arg_tys)
        (Name_occurrences.create ())
        joined_arg_tys
    in
    let joined_env =
      TE.restrict_to_names joined_env
        (Name_occurrences.union free_names_in_joined_arg_tys
          (TE.domain joined_env))
    in
*)
    Some (joined_arg_tys, joined_env)
*)
