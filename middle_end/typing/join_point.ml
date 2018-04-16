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

(* CR mshinwell: Use lwhite's suggestion of doing the existential
   introduction at [Switch] time *)
(* XXX Needs documentation.  Change name of [default_env].  Note that
   this environment must contain entries for all parameters of
   the relevant continuation *)
let param_types_and_body_env_opt uses freshening ~default_env =
  match Continuation_uses.uses uses with
  | [] -> None
  | uses ->
    let arity = Flambda.Typed_parameter.List.arity t.params in
    let param_tys =
      List.map (fun param ->
          let var = Flambda.Typed_parameter.var param in
          let var = Freshening.apply_variable freshening var in
          let kind = Flambda.Typed_parameter.kind param in
          T.alias_type_of kind (Name.var var))
        t.params
    in
    let bottom_arg_tys = T.bottom_types_from_arity arity in
    let arg_tys_with_envs_rev, joined_env =
      List.fold_left
        (fun (arg_tys_with_envs_rev, joined_env) (use : Use.t) ->
Format.eprintf "Cutting environment for %a, level %a, freshening is:@ %a\n%!"
Continuation.print t.continuation
Scope_level.print t.definition_scope_level
Freshening.print freshening;
          let use_env =
            TE.cut use.env
              ~existential_if_defined_at_or_later_than:
                (Scope_level.next t.definition_scope_level)
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
                  Variable.print var Scope_level.print scope_level;
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

let param_types_and_body_env uses freshening ~arity ~default_env =
  match param_types_and_body_env_opt uses freshening ~default_env with
  | None -> T.bottom_types_from_arity arity, default_env
  | Some (arg_tys, env) -> arg_tys, env
