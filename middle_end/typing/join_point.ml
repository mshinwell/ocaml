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

(* CR-soon mshinwell: Consider lwhite's suggestion of doing the existential
   introduction at [Switch] time *)

(* XXX Need to work out what to do re. freshening *)
let param_types_and_body_env_opt cont_uses _freshening ~default_env =
  match Continuation_uses.uses cont_uses with
  | [] -> None
  | uses ->
    let scope_level = Continuation_uses.definition_scope_level cont_uses in
    let params = Continuation_uses.params cont_uses in
    let arity = Flambda.Typed_parameter.List.arity params in
    let arg_tys_with_env_extensions, joined_env_extension =
      List.fold_left
        (fun (arg_tys_with_env_extensions, joined_env_extension) use ->
          let use_env = Continuation_uses.Use.typing_env use in
          let args_with_tys_this_use =
            Continuation_uses.Use.args_with_tys use
          in
          let canonical_names_and_resolved_types_for_args =
            List.map (fun ((arg : Simple.t option), ty) ->
                let ty, canonical_name = TE.resolve_aliases (use_env, ty) in
                match canonical_name with
                | None ->
                  begin match arg with
                  | None -> None, ty
                  | Some (Name (Var var)) -> Some var, ty
                  | Some (Name (Symbol _))
                  | Some (Const _)
                  | Some (Discriminant _) -> None, ty
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
                  let param = Flambda.Typed_parameter.var param in
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
          let env_extension = TEE.diff env_extension joined_env in
          let arg_tys =
            List.map (fun arg_ty -> arg_ty, env_extension) arg_tys
          in
          arg_tys)
        arg_tys_with_env_extensions
    in
    let arg_tys_with_env_extensions =
      Misc.Stdlib.List.transpose arg_tys_with_env_extensions
    in
    let bottom_arg_tys = T.bottom_types_from_arity arity in
    let joined_arg_tys_rev, joined_env =
      List.fold_left
        (fun (joined_arg_tys, joined_env)
             (param, (bottom_ty, all_uses_for_arg_with_env_extensions)) ->
          let joined_ty =
            List.fold_left (fun joined_ty (arg_ty, env_extension) ->
                let env_extension =
                  TEE.restrict_names_to_those_occurring_in_types
                    env_extension joined_env [arg_ty]
                in
                try
                  T.join joined_env TEE.empty env_extension joined_ty arg_ty
                with Misc.Fatal_error -> begin
                  Format.eprintf "\n%sContext is: parameter %a%s\n"
                    (Misc_color.bold_red ())
                    Flambda.Typed_parameter.print param
                    (Misc_color.reset ());
                  raise Misc.Fatal_error
                end)
              bottom_ty
              all_uses_for_arg_with_env_extensions
          in
          let joined_env =
            TE.add joined_env (Flambda.Typed_parameter.name param) scope_level
              (Definition joined_ty)
          in
          joined_ty :: joined_arg_tys, joined_env)
        ([], joined_env)
        (List.combine params (
          List.combine bottom_arg_tys arg_tys_with_env_extensions))
    in
    Some (List.rev joined_arg_tys_rev, joined_env, joined_env_extension)

let param_types_and_body_env uses freshening ~arity ~default_env =
  match param_types_and_body_env_opt uses freshening ~default_env with
  | None -> T.bottom_types_from_arity arity, default_env, TEE.empty
  | Some (arg_tys, env, env_extension) ->
    match env_extension with
    | None -> arg_tys, env, TEE.empty
    | Some env_extension -> arg_tys, env, env_extension

(*
Format.eprintf "Cutting environment for %a, level %a, freshening is:@ %a\n%!"
Continuation.print (Continuation_uses.continuation cont_uses)
Scope_level.print (Continuation_uses.definition_scope_level cont_uses)
Freshening.print freshening;
*)
(*
Format.eprintf "...result of cut is %a\n%!" TE.print this_env;
*)
(*
Format.eprintf "The joined environment for %a is:@ %a\n%!"
Continuation.print t.continuation
TE.print joined_env
*)
(*
Format.eprintf "The joined arg tys for %a are:@ %a\n%!"
Continuation.print t.continuation
(Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) joined_arg_tys;
*)
