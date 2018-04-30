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
    Format.eprintf ">>> param_types_and_body_env_opt %a, params %a\n%!"
      Continuation.print (Continuation_uses.continuation cont_uses)
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Flambda.Typed_parameter.print) params;
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
                  let param = Flambda.Typed_parameter.name param in
                  Name.Map.add (Name.var canonical_name) param acc)
              Name.Map.empty
              (List.combine canonical_names_and_resolved_types_for_args params)
          in
          let arg_tys =
            List.map (fun (_canonical_name, ty) ->
                T.rename_variables ty canonical_names_for_args_to_params)
              canonical_names_and_resolved_types_for_args
          in
Format.eprintf "Canonical names and resolved arg types:@ %a\n%!"
  (Format.pp_print_list ~pp_sep:Format.pp_print_space
    (fun ppf ((canonical_name, _ty), ty) ->
      Format.fprintf ppf "@[%a %a@]"
        (Misc.Stdlib.Option.print Variable.print) canonical_name
        T.print ty))
  (List.combine canonical_names_and_resolved_types_for_args arg_tys);
          let use_env = Continuation_uses.Use.typing_env use in
          let use_env =
            List.fold_left (fun use_env param ->
                (* XXX For recursive continuations the params should already
                   be in the env *)
                TE.add use_env (Flambda.Typed_parameter.name param)
                  (Scope_level.next scope_level)
                  (Definition (Flambda.Typed_parameter.ty param)))
              use_env
              params
          in
          let use_env_extension =
            TE.cut use_env
              ~existential_if_defined_at_or_later_than:
                (Scope_level.next scope_level)
          in
Format.eprintf "Cutting environment so existential at or later than \
    %a:@ %a@ Extension is:@ %a\n%!"
  Scope_level.print (Scope_level.next scope_level)
  TE.print use_env
  TEE.print use_env_extension;
          let joined_env_extension =
            match joined_env_extension with
            | None -> use_env_extension
            | Some joined_env_extension ->
              TEE.join default_env TEE.empty TEE.empty
                use_env_extension joined_env_extension
          in
          (arg_tys, use_env, use_env_extension) :: arg_tys_with_env_extensions,
            Some joined_env_extension)
        ([], None)
        uses
    in
Format.eprintf "Joined env extension is:@ %a@ default_env:@ %a\n%!"
  (Misc.Stdlib.Option.print TEE.print) joined_env_extension
  TE.print default_env;
    let joined_env, opening_existentials_freshening =
      match joined_env_extension with
      | None -> default_env, Name.Map.empty
      | Some joined_env_extension ->
        TE.add_or_meet_env_extension' default_env joined_env_extension
          (TE.max_level default_env)
    in
Format.eprintf "Joined env before diffing is:@ %a\n%!"
  TE.print joined_env;
    let arg_tys_with_env_extensions =
      List.map (fun (arg_tys, use_env, env_extension) ->
          let env_extension =
            match uses with
            | [_] -> TEE.empty
            | _ -> TEE.diff env_extension joined_env
          in
Format.eprintf "Diffed env extension:@ %a\n%!" TEE.print env_extension;
          let arg_tys =
            List.map (fun arg_ty ->
                match uses with
                | [_] ->
                  let freshened_arg_ty =
                    T.rename_variables arg_ty opening_existentials_freshening
                  in
(* XXX This is still restricting too much
   ...ah, it's because TE.domain isn't right for the CSE ones.  It's probably
   TE.domain for non-CSE ones and then the names on the right-hand side of the
   CSE ones
                  let env_extension =
                    (* CR mshinwell: rename this function *)
                    TEE.restrict_names_to_those_occurring_in_types
                      env_extension joined_env default_env [freshened_arg_ty]
                  in
*)
Format.eprintf "(1) Restricted env extension:@ %a\n%!" TEE.print env_extension;
                  freshened_arg_ty, env_extension
                | _ ->
                  assert (Name.Map.is_empty opening_existentials_freshening);
Format.eprintf "Use env, for case 2, is:@ %a\n%!"
  TE.print use_env;
(*
                  let env_extension =
                    TEE.restrict_names_to_those_occurring_in_types
                      env_extension use_env default_env [arg_ty]
                  in
*)
Format.eprintf "(2) Restricted env extension:@ %a\n%!" TEE.print env_extension;
                  arg_ty, env_extension)
              arg_tys
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
            List.fold_left
              (fun joined_ty (arg_ty, env_extension) ->
(*
Format.eprintf "Final use env extension for arg ty %a: %a\n%!"
  T.print arg_ty
  TEE.print env_extension;
*)
                try
let ty =
                  T.join joined_env TEE.empty env_extension joined_ty arg_ty
in
Format.eprintf "Joining@ JT %a with@ AT %a in@ TEE %a -->@ %a\n%!"
  T.print joined_ty
  T.print arg_ty
  TEE.print env_extension
  T.print ty;
ty
                with Misc.Fatal_error -> begin
                  Format.eprintf "\n%sContext is: parameter %a%s@ in@ %a\n"
                    (Misc_color.bold_red ())
                    Flambda.Typed_parameter.print param
                    (Misc_color.reset ())
                    TE.print joined_env;
                  raise Misc.Fatal_error
                end)
              bottom_ty
              all_uses_for_arg_with_env_extensions
          in
          let joined_env =
            TE.add_equation joined_env (Flambda.Typed_parameter.name param)
              (Scope_level.next scope_level) joined_ty
          in
          joined_ty :: joined_arg_tys, joined_env)
        ([], joined_env)
        (List.combine params (
          List.combine bottom_arg_tys arg_tys_with_env_extensions))
    in
    Some (List.rev joined_arg_tys_rev, joined_env, joined_env_extension)

let param_types_and_body_env uses freshening ~arity ~default_env =
  let arg_tys, env, env_extension =
    match param_types_and_body_env_opt uses freshening ~default_env with
    | None -> T.bottom_types_from_arity arity, default_env, TEE.empty
    | Some (arg_tys, env, env_extension) ->
      match env_extension with
      | None -> arg_tys, env, TEE.empty
      | Some env_extension -> arg_tys, env, env_extension
  in
  Format.eprintf "Final argument types for %a are:@ %a\n%!"
    Continuation.print (Continuation_uses.continuation uses)
    (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) arg_tys;
  arg_tys, env, env_extension
