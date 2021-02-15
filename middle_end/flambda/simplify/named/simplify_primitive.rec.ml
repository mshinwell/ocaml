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

type cse_result =
  | Invalid of T.t
  | Applied of (Simplified_named.t * TEE.t * Simple.t list * DE.t)
  | Not_applied of DE.t

let apply_cse denv ~original_prim =
  match P.Eligible_for_cse.create original_prim with
  | None -> None
  | Some with_fixed_value ->
    match DE.find_cse denv with_fixed_value with
    | None -> None
    | Some simple ->
      (* CR mshinwell: Is this missing a [min_name_mode] constraint? *)
      match TE.get_canonical_simple_exn (DE.typing_env denv) simple with
      | exception Not_found -> None
      | simple -> Some simple

let try_cse denv ~original_prim ~simplified_args:args ~min_name_mode
    ~result_var : cse_result =
  (* CR-someday mshinwell: Use [meet] and [reify] for CSE?  (discuss with
     lwhite) *)
  (* CR-someday mshinwell: Find example that suggested we needed to allow
     In_types name mode for CSE primitive arguments. *)
  if not (Name_mode.equal min_name_mode Name_mode.normal) then Not_applied denv
  else
    let result_var = VB.var result_var in
    match apply_cse denv ~original_prim with
    | Some replace_with ->
      let named = Named.create_simple replace_with in
      let ty = T.alias_type_of (P.result_kind original_prim) replace_with in
      let env_extension = TEE.one_equation (Name.var result_var) ty in
      Applied (Simplified_named.reachable named, env_extension, args, denv)
    | None ->
      let denv =
        match P.Eligible_for_cse.create original_prim with
        | None -> denv
        | Some eligible_prim ->
          let bound_to = Simple.var result_var in
          DE.add_cse denv eligible_prim ~bound_to
      in
      Not_applied denv

let simplify_primitive dacc ~original_named (prim : P.t) dbg ~result_var =
  let min_name_mode = Var_in_binding_pos.name_mode result_var in
  let result_var' = Var_in_binding_pos.var result_var in
  let args_rev, found_invalid =
    ListLabels.fold_left (P.args prim)
      ~init:([], false)
      ~f:(fun args_rev arg ->
        match S.simplify_simple denv arg ~min_name_mode with
        | Bottom, arg_ty -> (arg, arg_ty) :: args_rev, true
        | Ok arg, arg_ty -> (arg, arg_ty) :: args_rev, found_invalid)
  in
  let args = List.rev args_rev in
  if found_invalid then
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Simplified_named.invalid (), env_extension, args, denv
  else
    match
      try_cse denv ~original_prim ~simplified_args ~min_name_mode
        ~result_var:result_var'
    with
    | Invalid ty ->
      let env_extension = TEE.one_equation (Name.var result_var') ty in
      Simplified_named.invalid (), env_extension, args, denv
    | Applied result -> result
    | Not_applied denv ->
      let simplified_named, env_extension, args, denv =
        match prim, args with
        | Unary (prim, _), [arg, arg_ty] ->
          Simplify_unary_primitive.simplify_unary_primitive denv
            prim ~arg ~arg_ty dbg ~result_var
        | Binary (prim, _, _), [arg1, arg1_ty; arg2, arg2_ty] ->
          Simplify_binary_primitive.simplify_binary_primitive denv
            prim ~arg1 ~arg1_ty ~arg2 ~arg2_ty dbg ~result_var
        | Ternary (prim, _, _), [arg1, arg1_ty; arg2, arg2_ty; arg3, arg3_ty] ->
          Simplify_ternary_primitive.simplify_ternary_primitive denv
            prim ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~arg3 ~arg3_ty dbg ~result_var
        | Variadic (variadic_prim, _), args_with_tys ->
          Simplify_variadic_primitive.simplify_variadic_primitive denv
            ~args_with_tys dbg ~result_var
        | (Unary _ | Binary _ | Ternary _ | Variadic _), ([] | _::_) ->
          Misc.fatal_errorf "Mismatch between primitive %a and args %a"
            P.print prim
            Simple.List.print (List.map fst args)
      in
      let closure_vars = DE.closure_var_uses denv in
      let dacc =
        denv
        |> DE.without_closure_var_uses
        |> DA.with_denv dacc
        |> DA.add_uses_of_closure_vars ~closure_vars
      in
      simplified_named, env_extension, args, dacc
