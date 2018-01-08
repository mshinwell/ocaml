(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module E = Simplify_env_and_result.Env
module T = Flambda_type

let simplify_name env name =
  let ty = E.type_of_name env name in
  match ty with
  | None ->
    Misc.fatal_errorf "Unbound name %a" Name.print name
  | Some ty ->
    let ty, canonical_name = (E.type_accessor env T.resolve_aliases) ty in
    match canonical_name with
    | None -> name, ty
    | Some canonical_name -> canonical_name, ty

type bounds_check_result =
  | In_range
  | Out_of_range

(* CR mshinwell: This function will also be needed when producing the
   bounds check code when compiling from [Lambda]. *)
let bounds_check ~width ~string_length_in_bytes ~index_in_bytes
      : bounds_check_result =
  let index_in_bytes = Immediate.to_targetint index_in_bytes in
  if Targetint.OCaml.compare index_in_bytes Targetint.OCaml.zero < 0 then
    Out_of_range
  else
    let result_size_in_bytes =
      Targetint.OCaml.of_int
        (Flambda_primitive.byte_width_of_string_accessor_width width)
    in
    (* We are careful here to avoid overflow for ease of reasoning. *)
    let highest_index_allowed =
      Targetint.OCaml.sub string_length_in_bytes result_size_in_bytes
    in
    if Targetint.OCaml.compare index_in_bytes highest_index_allowed >= 0 then
      Out_of_range
    else
      In_range

let all_indexes_out_of_range ~width indexes ~max_string_length =
  Immediate.Set.for_all (fun index_in_bytes ->
      let in_range =
        bounds_check ~width ~string_length_in_bytes:max_string_length
          ~index_in_bytes
      in
      match in_range with
      | Out_of_range -> true
      | In_range -> false)
    indexes

let prepare_to_simplify_set_of_closures ~env
      ~(set_of_closures : Flambda.Set_of_closures.t)
      ~function_decls =


  let free_vars =
    Variable.Map.map (fun (external_var : Flambda.Free_var.t) ->
        let var =
          let var =
            Freshening.apply_variable (E.freshening env) external_var.var
          in
          (* XXX This should use [reify], since there's an advantage if that
             works: removing a free variable. *)
          match
            T.follow_variable_equality (E.find_exn env var)
              ~is_present_in_env:(fun var -> E.mem env var)
          with
          | None -> var
          | Some var -> var
        in
        let ty = E.find_exn env var in
        var, approx)
      set_of_closures.free_vars
  in
  let environment_before_cleaning = env in
  (* [E.local] helps us to catch bugs whereby variables escape their scope. *)
  let env = E.local env in
  let free_vars, function_decls, sb, freshening =
    Freshening.apply_function_decls_and_free_vars (E.freshening env) free_vars
      function_decls ~only_freshen_parameters:(not freshen)
  in
  let env = E.set_freshening env sb in
  let specialised_args =
    Variable.Map.map_keys (Freshening.apply_variable (E.freshening env))
      specialised_args
  in
  let direct_call_surrogates =
    Variable.Map.fold (fun existing surrogate surrogates ->
        let existing =
          Freshening.Project_var.apply_closure_id freshening
            (Closure_id.wrap existing)
        in
        let surrogate =
          Freshening.Project_var.apply_closure_id freshening
            (Closure_id.wrap surrogate)
        in
        assert (not (Closure_id.Map.mem existing surrogates));
        Closure_id.Map.add existing surrogate surrogates)
      set_of_closures.direct_call_surrogates
      Closure_id.Map.empty
  in
  let env =
    E.enter_set_of_closures_declaration env
      function_decls.set_of_closures_origin
  in
  (* we use the previous closure for evaluating the functions *)
  let internal_value_set_of_closures =
    let bound_vars =
      Variable.Map.fold (fun id (_, desc) map ->
          Var_within_closure.Map.add (Var_within_closure.wrap id) desc map)
        free_vars Var_within_closure.Map.empty
    in
    T.create_value_set_of_closures ~function_decls ~bound_vars
      ~invariant_params:(lazy Variable.Map.empty) ~specialised_args
      ~freshening ~direct_call_surrogates
  in
  (* Populate the environment with the approximation of each closure.
     This part of the environment is shared between all of the closures in
     the set of closures. *)
  let set_of_closures_env =
    Variable.Map.fold (fun closure _ env ->
        let approx =
          T.closure ~closure_var:closure
            (Closure_id.Map.singleton (Closure_id.wrap closure)
               internal_value_set_of_closures)
        in
        E.add env closure approx
      )
      function_decls.funs env
  in
  free_vars, specialised_args, function_decls,
    internal_value_set_of_closures, set_of_closures_env

let prepare_to_simplify_closure ~(function_decl : Flambda.Function_declaration.t)
      ~free_vars ~specialised_args ~set_of_closures_env =
  let closure_env =
    Variable.Map.fold (fun inner_var (_outer_var, ty) env ->
        E.add_outer_scope env inner_var ty)
      free_vars set_of_closures_env
  in
  (* CR mshinwell: Freshening?  And cleaning? *)
  List.fold_left (fun env param ->
      let var = Parameter.var param in
      let ty = Parameter.ty param in
      let env = E.add env var ty in
      match T.projection ty with
      | None -> env
      | Some projection ->
        E.add_projection env ~projection ~bound_to:var)
    env function_decl.params
