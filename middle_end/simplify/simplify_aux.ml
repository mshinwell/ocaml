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
  ignore set_of_closures;
  let env = E.local env in
  let function_decls, freshening =
    Flambda.Function_declarations.freshen function_decls (E.freshening env)
  in
  let env =
    E.enter_set_of_closures_declaration (E.set_freshening env freshening)
      function_decls.set_of_closures_origin
  in
  let set_of_closures_ty =
    T.unknown (Flambda_kind.value ())
(* XXX to fix.  Old code:
    let bound_vars =
      Variable.Map.fold (fun id (_, desc) map ->
          Var_within_closure.Map.add (Var_within_closure.wrap id) desc map)
        free_vars Var_within_closure.Map.empty
    in
    T.create_value_set_of_closures ~function_decls ~bound_vars
      ~invariant_params:(lazy Variable.Map.empty) ~specialised_args
      ~freshening ~direct_call_surrogates
*)
  in
(*
  let set_of_closures_env =
    Closure_id.Map.fold (fun closure _ env ->
        let closure_ty =
          T.unknown (Flambda_kind.value Definitely_pointer)
(* XXX to fix.  Old code:
          T.closure ~closure_var:closure
            (Closure_id.Map.singleton (Closure_id.wrap closure)
               internal_value_set_of_closures)
*)
        in
        E.add_variable env closure closure_ty)
      function_decls.funs env
  in
*)
  let set_of_closures_env = env in
  function_decls, set_of_closures_ty, set_of_closures_env

let prepare_to_simplify_closure
      ~(function_decl : Flambda.Function_declaration.t)
      ~set_of_closures_env =
(*
  let closure_env =
    Variable.Map.fold (fun inner_var (_outer_var, ty) env ->
        E.add_outer_scope env inner_var ty)
      free_vars set_of_closures_env
  in
*)
  let my_closure =
    Freshening.apply_variable (E.freshening set_of_closures_env)
      function_decl.my_closure
  in
  (* XXX This level handling is messy.  This is to line up with handling
     of toplevel expressions *)
  let env = E.decrement_continuation_scope_level set_of_closures_env in
  let env =
    List.fold_left (fun env (param : Flambda.Typed_parameter.t) ->
        let var = Flambda.Typed_parameter.var param in
        let ty = Flambda.Typed_parameter.ty param in
        E.add_variable env var ty)
      env function_decl.params
  in
(*
  let env = E.increment_continuation_scope_level env in
*)
  (* XXX use the correct my_closure type. *)
  let env =
    E.add_variable env my_closure
      (T.unknown (Flambda_kind.value ()))
  in
  E.increment_continuation_scope_level env

let initial_inlining_threshold ~round : Inlining_cost.Threshold.t =
  let unscaled =
    Clflags.Float_arg_helper.get ~key:round !Clflags.inline_threshold
  in
  (* CR-soon pchambart: Add a warning if this is too big
     mshinwell: later *)
  Can_inline_if_no_larger_than
    (int_of_float
      (unscaled *. float_of_int Inlining_cost.scale_inline_threshold_by))

let initial_inlining_toplevel_threshold ~round : Inlining_cost.Threshold.t =
  let ordinary_threshold =
    Clflags.Float_arg_helper.get ~key:round !Clflags.inline_threshold
  in
  let toplevel_threshold =
    Clflags.Int_arg_helper.get ~key:round !Clflags.inline_toplevel_threshold
  in
  let unscaled =
    (int_of_float ordinary_threshold) + toplevel_threshold
  in
  (* CR-soon pchambart: Add a warning if this is too big
     mshinwell: later *)
  Can_inline_if_no_larger_than
    (unscaled * Inlining_cost.scale_inline_threshold_by)

let params_for_exception_handler () =
  let param = Parameter.wrap (Variable.create "exn") in
  let ty = T.any_value () in
  [Flambda.Typed_parameter.create param ty]
