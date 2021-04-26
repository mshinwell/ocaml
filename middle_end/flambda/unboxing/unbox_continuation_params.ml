(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

open Unboxing_types

type nonrec decisions = decisions

let print = print

let update_decision ~pass
      rewrite_ids_already_seen nth_arg arg_type_by_use_id = function
  | Do_not_unbox _ as decision -> decision
  | decision ->
    Apply_cont_rewrite_id.Map.fold
      (fun rewrite_id
           (arg_at_use : Continuation_env_and_param_types.arg_at_use)
           decision ->
         if Apply_cont_rewrite_id.Set.mem rewrite_id rewrite_ids_already_seen
         then decision
         else begin
           let typing_env_at_use = arg_at_use.typing_env in
           let arg_type_at_use = arg_at_use.arg_type in
           match TE.get_alias_then_canonical_simple_exn typing_env_at_use
                   ~min_name_mode:Name_mode.normal arg_type_at_use with
           | simple ->
             Unboxing_epa.compute_extra_args_for_one_decision_and_use
               ~pass rewrite_id ~typing_env_at_use
               (Available simple) decision
           | exception Not_found ->
             Unboxing_epa.compute_extra_args_for_one_decision_and_use
               ~pass rewrite_id ~typing_env_at_use
               (Added_by_wrapper_at_rewrite_use { nth_arg; }) decision
         end
      ) arg_type_by_use_id decision

let make_decisions
      ~continuation_is_recursive
      ~arg_types_by_use_id
      denv params params_types : DE.t * decisions =
  let empty = Apply_cont_rewrite_id.Set.empty in
  let _, denv, rev_decisions, seen =
    Misc.Stdlib.List.fold_left3
      (fun (nth, denv, rev_decisions, seen) param param_type
           arg_type_by_use_id ->
         (* Make an optimist decision, filter it based on the arg types at the
            use sites (to prevent decisions that would be detrimental),
            and compute the necessary denv. *)
         let decision =
           Optimistic_unboxing_decision.make_optimistic_decision ~depth:0
             (DE.typing_env denv) ~param_type
           |> update_decision empty nth arg_type_by_use_id
                ~pass:(Filter { recursive = continuation_is_recursive; })
           |> Is_unboxing_beneficial.filter_non_beneficial_decisions
         in
         let denv =
           Build_unboxing_denv.denv_of_decision denv ~param_var:(KP.var param)
             decision
         in
         (* Compute the set of rewrite ids that have been considered when
            updating decisions, and check that all arg_type_by_use_id cover
            the same set of rewrite ids. *)
         let seen =
           match seen with
           | Some s ->
             assert (Apply_cont_rewrite_id.Map.for_all (fun id _ ->
               Apply_cont_rewrite_id.Set.mem id s) arg_type_by_use_id);
             s
           | None ->
             Apply_cont_rewrite_id.Map.fold (fun id _ acc ->
               Apply_cont_rewrite_id.Set.add id acc
             ) arg_type_by_use_id empty
         in
         (nth + 1, denv, decision :: rev_decisions, Some seen)
      ) (0, denv, [], None) params params_types arg_types_by_use_id
  in
  let rewrite_ids_seen =
    match seen with
    | None -> empty
    | Some s -> s
  in
  let decisions = List.combine params (List.rev rev_decisions) in
  denv, { decisions; rewrite_ids_seen; }

let compute_extra_params_and_args { decisions; rewrite_ids_seen; }
      ~arg_types_by_use_id existing_extra_params_and_args =
  let _, extra_params_and_args =
    List.fold_left2
      (fun (nth, extra_params_and_args) arg_type_by_use_id (_, decision) ->
         let decision =
           update_decision ~pass:Compute_all_extra_args
             rewrite_ids_seen nth arg_type_by_use_id decision
         in
         let extra_params_and_args =
           Unboxing_epa.add_extra_params_and_args
             extra_params_and_args decision
         in
         (nth + 1, extra_params_and_args)
      ) (0, existing_extra_params_and_args) arg_types_by_use_id decisions
  in
  extra_params_and_args
