(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

module ASA = Augment_specialised_args
module W = ASA.What_to_specialise

module Transform = struct
  let pass_name = "unbox-specialised-args"
  let variable_suffix = "_unbox_spec_args"

  let precondition ~backend ~env ~(set_of_closures : Flambda.set_of_closures) =
    (* !Clflags.unbox_specialised_args *) true
      && not (Variable.Map.is_empty set_of_closures.specialised_args)

  let what_to_specialise ~backend ~env
        ~(set_of_closures : Flambda.set_of_closures) =
    let projections_by_function =
      Variable.Map.filter_map set_of_closures.function_decls.funs
        ~f:(fun _fun_var (function_decl : Flambda.function_declaration) ->
            if function_decl.stub then None
            else
              Extract_projections.from_function_decl ~env ~function_decl
                ~which_variables:set_of_closures.specialised_args)
    in
    (* CR-soon mshinwell: consider caching the Invariant_params *relation*
       as well as the "_in_recursion" map *)
    let invariant_params_flow =
      Invariant_params.invariant_param_sources set_of_closures.function_decls
        ~backend
    in
    (* If for function [f] we would extract a projection expression [e]
       from some specialised argument [x] of [f], and we know from
       [Invariant_params] that a specialised argument [y] of another function
       [g] flows to [x], then add [e] with [y] substituted for [x]
       throughout as a newly-specialised argument for [g].  This should help
       reduce the number of simplification rounds required for
       mutually-recursive functions. *)
    Variable.Map.fold (fun fun_var
              (extraction : Extract_projections.extraction)
              what_to_specialise ->
        let first_fun_var_and_group : W.fun_var_and_group list =
          [{ fun_var;
             group = extraction.group;
          }]
        in
        let fun_vars_and_groups =
          match Variable.Map.find extraction.group invariant_params_flow with
          | exception Not_found -> first_fun_var_and_group
          | flow ->
            Variable.Pair.Set.fold (fun (fun_var, group) fun_vars_and_groups ->
                let fun_var_and_group : W.fun_var_and_group =
                  { fun_var;
                    group;
                  }
                in
                fun_var_and_group :: fun_vars_and_groups)
              flow
              first_fun_var_and_group
        in
        W.new_specialised_arg what_to_specialise
          ~fun_vars_and_groups
          ~defining_expr_in_terms_of_existing_outer_vars:
            extraction.defining_expr_in_terms_of_existing_outer_vars
          ~projection:extraction.projection)
      ~extract_projections_result_by_function
      (W.create ~set_of_closures)
end

include ASA.Make (Transform)
