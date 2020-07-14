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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

module SCC_lifted_constants = Strongly_connected_components.Make (CIS)

type result = {
  bindings_outermost_last : LC.t list;
}

let empty_result = {
  bindings_outermost_last = [];
}

let build_dep_graph ~fold_over_lifted_constants =
  fold_over_lifted_constants ~init:(CIS.Map.empty, CIS.Map.empty)
    ~f:(fun (dep_graph, code_id_or_symbol_to_const)
         (lifted_constant, extra_deps) ->
      (*
      Format.eprintf "Input for one set: %a =@ %a\n%!"
        Bound_symbols.print bound_symbols
        Static_const.print defining_expr;
      *)
      let defining_expr = LC.defining_expr lifted_constant in
      let bound_symbols = LC.bound_symbols lifted_constant in
      let types_of_symbols = LC.types_of_symbols lifted_constant in
      let bound_symbols_free_names = Bound_symbols.free_names bound_symbols in
      let free_names_with_envs =
        (* To avoid existing sets of closures (with or without associated
           code) being pulled apart, we add a dependency from each code ID
           or closure symbol being defined to all other code IDs and
           symbols bound by the same binding. *)
        match bound_symbols with
        | Singleton _ ->
          begin match Symbol.Map.get_singleton types_of_symbols with
          | Some (_sym, (denv, _typ)) ->
            [Static_const.free_names defining_expr, Some (DE.typing_env denv)]
          | None ->
            Misc.fatal_errorf "Expected singleton entry in \
                [types_of_symbols] for lifted constant:@ %a"
              LC.print lifted_constant
          end
        | Sets_of_closures bound ->
          let from_bound_symbols =
            (* We never need the environment of definition for symbols or
               code IDs, only for variables (see below). *)
            [bound_symbols_free_names, None]
          in
          let defining_expr =
            Static_const.must_be_sets_of_closures defining_expr
          in
          ListLabels.fold_left2 bound defining_expr
            ~init:from_bound_symbols
            ~f:(fun free_names_with_envs
                    (bound : Bound_symbols.Code_and_set_of_closures.t)
                    (code_and_set_of_closures
                      : Static_const.Code_and_set_of_closures.t) ->
              let closure_symbols = bound.closure_symbols in
              (* We don't need to look at [code_and_set_of_closures.code]
                 since all code IDs therein will also be in
                 [from_bound_symbols], above. *)
              let set_of_closures = code_and_set_of_closures.set_of_closures in
              let free_names = Set_of_closures.free_names set_of_closures in
              Closure_id.Lmap.fold
                (fun _closure_id closure_sym free_names_with_envs ->
                  match Symbol.Map.find closure_sym types_of_symbols with
                  | exception Not_found ->
                    Misc.fatal_errorf "No environment and type given for \
                        closure symbol %a"
                      Symbol.print closure_sym
                  | denv, _typ ->
                    (* All of the closure symbols correspond to the same set
                       of free names (the closure vars' free names in the
                       set of closures). *)
                    (free_names, Some (DE.typing_env denv))
                      :: free_names_with_envs)
                closure_symbols
                free_names_with_envs)
      in
      let free_names_with_envs =
        match extra_deps with
        | None -> free_names_with_envs
        | Some (extra_deps_denv, extra_deps) ->
          (extra_deps, Some (DE.typing_env extra_deps_denv))
            :: free_names_with_envs
      in
      (* Beware: when coming from [Reify_continuation_params] the
         sets of closures may have dependencies on variables that are
         now equal to symbols in the environment.  (They haven't been
         changed to symbols yet as the simplifier hasn't been run on
         the definitions.)  Some of these symbols may be the ones
         involved in the current SCC calculation.  For this latter set,
         we must explicitly add them as dependencies. *)
      let free_syms =
        ListLabels.fold_left free_names_with_envs
          ~init:Symbol.Set.empty
          ~f:(fun free_syms (free_names, typing_env) ->
            Name_occurrences.fold_names free_names
              ~init:free_syms
              ~f:(fun free_syms name ->
                Name.pattern_match name
                  ~var:(fun var ->
                    try
                      let typing_env =
                        match typing_env with
                        | Some typing_env -> typing_env
                        | None -> assert false  (* see above *)
                      in
                      match
                        TE.get_canonical_simple_exn typing_env
                          ~min_name_mode:NM.normal
                          (Simple.var var)
                      with
                      | exception Not_found -> free_syms
                      | canonical ->
                        Simple.pattern_match canonical
                          ~const:(fun _ -> free_syms)
                          ~name:(fun name ->
                            Name.pattern_match name
                              ~var:(fun _var -> free_syms)
                              ~symbol:(fun sym -> Symbol.Set.add sym free_syms))
                    with Misc.Fatal_error -> begin
                      if !Clflags.flambda_context_on_error then begin
                        Format.eprintf "\n%sContext is:%s finding canonical \
                            for %a,@ current constant binding is@ %a =@ %a@ \
                            with free names:@ %a"
                          (Flambda_colours.error ())
                          (Flambda_colours.normal ())
                          Variable.print var
                          Bound_symbols.print bound_symbols
                          Static_const.print defining_expr
                          Name_occurrences.print free_names
                      end;
                      raise Misc.Fatal_error
                    end)
                  ~symbol:(fun sym -> Symbol.Set.add sym free_syms)))
      in
      let free_code_ids =
        ListLabels.fold_left free_names_with_envs ~init:Code_id.Set.empty
          ~f:(fun free_code_ids (free_names, _) ->
            free_names
            |> Name_occurrences.code_ids_and_newer_version_of_code_ids
            |> Code_id.Set.union free_code_ids)
      in
      let deps =
        CIS.Set.union (CIS.set_of_symbol_set free_syms)
          (CIS.set_of_code_id_set free_code_ids)
      in
      CIS.Set.fold
        (fun (being_defined : CIS.t)
             (dep_graph, code_id_or_symbol_to_const) ->
          let dep_graph = CIS.Map.add being_defined deps dep_graph in
          let code_id_or_symbol_to_const =
            CIS.Map.add being_defined
              lifted_constant
              code_id_or_symbol_to_const
          in
          dep_graph, code_id_or_symbol_to_const)
        (Bound_symbols.everything_being_defined bound_symbols)
        (dep_graph, code_id_or_symbol_to_const))

let sort ~fold_over_lifted_constants =
  (* The various lifted constants may exhibit recursion between themselves
     (specifically between closures and/or code).  We use SCC to obtain a
     topological sort of groups that must be coalesced into single
     code-and-set-of-closures definitions. *)
  let lifted_constants_dep_graph, code_id_or_symbol_to_const =
    build_dep_graph ~fold_over_lifted_constants
  in
  (*
  Format.eprintf "SCC graph is:@ %a\n%!"
    (CIS.Map.print CIS.Set.print)
    lifted_constants_dep_graph;
  *)
  let connected_components =
    SCC_lifted_constants.connected_components_sorted_from_roots_to_leaf
      lifted_constants_dep_graph
  in
  let bindings_outermost_last =
    Array.fold_left (fun bindings (group : SCC_lifted_constants.component) ->
        let binding =
          match group with
          | No_loop code_id_or_symbol ->
            CIS.Map.find code_id_or_symbol code_id_or_symbol_to_const
          | Has_loop members ->
            let _, types_of_symbols, sets_of_closures_bound_symbols,
                code_and_sets_of_closures =
              List.fold_left
                (fun ((defining_expr_already_seen,
                       types_of_symbols_acc,
                       sets_of_closures_bound_symbols_acc,
                       code_and_sets_of_closures_acc) as acc)
                     code_id_or_symbol ->
                  if CIS.Set.mem code_id_or_symbol
                       defining_expr_already_seen
                  then acc
                  else
                    let lifted_constant =
                      CIS.Map.find code_id_or_symbol code_id_or_symbol_to_const
                    in
                    let types_of_symbols =
                      LC.types_of_symbols lifted_constant
                    in
                    let defining_expr = LC.defining_expr lifted_constant in
                    let bound_symbols = LC.bound_symbols lifted_constant in
                    (* We may encounter the same defining expression more
                       than once (e.g. a set of closures via a code ID and
                       a symbol), but we don't want duplicates in the result
                       list. *)
                    let defining_expr_already_seen =
                      CIS.Set.union
                        (Bound_symbols.everything_being_defined bound_symbols)
                        defining_expr_already_seen
                    in
                    let sets_of_closures_bound_symbols =
                      match (bound_symbols : Bound_symbols.t) with
                      | Sets_of_closures sets -> sets
                      | Singleton _ ->
                        Misc.fatal_errorf "Code ID or symbol %a was involved@ \
                            in (non-closure) recursion that cannot be compiled"
                          CIS.print code_id_or_symbol
                    in
                    let code_and_set_of_closures_list =
                      Static_const.must_be_sets_of_closures defining_expr
                    in
                    let types_of_symbols =
                      Symbol.Map.disjoint_union types_of_symbols
                        types_of_symbols_acc
                    in
                    defining_expr_already_seen,
                      types_of_symbols,
                      sets_of_closures_bound_symbols
                        @ sets_of_closures_bound_symbols_acc,
                      code_and_set_of_closures_list
                        @ code_and_sets_of_closures_acc)
                (CIS.Set.empty, Symbol.Map.empty, [], [])
                members
            in
            let bound_symbols : Bound_symbols.t =
              Sets_of_closures sets_of_closures_bound_symbols
            in
            let defining_expr : Static_const.t =
              Sets_of_closures code_and_sets_of_closures
            in
            LC.create bound_symbols defining_expr ~types_of_symbols
        in
        binding :: bindings)
      []
      (Array.of_list (List.rev (Array.to_list connected_components)))
  in
  (* By reversing the list we rely on the following property:
       Let the list L be a topological sort of a directed graph G.
       Then the reverse of L is a topological sort of the transpose of G.
  *)
  (*
  Format.eprintf "Result, outermost first:@ %a\n%!"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space LC.print)
    (List.rev bindings_outermost_last);
  *)
  { bindings_outermost_last; }
