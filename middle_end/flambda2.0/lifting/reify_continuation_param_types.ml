(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

let lift_non_closure_discovered_via_reified_continuation_param_types dacc
      var to_lift ~reified_continuation_params_to_symbols
      ~reified_definitions ~closure_symbols_by_set =
  let static_const = Reification.create_static_const to_lift in
  match R.find_shareable_constant (DA.r dacc) static_const with
  | Some symbol ->
    let reified_continuation_params_to_symbols =
      Variable.Map.add var symbol reified_continuation_params_to_symbols
    in
    dacc, reified_continuation_params_to_symbols, reified_definitions,
      closure_symbols_by_set
  | None ->
    let symbol =
      Symbol.create (Compilation_unit.get_current_exn ())
        (Linkage_name.create (Variable.unique_name var))
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        DE.add_equation_on_name (DE.define_symbol denv symbol K.value)
          (Name.var var)
          (T.alias_type_of K.value (Simple.symbol symbol)))
    in
    let dacc =
      DA.map_r dacc ~f:(fun r ->
        R.consider_constant_for_sharing r symbol static_const)
    in
    let reified_definitions =
      (Let_symbol.Bound_symbols.Singleton symbol, static_const)
        :: reified_definitions
    in
    let reified_continuation_params_to_symbols =
      Variable.Map.add var symbol reified_continuation_params_to_symbols
    in
    dacc, reified_continuation_params_to_symbols, reified_definitions,
      closure_symbols_by_set

let lift_set_of_closures_discovered_via_reified_continuation_param_types dacc
      var closure_id function_decls ~closure_vars
      ~reified_continuation_params_to_symbols
      ~reified_definitions ~closure_symbols_by_set =
  let module I = T.Function_declaration_type.Inlinable in
  let set_of_closures =
    let function_decls =
      Closure_id.Map.map (fun inlinable ->
        Function_declaration.create ~code_id:(I.code_id inlinable)
          ~params_arity:(I.param_arity inlinable)
          ~result_arity:(I.result_arity inlinable)
          ~stub:(I.stub inlinable)
          ~dbg:(I.dbg inlinable)
          ~inline:(I.inline inlinable)
          ~is_a_functor:(I.is_a_functor inlinable)
          ~recursive:(I.recursive inlinable))
        function_decls
      |> Function_declarations.create
    in
    Set_of_closures.create function_decls ~closure_elements:closure_vars
  in
  let bind_continuation_param_to_symbol dacc ~closure_symbols =
    let dacc, symbol =
      DA.map_denv2 dacc ~f:(fun denv ->
        match Closure_id.Map.find closure_id closure_symbols with
        | exception Not_found ->
          Misc.fatal_errorf "Variable %a claimed to hold closure with \
              closure ID %a, but no symbol was found for that closure ID"
            Variable.print var
            Closure_id.print closure_id
        | symbol ->
          let denv =
            DE.add_equation_on_name denv (Name.var var)
              (T.alias_type_of K.value (Simple.symbol symbol))
          in
          denv, symbol)
    in
    dacc, Variable.Map.add var symbol reified_continuation_params_to_symbols
  in
  match Set_of_closures.Map.find set_of_closures closure_symbols_by_set with
  | exception Not_found ->
    let closure_symbols =
      Closure_id.Map.mapi (fun closure_id _function_decl ->
          (* CR mshinwell: share name computation with
             [Simplify_named] *)
          let name =
            closure_id
            |> Closure_id.rename
            |> Closure_id.to_string
            |> Linkage_name.create 
          in
          Symbol.create (Compilation_unit.get_current_exn ()) name)
        function_decls
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        Closure_id.Map.fold (fun _closure_id closure_symbol denv ->
            DE.define_symbol denv closure_symbol K.value)
          closure_symbols
          denv)
    in
    let definition =
      (* We don't need to assign new code IDs, since we're not changing the
         code. The code will actually be re-simplified (when we reach the new
         [Let_symbol] bindings)---at that point, new code IDs may well be
         assigned. (That is also the point at which references to the closures
         being lifted, via the continuation's parameters, will be changed to go
         via symbols.) *)
      Let_symbol.pieces_of_code
        ~newer_versions_of:Code_id.Map.empty
        ~set_of_closures:(closure_symbols, set_of_closures)
        Code_id.Map.empty
    in
    let reified_definitions = definition :: reified_definitions in
    let closure_symbols_by_set =
      Set_of_closures.Map.add set_of_closures closure_symbols
        closure_symbols_by_set
    in
    let dacc, reified_continuation_params_to_symbols =
      bind_continuation_param_to_symbol dacc ~closure_symbols
    in
    dacc, reified_continuation_params_to_symbols, reified_definitions,
      closure_symbols_by_set
  | closure_symbols ->
    let dacc, reified_continuation_params_to_symbols =
      bind_continuation_param_to_symbol dacc ~closure_symbols
    in
    dacc, reified_continuation_params_to_symbols, reified_definitions,
      closure_symbols_by_set

let reify_types_of_continuation_param_types dacc ~params =
  let orig_typing_env = DE.typing_env (DA.denv dacc) in
  Variable.Set.fold
    (fun var (dacc, reified_continuation_params_to_symbols, reified_definitions,
              closure_symbols_by_set) ->
      let ty = TE.find orig_typing_env (Name.var var) in
      let existing_symbol =
        (* We must avoid attempting to create aliases between symbols or
           (equivalently) defining static parts that already have symbols.
           This could happen if [var] is actually equal to another of the
           continuation's parameters. *)
        TE.get_canonical_simple (DE.typing_env (DA.denv dacc))
          ~min_name_mode:NM.normal (Simple.var var)
        |> Or_bottom.value_map ~bottom:None
             ~f:(Option.map Simple.must_be_symbol)
      in
      match existing_symbol with
      | Some (Some _) ->
        dacc, reified_continuation_params_to_symbols, reified_definitions,
          closure_symbols_by_set
      | None | Some None ->
        (* Since the continuation we're dealing with might be inlined and
           we don't handle [extra_params_and_args] on such continuations at
           the [Apply_cont] site, be certain that the only variables appearing
           in our new [Let_symbol] bindings don't involve any of those
           (extra) parameters. *)
        let typing_env = DE.typing_env (DA.denv dacc) in
        let allowed_free_vars =
          Variable.Set.diff (TE.var_domain typing_env) params
        in
        match
          T.reify ~allowed_free_vars typing_env ~min_name_mode:NM.normal ty
        with
        | Lift to_lift ->
          lift_non_closure_discovered_via_reified_continuation_param_types
            dacc var to_lift ~reified_continuation_params_to_symbols
            ~reified_definitions ~closure_symbols_by_set
        | Lift_set_of_closures { closure_id; function_decls; closure_vars; } ->
          lift_set_of_closures_discovered_via_reified_continuation_param_types
            dacc var closure_id function_decls ~closure_vars
            ~reified_continuation_params_to_symbols
            ~reified_definitions ~closure_symbols_by_set
        | Simple _ | Cannot_reify | Invalid ->
          dacc, reified_continuation_params_to_symbols, reified_definitions,
            closure_symbols_by_set)
    params
    (dacc, Variable.Map.empty, [], Set_of_closures.Map.empty)

let lift_via_reification_of_continuation_param_types dacc ~params
      ~(extra_params_and_args : Continuation_extra_params_and_args.t)
      ~(handler : Expr.t) =
  let dacc, _reified_continuation_params_to_symbols, reified_definitions,
      _closure_symbols_by_set =
    let params =
      Variable.Set.union (KP.List.var_set params)
        (KP.List.var_set extra_params_and_args.extra_params)
    in
    reify_types_of_continuation_param_types dacc ~params
  in
  (* CR mshinwell: If recursion extends beyond that which can be handled by the
     set-of-closures cases, then we would need something like the "symbol
     placeholder" approach (variables that are substituted for the
     continuation's parameters, which are in turn substituted for symbols at the
     Cmm translation phase). (Any SCC class containing >1 set of closures is
     maybe a bug?) *)
  let reified_definitions =
    Sort_lifted_constants.sort dacc reified_definitions
  in
  let handler =
    List.fold_left (fun handler (bound_symbols, defining_expr) ->
        Let_symbol.create bound_symbols defining_expr handler
        |> Expr.create_let_symbol)
      handler
      reified_definitions.bindings_outermost_last
  in
  dacc, handler
