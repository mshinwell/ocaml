(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  defined_vars : (Flambda_kind.t * Binding_time.t) Variable.Map.t;
  equations : Type_grammar.t Name.Map.t;
  cse : Simple.t Flambda_primitive.Eligible_for_cse.Map.t;
}

let has_no_defined_vars t =
  Variable.Map.is_empty t.defined_vars

let defined_vars_in_order t =
  let sorted =
    List.sort
      (fun (_var1, (_kind1, binding_time1)) (_var2, (_kind2, binding_time2)) ->
        Binding_time.compare binding_time1 binding_time2)
      (Variable.Map.bindings t.defined_vars)
  in
  List.map (fun (var, (kind, _binding_time)) -> var, kind) sorted

let defined_vars_in_order' t = List.map fst (defined_vars_in_order t)

let print_with_cache ~cache ppf (({ defined_vars; equations; cse; } as t) : t) =
  let print_equations ppf equations =
    let equations = Name.Map.bindings equations in
    match equations with
    | [] -> Format.pp_print_string ppf "()"
    | _::_ ->
      Format.pp_print_string ppf "(";
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (name, ty) ->
          Format.fprintf ppf
            "@[<hov 1>%a@ :@ %a@]"
            Name.print name
            (Type_grammar.print_with_cache ~cache) ty)
        ppf equations;
      Format.pp_print_string ppf ")"
  in
  if Variable.Map.is_empty defined_vars then
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(equations@ @[<hov 1>%a@])@])@ \
        @[<hov 1>(cse@ @[<hov 1>%a@])@]\
        @]"
      print_equations equations
      (Flambda_primitive.Eligible_for_cse.Map.print Simple.print) cse
  else
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(defined_vars@ @[<hov 1>%a@])@]@ \
        @[<hov 1>(equations@ @[<hov 1>%a@])@]@ \
        @[<hov 1>(cse@ @[<hov 1>%a@])@]\
        )@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Variable.print)
      (defined_vars_in_order' t)
      print_equations equations
      (Flambda_primitive.Eligible_for_cse.Map.print Simple.print) cse

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant _t = ()

let apply_name_permutation ({ defined_vars; equations; cse; } as t)
      perm =
  let defined_vars_changed = ref false in
  let defined_vars' =
    Variable.Map.fold (fun var (kind, binding_time) defined_vars ->
        let var' = Name_permutation.apply_variable perm var in
        if not (var == var') then begin
          defined_vars_changed := true
        end;
        Variable.Map.add var' (kind, binding_time) defined_vars)
      defined_vars
      Variable.Map.empty
  in
  let equations_changed = ref false in
  let equations' =
    Name.Map.fold (fun name typ equations ->
        let name' = Name_permutation.apply_name perm name in
        let typ' = Type_grammar.apply_name_permutation typ perm in
        if not (name == name' && typ == typ') then begin
          equations_changed := true
        end;
        Name.Map.add name' typ' equations)
      equations
      Name.Map.empty
  in
  let cse_changed = ref false in
  let cse' =
    Flambda_primitive.Eligible_for_cse.Map.fold (fun prim simple cse' ->
        let simple' = Simple.apply_name_permutation simple perm in
        let prim' =
          Flambda_primitive.Eligible_for_cse.apply_name_permutation prim perm
        in
        if (not (simple == simple')) || (not (prim == prim')) then begin
          cse_changed := true
        end;
        Flambda_primitive.Eligible_for_cse.Map.add prim' simple' cse')
      cse
      Flambda_primitive.Eligible_for_cse.Map.empty
  in
  if (not !defined_vars_changed)
    && (not !equations_changed)
    && (not !cse_changed)
  then t
  else 
    { defined_vars = defined_vars';
      equations = equations';
      cse = cse';
    }

let free_names { defined_vars; equations; cse; } =
  let free_names_defined_vars =
    Name_occurrences.create_variables (Variable.Map.keys defined_vars)
      Name_occurrence_kind.in_types
  in
  let free_names_equations =
    Name.Map.fold (fun name typ free_names ->
        let free_names' = 
          Name_occurrences.add_name (Type_grammar.free_names typ)
            name Name_occurrence_kind.in_types
        in
        Name_occurrences.union free_names free_names')
      equations
      free_names_defined_vars
  in
  Flambda_primitive.Eligible_for_cse.Map.fold
    (fun prim (bound_to : Simple.t) acc ->
      match Simple.descr bound_to with
      | Const _ | Discriminant _ -> acc
      | Name name ->
        let free_in_prim =
          Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
            (Flambda_primitive.Eligible_for_cse.free_names prim)
            Name_occurrence_kind.in_types
        in
        Name_occurrences.add_name free_in_prim
          name Name_occurrence_kind.in_types)
    cse
    free_names_equations

let empty () =
  { defined_vars = Variable.Map.empty;
    equations = Name.Map.empty;
    cse = Flambda_primitive.Eligible_for_cse.Map.empty;
  }

let is_empty { defined_vars; equations; cse; } =
  Variable.Map.is_empty defined_vars
    && Name.Map.is_empty equations
    && Flambda_primitive.Eligible_for_cse.Map.is_empty cse

let equations t = t.equations

let cse t = t.cse

let add_definition t var kind binding_time =
  if Variable.Map.mem var t.defined_vars then begin
    Misc.fatal_errorf "Environment extension already binds variable %a:@ %a"
      Variable.print var
      print t
  end;
  { t with
    defined_vars = Variable.Map.add var (kind, binding_time) t.defined_vars
  }

let check_equation t name ty =
  match Type_grammar.get_alias ty with
  | None -> ()
  | Some simple ->
    match Simple.descr simple with
    | Name name' ->
      if Name.equal name name' then begin
        Misc.fatal_errorf "Directly recursive equation@ %a = %a@ \
            disallowed (Typing_env_level):@ %a"
          Name.print name
          Type_grammar.print ty
          print t
      end
    | Const _ | Discriminant _ -> ()

let one_equation name ty =
  check_equation (empty ()) name ty;
  { defined_vars = Variable.Map.empty;
    equations = Name.Map.singleton name ty;
    cse = Flambda_primitive.Eligible_for_cse.Map.empty;
  }

let add_or_replace_equation t name ty =
  check_equation t name ty;
  { t with
    equations = Name.Map.add name ty t.equations;
  }

let find_equation t name =
  match Name.Map.find name t.equations with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment level:@ %a"
      Name.print name
      print t
  | ty -> ty

let add_cse t prim ~bound_to =
  match Flambda_primitive.Eligible_for_cse.Map.find prim t.cse with
  | exception Not_found ->
    let cse =
      Flambda_primitive.Eligible_for_cse.Map.add prim bound_to t.cse
    in
    { t with cse; }
  | _bound_to -> t

let concat (t1 : t) (t2 : t) =
  let defined_vars =
    Variable.Map.merge (fun var data1 data2 ->
        match data1, data2 with
        | None, None -> None
        | Some data, None | None, Some data -> Some data
        | Some _, _ ->
          Misc.fatal_errorf "Cannot concatenate levels that have overlapping \
              defined variables (e.g. %a):@ %a@ and@ %a"
            Variable.print var
            print t1
            print t2)
      t1.defined_vars
      t2.defined_vars
  in
  let equations =
    Name.Map.fold (fun name ty equations ->
        Name.Map.add name ty equations)
      t2.equations
      t1.equations
  in
  let cse =
    Flambda_primitive.Eligible_for_cse.Map.fold (fun prim bound_to equations ->
        Flambda_primitive.Eligible_for_cse.Map.add prim bound_to equations)
      t2.cse
      t1.cse
  in
  { defined_vars;
    equations;
    cse;
  }

let meet_equation env t name typ =
  let meet_typ, env_extension =
    match Name.Map.find name t.equations with
    | exception Not_found -> typ, Typing_env_extension.empty ()
    | existing_typ -> Type_grammar.meet env typ existing_typ
  in
  let env =
    Meet_env.map_env env ~f:(fun typing_env ->
      Typing_env.add_equation
        (Typing_env.add_env_extension typing_env env_extension)
        name typ)
  in
  let equations = Name.Map.add (* replace *) name meet_typ t.equations in
  let equations =
    Typing_env_extension.pattern_match env_extension ~f:(fun t_from_meet ->
      if not (Variable.Map.is_empty t_from_meet.defined_vars) then begin
        Misc.fatal_errorf "Didn't expect [defined_vars] in:@ %a"
          print t_from_meet
      end;
      Name.Map.fold (fun name typ equations ->
          Name.Map.add (* replace *) name typ equations)
        t_from_meet.equations
        equations)
  in
  let t = { t with equations; } in
  t, env

let meet0 env (t1 : t) (t2 : t) =
  let defined_vars =
    Variable.Map.merge (fun var data1 data2 ->
        match data1, data2 with
        | None, None -> None
        | Some data, None | None, Some data -> Some data
        | Some _, _ ->
          Misc.fatal_errorf "Cannot meet levels that have overlapping \
              defined variables (e.g. %a):@ %a@ and@ %a"
            Variable.print var
            print t1
            print t2)
      t1.defined_vars
      t2.defined_vars
  in
  let env =
    (* CR mshinwell: Should the binding time be ignored? *)
    Meet_env.map_env env ~f:(fun typing_env ->
      Variable.Map.fold (fun var (kind, _binding_time) typing_env ->
          let name =
            Name_in_binding_pos.create (Name.var var)
              Name_occurrence_kind.in_types
          in
          Typing_env.add_definition typing_env name kind)
        defined_vars
        typing_env)
  in
  let t, env =
    Name.Map.fold (fun name ty (t, env) -> meet_equation env t name ty)
      t1.equations
      (empty (), env)
  in
  let t, _env =
    Name.Map.fold (fun name ty (t, env) -> meet_equation env t name ty)
      t2.equations
      (t, env)
  in
  let cse =
    Flambda_primitive.Eligible_for_cse.Map.merge (fun _ simple1 simple2 ->
        match simple1, simple2 with
        | None, None | None, Some _ | Some _, None -> None
        | Some simple1, Some simple2 ->
          if Simple.equal simple1 simple2 then Some simple1
          else None)
      t1.cse t2.cse
  in
  { t with cse; }

let meet env t1 t2 =
  (* Care: the domains of [t1] and [t2] are treated as contravariant.
     As such, since this is [meet], we perform unions on the domains.
     So if one of them is bottom, the result of meeting it with any other
     level is that level, not bottom. *)
  if is_empty t1 then t2
  else if is_empty t2 then t1
  else meet0 env t1 t2

type cannot_use =
  | Equation_ok
  | Equation_ineligible

module Rhs_kind = struct
  type t =
    | Needs_extra_binding of { bound_to : Simple.t; }
    | Rhs_in_scope of { bound_to : Simple.t; }

  let bound_to t =
    match t with
    | Needs_extra_binding { bound_to; }
    | Rhs_in_scope { bound_to; } -> bound_to

  include Identifiable.Make (struct
    type nonrec t = t

    let print _ _ = Misc.fatal_error "Rhs_kind.print not yet implemented"
    let output _ _ = Misc.fatal_error "Rhs_kind.output not yet implemented"
    let hash _ = Misc.fatal_error "Rhs_kind.hash not yet implemented"
    let equal _ = Misc.fatal_error "Rhs_kind.equal not yet implemented"

    let compare t1 t2 =
      match t1, t2 with
      | Needs_extra_binding { bound_to = bound_to1; },
          Needs_extra_binding { bound_to = bound_to2; } ->
        Simple.compare bound_to1 bound_to2
      | Rhs_in_scope { bound_to = bound_to1; },
          Rhs_in_scope { bound_to = bound_to2; } ->
        Simple.compare bound_to1 bound_to2
      | Needs_extra_binding _, _ -> -1
      | Rhs_in_scope _, _ -> 1
  end)
end

let cse_after_n_way_join envs_with_extensions ~allowed =
  let module EP = Flambda_primitive.Eligible_for_cse in
  let canonicalise env simple ~min_occurrence_kind =
    let canonical_simple =
      Typing_env.get_canonical_simple env ~min_occurrence_kind simple
    in
    match canonical_simple with
    | Bottom | Ok None -> None
    | Ok (Some simple) ->
      if Simple.allowed simple ~allowed then Some simple
      else None
  in
  let canonicalise_lhs env cse =
    EP.Map.fold (fun prim bound_to cse ->
        let cannot_use, prim =
          (* CR mshinwell: share code with [make_suitable_for_environment]? *)
          EP.fold_args prim
            ~init:Equation_ok
            ~f:(fun cannot_use arg ->
              match cannot_use with
              | Equation_ineligible -> Equation_ineligible, arg
              | Equation_ok ->
                let min_occurrence_kind = Name_occurrence_kind.in_types in
                match canonicalise env arg ~min_occurrence_kind with
                | None -> Equation_ineligible, arg
                | Some arg -> Equation_ok, arg)
        in
        match cannot_use with
        | Equation_ineligible -> cse
        | Equation_ok -> EP.Map.add prim bound_to cse)
      cse
      EP.Map.empty
  in
  let cses_with_canonicalised_lhs =
    List.map (fun (env, id, _interesting_vars, t) ->
        env, id, canonicalise_lhs env t.cse)
      envs_with_extensions
  in
  let lhs_of_cses_valid_on_all_paths =
    match cses_with_canonicalised_lhs with
    | [] -> EP.Set.empty
    | (_env, _id, cse)::cses ->
      List.fold_left (fun valid_on_all_paths (_env, _id, cse) ->
          EP.Set.inter (EP.Map.keys cse) valid_on_all_paths)
        (EP.Map.keys cse)
        cses
  in
  EP.Set.fold (fun prim (cse, extra_bindings) ->
      let rhs_kinds =
        List.fold_left (fun rhs_kinds (env, id, cse) ->
            let bound_to = EP.Map.find prim cse in
            let rhs_kind : Rhs_kind.t =
              (* XXX This may not be sufficient---we may need to abort
                 completely.  Or maybe CSE equations shouldn't exist that
                 would require that, and we should have an earlier check? *)
              let min_occurrence_kind = Name_occurrence_kind.normal in
              match canonicalise env bound_to ~min_occurrence_kind with
              | None -> Needs_extra_binding { bound_to; }
              | Some bound_to -> Rhs_in_scope { bound_to; }
            in
            begin match Simple.descr (Rhs_kind.bound_to rhs_kind) with
            | Name name ->
              if not (Typing_env.mem env name) then begin
                Misc.fatal_errorf "Unbound name %a during CSE:@ %a"
                  Name.print name
                  Typing_env.print env
              end;
            | Const _ | Discriminant _ -> () (* CR mshinwell: add more *)
            end;
            Apply_cont_rewrite_id.Map.add id rhs_kind rhs_kinds)
          Apply_cont_rewrite_id.Map.empty
          cses_with_canonicalised_lhs
      in
      let rhs_has_same_value_on_all_paths =
        Rhs_kind.Set.get_singleton
          (Rhs_kind.Set.of_list (Apply_cont_rewrite_id.Map.data rhs_kinds))
      in
      match rhs_has_same_value_on_all_paths with
      | None | Some (Needs_extra_binding _) ->
        let prim_result_kind =
          Flambda_primitive.result_kind' (EP.to_primitive prim)
        in
        let extra_param =
          let var = Variable.create "cse_param" in
          Kinded_parameter.create (Parameter.wrap var) prim_result_kind
        in
        let bound_to =
          Apply_cont_rewrite_id.Map.map Rhs_kind.bound_to rhs_kinds
        in
        let cse =
          EP.Map.add prim (Simple.var (Kinded_parameter.var extra_param)) cse
        in
        let extra_args =
          Apply_cont_rewrite_id.Map.map
            (fun simple : Continuation_extra_params_and_args.Extra_arg.t ->
              Already_in_scope simple)
            bound_to
        in
        let extra_bindings =
          Continuation_extra_params_and_args.add extra_bindings ~extra_param
            ~extra_args
        in
        cse, extra_bindings
      | Some ((Rhs_in_scope _) as rhs_kind) ->
        EP.Map.add prim (Rhs_kind.bound_to rhs_kind) cse, extra_bindings)
    lhs_of_cses_valid_on_all_paths
    (EP.Map.empty, Continuation_extra_params_and_args.empty)

let trivial_join t ~initial_env_at_join ~interesting_vars envs_with_levels =
  (* For trivial joins we allow variables defined in the level [t] to be
     treated as existential.  However we only do this for variables that
     may be "interesting" (typically the transitive closure, via the
     type environment, of those occurring in the argument types at the
     single continuation use site). *)
  let allowed =
    Variable.Set.union (Typing_env.var_domain initial_env_at_join)
      interesting_vars
  in
  let defined_vars =
    Variable.Map.filter (fun var _ -> Variable.Set.mem var allowed)
      t.defined_vars
  in
  let equations =
    Name.Map.filter (fun (name : Name.t) _ ->
        match name with
        | Var var -> Variable.Set.mem var allowed
        | Symbol _ -> true)
      t.equations
  in
  let cse, extra_cse_bindings =
    cse_after_n_way_join envs_with_levels ~allowed
  in
  let t =
    { defined_vars;
      equations;
      cse;
    }
  in
  t, extra_cse_bindings

let non_trivial_join ~initial_env_at_join:env_at_join envs_with_levels =
  assert (List.length envs_with_levels > 1);
  (* For non-trivial joins, no existentials are currently propagated. *)
  let names_with_equations_in_join =
    let names_at_join =
      Name.set_of_var_set (Typing_env.var_domain env_at_join)
    in
    List.fold_left (fun names_with_equations_in_join (_env, _id, _vars, t) ->
        Name.Set.inter (Name.Map.keys t.equations)
          names_with_equations_in_join)
      names_at_join
      envs_with_levels
  in
  let t =
    Name.Set.fold (fun name result_t ->
        assert (not (Name.Map.mem name result_t.equations));
        match envs_with_levels with
        | [] -> result_t
        | (first_env_at_use, _id, _vars, first_t) :: envs_with_levels ->
          let result_t, join_ty =
            Type_grammar.make_suitable_for_environment0
              (find_equation first_t name)
              first_env_at_use ~suitable_for:env_at_join
              result_t
          in
          let result_t, join_ty =
            List.fold_left
              (fun (result_t, join_ty) (env_at_use, _id, _vars, t) ->
                let result_t, ty =
                  Type_grammar.make_suitable_for_environment0
                    (find_equation t name)
                    env_at_use ~suitable_for:env_at_join
                    result_t
                in
                let join_ty = Type_grammar.join env_at_join join_ty ty in
                result_t, join_ty)
              (result_t, join_ty)
              envs_with_levels
          in
          add_or_replace_equation result_t name join_ty)
      names_with_equations_in_join
      (empty ())
  in
  let cse, extra_cse_bindings =
    cse_after_n_way_join envs_with_levels
      ~allowed:(Typing_env.var_domain env_at_join)
  in
  let t : t = { t with cse; } in
  assert (Variable.Map.is_empty t.defined_vars);
  t, extra_cse_bindings

let n_way_join ~initial_env_at_join envs_with_levels =
  match envs_with_levels with
  | [] -> empty (), Continuation_extra_params_and_args.empty
  | [_env, _id, interesting_vars, t] as envs_with_levels ->
    trivial_join t ~initial_env_at_join ~interesting_vars envs_with_levels
  | envs_with_levels -> non_trivial_join ~initial_env_at_join envs_with_levels
