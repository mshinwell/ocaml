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
  defined_vars : Flambda_kind.t Variable.Map.t;
  equations : Type_grammar.t Name.Map.t;
  cse : Simple.t Flambda_primitive.Eligible_for_cse.Map.t;
}

let defined_vars t = t.defined_vars

let defined_names t =
  Name.set_of_var_set (Variable.Map.keys t.defined_vars)

(*
let defines_name_but_no_equations t name =
  match Name.to_var name with
  | None -> false
  | Some var ->
    Variable.Map.mem var t.defined_vars
      && not (Name.Map.mem name t.equations)
*)

let print_with_cache ~cache ppf { defined_vars; equations; cse; } =
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
  (* CR mshinwell: Print [defined_vars] when not called from
     [Typing_env.print] *)
  if Variable.Map.is_empty defined_vars then
    if Flambda_primitive.Eligible_for_cse.Map.is_empty cse then
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(equations@ @[<v 1>%a@])@])\
          @]"
        print_equations equations
    else
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(equations@ @[<v 1>%a@])@])@ \
          @[<hov 1>(cse@ @[<hov 1>%a@])@]\
          @]"
        print_equations equations
        (Flambda_primitive.Eligible_for_cse.Map.print Simple.print) cse
  else
    if Flambda_primitive.Eligible_for_cse.Map.is_empty cse then
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(equations@ @[<v 1>%a@])@]@ \
          )@]"
        print_equations equations
    else
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(equations@ @[<v 1>%a@])@]@ \
          @[<hov 1>(cse@ @[<hov 1>%a@])@]\
          )@]"
        print_equations equations
        (Flambda_primitive.Eligible_for_cse.Map.print Simple.print) cse

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant _t = ()

let apply_name_permutation ({ defined_vars; equations; cse; } as t)
      perm =
  let defined_vars_changed = ref false in
  let defined_vars' =
    Variable.Map.fold (fun var kind defined_vars ->
        let var' = Name_permutation.apply_variable perm var in
        if not (var == var') then begin
          defined_vars_changed := true
        end;
        Variable.Map.add var' kind defined_vars)
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
      Name_mode.in_types
  in
  let free_names_equations =
    Name.Map.fold (fun name typ free_names ->
        let free_names' = 
          Name_occurrences.add_name (Type_grammar.free_names typ)
            name Name_mode.in_types
        in
        Name_occurrences.union free_names free_names')
      equations
      free_names_defined_vars
  in
  Flambda_primitive.Eligible_for_cse.Map.fold
    (fun prim (bound_to : Simple.t) acc ->
      Simple.pattern_match bound_to
        ~const:(fun _ -> acc)
        ~name:(fun name ->
          let free_in_prim =
            Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
              (Flambda_primitive.Eligible_for_cse.free_names prim)
              Name_mode.in_types
          in
          Name_occurrences.add_name free_in_prim
            name Name_mode.in_types))
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

let add_definition t var kind =
  if !Clflags.flambda_invariant_checks
    && Variable.Map.mem var t.defined_vars
  then begin
    Misc.fatal_errorf "Environment extension already binds variable %a:@ %a"
      Variable.print var
      print t
  end;
  { t with
    defined_vars = Variable.Map.add var kind t.defined_vars
  }

let equation_is_directly_recursive name ty =
  match Type_grammar.get_alias_exn ty with
  | exception Not_found -> false
  | simple ->
    Simple.pattern_match simple
      ~name:(fun name' -> Name.equal name name')
      ~const:(fun _ -> false)

let check_equation t name ty =
  if equation_is_directly_recursive name ty then begin
    Misc.fatal_errorf "Directly recursive equation@ %a = %a@ \
        disallowed (Typing_env_level):@ %a"
      Name.print name
      Type_grammar.print ty
      print t
  end

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
    Variable.Map.union (fun var _data1 _data2 ->
        Misc.fatal_errorf "Cannot concatenate levels that have overlapping \
            defined variables (e.g. %a):@ %a@ and@ %a"
          Variable.print var
          print t1
          print t2)
      t1.defined_vars
      t2.defined_vars
  in
  let equations =
    Name.Map.union (fun _ _ty1 ty2 -> Some ty2) t1.equations t2.equations
  in
  let cse =
    Flambda_primitive.Eligible_for_cse.Map.union (fun _prim _t1 t2 -> Some t2)
      t1.cse
      t2.cse
  in
  { defined_vars;
    equations;
    cse;
  }

let meet_equation0 env t name typ =
  check_equation t name typ;
  let meet_typ, env_extension =
    match Name.Map.find name t.equations with
    | exception Not_found -> typ, Typing_env_extension.empty ()
    | existing_typ -> Type_grammar.meet' env typ existing_typ
  in
  let env =
    let typing_env =
      let typing_env = Meet_env.env env in
      Typing_env.add_equation
        (Typing_env.add_env_extension typing_env env_extension)
        name typ
    in
    Meet_env.with_typing_env env typing_env
  in
  (* CR mshinwell: This special case needs further thinking about *)
  (* When meeting recursive types we can end up attempting to add
     equations "x : =x". *)
  let equations =
    if equation_is_directly_recursive name meet_typ then t.equations
    else Name.Map.add (* replace *) name meet_typ t.equations
  in
  let equations =
    Typing_env_extension.pattern_match env_extension ~f:(fun t_from_meet ->
      if not (Variable.Map.is_empty t_from_meet.defined_vars) then begin
        Misc.fatal_errorf "Didn't expect [defined_vars] in:@ %a"
          print t_from_meet
      end;
      Name.Map.fold (fun name typ equations ->
          check_equation t name typ;
          Name.Map.add (* replace *) name typ equations)
        t_from_meet.equations
        equations)
  in
  let t = { t with equations; } in
  t, env

let meet_equation env t name typ =
  try meet_equation0 env t name typ
  with Misc.Fatal_error ->
    if !Clflags.flambda2_context_on_error then begin
      Format.eprintf "\n%sContext is:%s meeting equation %a : %a@ in \
          level@ %a@ and environment@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Name.print name
        Type_grammar.print typ
        print t
        Typing_env.print (Meet_env.env env)
    end;
    raise Misc.Fatal_error

let meet0 env (t1 : t) (t2 : t) =
  let defined_vars =
    Variable.Map.union (fun var kind1 kind2 ->
        if Flambda_kind.equal kind1 kind2 then Some kind1
        else
          Misc.fatal_errorf "Cannot meet levels that have overlapping \
              defined variables (e.g. %a) that disagree on kind and/or \
              binding time:@ %a@ and@ %a"
            Variable.print var
            print t1
            print t2)
      t1.defined_vars
      t2.defined_vars
  in
  let env =
    (* CR mshinwell: Should the binding time be ignored? *)
    let typing_env =
      Variable.Map.fold (fun var kind typing_env ->
          let name =
            Name_in_binding_pos.create (Name.var var) Name_mode.in_types
          in
          Typing_env.add_definition typing_env name kind)
        defined_vars
        (Meet_env.env env)
    in
    Meet_env.with_typing_env env typing_env
  in
  let t =
    { (empty ()) with
      defined_vars;
    }
  in
  let t, env =
    Name.Map.fold (fun name ty (t, env) -> meet_equation env t name ty)
      t1.equations
      (t, env)
  in
  let t, _env =
    Name.Map.fold (fun name ty (t, env) -> meet_equation env t name ty)
      t2.equations
      (t, env)
  in
  let cse =
    (* CR mshinwell: Add [Map.inter] (also used elsewhere) *)
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

(* CR mshinwell: [env_at_fork] -> [env_at_cut] *)

let join_types ~env_at_fork envs_with_levels =
  List.fold_left
    (fun (joined_types, env_at_fork) (env_at_use, _id, _use_kind, _vars, t) ->
      Format.eprintf "join_types with level:@ %a\n%!" print t;
      let env_at_fork =
        Variable.Map.fold (fun var kind env ->
            let env =
              Typing_env.add_definition env
                (Name_in_binding_pos.var
                  (Var_in_binding_pos.create var Name_mode.in_types))
                kind
            in
            Typing_env.add_equation env (Name.var var)
              (Type_grammar.bottom kind))
          t.defined_vars
          env_at_fork
      in
      let env_at_fork =
        List.fold_left (fun env_at_fork (env_at_use, _, _, _, _) ->
            let symbols = Typing_env.defined_symbols env_at_use in
            let env_at_fork =
              Typing_env.add_symbol_definitions env_at_fork symbols
            in
            Symbol.Set.fold (fun sym env_at_fork ->
                Typing_env.add_equation env_at_fork (Name.symbol sym)
                  (Type_grammar.bottom Flambda_kind.value))
              symbols
              env_at_fork)
          env_at_fork
          envs_with_levels
      in
      let joined_types =
        Name.Map.union (fun name joined_ty use_ty ->
            Format.eprintf "Processing name:@ %a\n%!" Name.print name;
            let joined_ty =
              (* Recall: the order of environments matters here. *)
              Type_grammar.join env_at_fork
                ~left_env:env_at_fork ~left_ty:joined_ty
                ~right_env:env_at_use ~right_ty:use_ty
            in
            Format.eprintf "joined type:@ %a\n%!"
              Type_grammar.print joined_ty;
            Some joined_ty)
          joined_types
          t.equations
      in
      joined_types, env_at_fork)
    (Name.Map.empty, env_at_fork)
    envs_with_levels

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

    let print ppf t =
      match t with
      | Needs_extra_binding { bound_to; } ->
        Format.fprintf ppf "@[<hov 1>(Needs_extra_binding@ %a)@]"
          Simple.print bound_to
      | Rhs_in_scope { bound_to; } ->
        Format.fprintf ppf "@[<hov 1>(Rhs_in_scope@ %a)@]"
          Simple.print bound_to

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

let cse_with_eligible_lhs ~env_at_fork envs_with_levels =
  let module EP = Flambda_primitive.Eligible_for_cse in
  List.fold_left (fun eligible (env_at_use, id, _, _, t) ->
      EP.Map.fold (fun prim bound_to eligible ->
        let prim =
          EP.filter_map_args prim ~f:(fun arg ->
            match
              Typing_env.get_canonical_simple_exn env_at_use arg
                ~min_name_mode:Name_mode.normal
            with
            | exception Not_found -> None
            | arg ->
              if not (Typing_env.mem_simple env_at_fork arg) then None
              else Some arg)
        in
        match prim with
        | None -> eligible
        | Some prim ->
          match
            Typing_env.get_canonical_simple_exn env_at_use bound_to
              ~min_name_mode:Name_mode.normal
          with
          | exception Not_found -> eligible
          | bound_to ->
            let bound_to : Rhs_kind.t =
              if Typing_env.mem_simple env_at_fork bound_to then
                Rhs_in_scope { bound_to; }
              else
                Needs_extra_binding { bound_to; }
            in
            (* CR mshinwell: Add [Map.add_or_replace]. *)
            match EP.Map.find prim eligible with
            (* CR mshinwell: The list needs to be deduped *)
            | exception Not_found ->
              EP.Map.add prim
                (Apply_cont_rewrite_id.Map.singleton id bound_to)
                eligible
            | from_prev_levels ->
              let map =
                Apply_cont_rewrite_id.Map.add id bound_to from_prev_levels
              in
              EP.Map.add prim map eligible)
      t.cse
      eligible)
    EP.Map.empty
    envs_with_levels

let join_cse envs_with_levels cse ~allowed =
  let module EP = Flambda_primitive.Eligible_for_cse in
  EP.Map.fold (fun prim bound_to_map (cse, extra_bindings, allowed) ->
      let has_value_on_all_paths =
        List.for_all (fun (_, id, _, _, _) ->
            Apply_cont_rewrite_id.Map.mem id bound_to_map)
          envs_with_levels
      in
      if not has_value_on_all_paths then
        cse, extra_bindings, allowed
      else
        let bound_to_set =
          Apply_cont_rewrite_id.Map.data bound_to_map
          |> Rhs_kind.Set.of_list
        in
        match Rhs_kind.Set.get_singleton bound_to_set with
        | Some (Rhs_kind.Rhs_in_scope { bound_to; }) ->
          EP.Map.add prim bound_to cse, extra_bindings, allowed
        | None | Some (Rhs_kind.Needs_extra_binding { bound_to = _; }) ->
          let prim_result_kind =
            Flambda_primitive.result_kind' (EP.to_primitive prim)
          in
          let var = Variable.create "cse_param" in
          let extra_param = Kinded_parameter.create var prim_result_kind in
          let bound_to =
            Apply_cont_rewrite_id.Map.map Rhs_kind.bound_to bound_to_map
          in
          let cse = EP.Map.add prim (Simple.var var) cse in
          let extra_args =
            Apply_cont_rewrite_id.Map.map
              (fun simple : Continuation_extra_params_and_args.Extra_arg.t ->
                Already_in_scope simple)
              bound_to
          in
          let extra_bindings =
            Continuation_extra_params_and_args.add extra_bindings
              ~extra_param ~extra_args
          in
          let allowed =
            Name_occurrences.add_name allowed (Name.var var)
              Name_mode.normal
          in
          cse, extra_bindings, allowed)
    cse
    (EP.Map.empty, Continuation_extra_params_and_args.empty, allowed)

let construct_joined_level envs_with_levels ~allowed ~joined_types ~cse =
  let module EP = Flambda_primitive.Eligible_for_cse in
  let defined_vars =
    List.fold_left (fun defined_vars (_env_at_use, _id, _use_kind, _vars, t) ->
        let defined_vars_this_level =
          Variable.Map.filter (fun var _ ->
              Name_occurrences.mem_var allowed var)
            t.defined_vars
        in
        Variable.Map.union (fun var kind1 kind2 ->
            if Flambda_kind.equal kind1 kind2 then Some kind1
            else
              Misc.fatal_errorf "Cannot join levels that disagree on the kind \
                  of [defined_vars] (%a and %a for %a)"
                Flambda_kind.print kind1
                Flambda_kind.print kind2
                Variable.print var)
          defined_vars
          defined_vars_this_level)
      Variable.Map.empty
      envs_with_levels
  in
  let equations =
    Name.Map.filter (fun name ty ->
        Name_occurrences.mem_name allowed name
          && not (Type_grammar.is_obviously_unknown ty))
      joined_types
  in
  let cse =
    (* Any CSE equation whose right-hand side identifies a name in the [allowed]
       set is propagated.  We don't need to check the left-hand sides because
       we know all of those names are in [env_at_fork]. *)
    EP.Map.filter (fun _prim bound_to ->
        Simple.pattern_match bound_to
          ~const:(fun _ -> true)
          ~name:(fun name -> Name_occurrences.mem_name allowed name))
      cse
  in
  { defined_vars;
    equations;
    cse;
  }

let join ~env_at_fork envs_with_levels =
  Format.eprintf "JOIN\n%!";
  Format.eprintf "At fork:@ %a\n%!" Typing_env.print env_at_fork;
  List.iter (fun (_, _, _, _, t) ->
      Format.eprintf "One level:@ %a\n%!" print t)
    envs_with_levels;
  (* For each name which has an equation on at least one of the given levels,
     calculate the joined type for that name, across all the given levels.
     If no type exists on a given level then it is treated as bottom (which
     does not affect the joined type being calculated). *)
  let joined_types, env_at_fork_with_existentials_defined =
    join_types ~env_at_fork envs_with_levels
  in
  Format.eprintf "joined_types:@ %a\n%!"
    (Name.Map.print Type_grammar.print) joined_types;
  (* CSE equations have a left-hand side specifying a primitive and a
     right-hand side specifying a [Simple].  The left-hand side is matched
     against portions of terms.  As such, the [Simple]s therein must have
     name mode [Normal], since we do not do CSE for phantom bindings (see
     [Simplify_common]).  It follows that any CSE equation whose left-hand side
     involves a name not defined at the fork point, having canonicalised such
     name, cannot be propagated.  This step also canonicalises the right-hand
     sides of the CSE equations. *)
  let cse = cse_with_eligible_lhs ~env_at_fork envs_with_levels in
  Format.eprintf "CSE with eligible LHS:@ %a\n%!"
    (Flambda_primitive.Eligible_for_cse.Map.print
      (Apply_cont_rewrite_id.Map.print Rhs_kind.print))
    cse;
  (* Next calculate which equations (describing joined types) to propagate to
     the join point.  (Recall that the environment at the fork point includes
     the parameters of the continuation being called at the join. We wish to
     ensure that information in the types of these parameters is not lost.)
     - Equations on names defined in the environment at the fork point are
     always propagated.
     - Definitions of, and equations on, names that occur free on the
     right-hand sides of the propagated equations are also themselves
     propagated. The definition of any such propagated name (i.e. one that
     does not occur in the environment at the fork point) will be made
     existential. *)
  let allowed =
    Name.Map.fold (fun name ty allowed ->
        if Typing_env.mem env_at_fork name
          || Name.is_symbol name
        then
          Name_occurrences.add_name
            (Name_occurrences.union allowed
              (Typing_env.free_names_transitive
                env_at_fork_with_existentials_defined ty))
            name Name_mode.in_types
        else
          allowed)
      joined_types
      Name_occurrences.empty
  in
  (* To make use of a CSE equation at or after the join point, its right-hand
     side must have the same value, no matter which path is taken from the
     fork point to the join point.  We filter out equations that do not
     satisfy this.  Sometimes we can force an equation to satisfy the
     property by explicitly passing the value of the right-hand side as an
     extra parameter to the continuation at the join point. *)
  let cse, extra_params, allowed = join_cse envs_with_levels cse ~allowed in
  Format.eprintf "Joined CSE:@ %a\n%!"
    (Flambda_primitive.Eligible_for_cse.Map.print Simple.print) cse;
  Format.eprintf "allowed:@ %a\n%!" Name_occurrences.print allowed;
  (* Having calculated which equations to propagate, the resulting level can
     now be constructed. *)
  let t = construct_joined_level envs_with_levels ~allowed ~joined_types ~cse in
  Format.eprintf "Join result:@ %a\n%!" print t;
  t, extra_params

let n_way_join ~env_at_fork envs_with_levels =
  match envs_with_levels with
  | [] -> empty (), Continuation_extra_params_and_args.empty
  | [_, _, Continuation_use_kind.Inlinable, _, _] ->
    Misc.fatal_error "Unnecessary join; should have been caught earlier"
  | envs_with_levels -> join ~env_at_fork envs_with_levels