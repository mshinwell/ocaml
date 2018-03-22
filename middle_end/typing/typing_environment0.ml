(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Make (T : sig
  include Flambda_type0_internal_intf.S

  val print : Format.formatter -> t -> unit

  val print_typing_environment
     : Format.formatter
    -> typing_environment
    -> unit

  val free_names_set : flambda_type -> Name.Set.t

  val force_to_kind_value : t -> of_kind_value ty

  val force_to_kind_naked_number
     : 'a Flambda_kind.Naked_number.t
    -> t
    -> 'a of_kind_naked_number ty

  val force_to_kind_fabricated : t -> of_kind_fabricated ty

  val kind : flambda_type -> Flambda_kind.t

  module Equations : Equations_intf.S
    with type equations := equations
    with type typing_environment := typing_environment
    with type flambda_type := flambda_type

  val meet
     : bias_towards:t_in_context
    -> t_in_context
    -> t * Equations.t
end) = struct
  type t = T.typing_environment

  open T

  (* CR mshinwell: Add invariant check.  First one: symbols should never be
     existential *)

  let create_typing_environment0 ~resolver ~must_be_closed =
    let existential_freshening = Freshening.activate Freshening.empty in
    { must_be_closed;
      resolver;
      canonical_names_to_aliases = Name.Map.empty;
      names_to_types = Name.Map.empty;
      levels_to_names = Scope_level.Map.empty;
      existentials = Name.Set.empty;
      existential_freshening;
    }

  let add_alias_typing_environment env ~canonical_name ~alias =
(* XXX This check is inconvenient for [r]
    if not (Name.Map.mem canonical_name env.names_to_types) then begin
      Misc.fatal_errorf "Cannot add alias %a of canonical name %a: the \
          canonical name is not bound in the environment: %a"
        Name.print alias
        Name.print canonical_name
        print_typing_environment env
    end;
*)
    let canonical_names_to_aliases =
      Name.Map.update canonical_name (function
          | None -> Some (Name.Set.singleton alias)
          | Some aliases -> Some (Name.Set.add alias aliases))
        env.canonical_names_to_aliases
    in
    { env with
      canonical_names_to_aliases;
    }

  let aliases_typing_environment env ~canonical_name =
    match Name.Map.find canonical_name env.canonical_names_to_aliases with
    | exception Not_found ->
      Misc.fatal_errorf "Cannot find aliases of canonical name %a which is \
          not bound in the environment: %a"
        Name.print canonical_name
        print_typing_environment env
    | aliases -> aliases

  let invariant_typing_environment env =
    if !Clflags.flambda_invariant_checks && env.must_be_closed then begin
      let free_names =
        Name.Map.fold (fun _name (_level, t) free_names ->
            Name.Set.union (free_names_set t) free_names)
          env.names_to_types
          Name.Set.empty
      in
      let domain = Name.Map.keys env.names_to_types in
      if not (Name.Set.subset free_names domain) then begin
        Misc.fatal_errorf "Typing environment is not closed (%a free):@ %a"
          Name.Set.print (Name.Set.diff free_names domain)
          print_typing_environment env
      end
    end

  type binding_type = Normal | Existential

  let find_typing_environment env name =
    match Name.Map.find name env.names_to_types with
    | exception Not_found ->
      Misc.fatal_errorf "Cannot find %a in environment:@ %a"
        Name.print name
        print_typing_environment env
    | _scope_level, ty ->
      let binding_type =
        if Name.Set.mem name env.existentials then Existential
        else Normal
      in
      match binding_type with
      | Normal -> ty, Normal
      | Existential ->
   (* XXX     let ty = rename_variables t freshening in *)
        ty, Existential

  type still_unresolved =
    | Resolved
    | Still_unresolved

  let resolve_aliases_on_ty0 (type a) env ~force_to_kind
        (ty : a ty) : (a ty) * (Name.t option) * still_unresolved =
    let rec resolve_aliases names_seen ~canonical_name (ty : a ty) =
      let resolve (name : Name_or_export_id.t) : _ * _ * still_unresolved =
        if Name_or_export_id.Set.mem name names_seen then begin
          Misc.fatal_errorf "Loop on %a whilst resolving aliases"
            Name_or_export_id.print name
        end;
        let continue_resolving t ~canonical_name =
          let names_seen = Name_or_export_id.Set.add name names_seen in
          let ty = force_to_kind t in
          resolve_aliases names_seen ~canonical_name ty
        in
        match name with
        | Name name ->
          let t, _binding_type = find_typing_environment env name in
          continue_resolving t ~canonical_name:(Some name)
        | Export_id export_id ->
          match env.resolver export_id with
          | Some t -> continue_resolving t ~canonical_name
          | None -> ty, None, Still_unresolved
      in
      match ty with
      | No_alias _ -> ty, canonical_name, Resolved
      | Type export_id -> resolve (Name_or_export_id.Export_id export_id)
      | Equals name ->
(*
        Format.eprintf "recursing on %a, seen %a\n%!" Name.print name
          Name_or_export_id.Set.print names_seen;
*)
        resolve (Name_or_export_id.Name name)
    in
    resolve_aliases Name_or_export_id.Set.empty ~canonical_name:None ty

  let resolve_aliases_on_ty env ~force_to_kind ty =
    let t, canonical_name, _still_unresolved =
      resolve_aliases_on_ty0 env ~force_to_kind ty
    in
    t, canonical_name

  let resolve_aliases_and_squash_unresolved_names_on_ty env ~kind:_
        ~force_to_kind ~unknown ty =
    let ty, canonical_name, still_unresolved =
      resolve_aliases_on_ty0 env ~force_to_kind ty
    in
    match still_unresolved with
    | Resolved -> ty, canonical_name
    | Still_unresolved -> unknown, canonical_name

  (* CR mshinwell: choose this function or the one above *)
  let resolve_aliases_and_squash_unresolved_names_on_ty' env ~kind:_
        ~print_ty ~force_to_kind ~unknown:_ ty
        : _ unknown_or_join * (Name.t option) =
    let ty, canonical_name, _still_unresolved =
      try resolve_aliases_on_ty0 env ~force_to_kind ty
      with Misc.Fatal_error -> begin
        Format.eprintf "\n%sContext is: \
            resolve_aliases_and_squash_unresolved_names_on_ty':%s\
            @ %a@ Environment:@ %a\n"
          (Misc_color.bold_red ())
          (Misc_color.reset ())
          print_ty ty
          print_typing_environment env;
        raise Misc.Fatal_error
      end
    in
    match ty with
    | No_alias uoj -> uoj, canonical_name
    | Type _ | Equals _ -> Unknown, canonical_name

  (* CR mshinwell: this should return not just the canonical name but all
     other aliases encountered, so the meet functions can add judgements
     for those. *)
  (* CR mshinwell: rename env -> t, t -> ty *)
  let resolve_aliases (env, t) : flambda_type * (Name.t option) =
    match t.descr with
    | Value ty ->
      let force_to_kind = force_to_kind_value in
      let ty, canonical_name =
        resolve_aliases_on_ty env ~force_to_kind ty
      in
      { t with descr = Value ty; }, canonical_name
    | Naked_number (ty, kind) ->
      let force_to_kind = force_to_kind_naked_number kind in
      let ty, canonical_name = resolve_aliases_on_ty env ~force_to_kind ty in
      { t with descr = Naked_number (ty, kind); }, canonical_name
    | Fabricated ty ->
      let force_to_kind = force_to_kind_fabricated in
      let ty, canonical_name = resolve_aliases_on_ty env ~force_to_kind ty in
      { t with descr = Fabricated ty; }, canonical_name

  let _resolve_aliases_and_squash_unresolved_names (env, t)
        : flambda_type * (Name.t option) =
    let kind = kind t in
    match t.descr with
    | Value ty ->
      let unknown : ty_value = No_alias Unknown in
      let force_to_kind = force_to_kind_value in
      let ty, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty env ~kind
          ~force_to_kind ~unknown ty
      in
      { t with descr = Value ty; }, canonical_name
    | Naked_number (ty, kind) ->
      let unknown : _ ty_naked_number = No_alias Unknown in
      let force_to_kind = force_to_kind_naked_number kind in
      let ty, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty env ~kind
          ~force_to_kind ~unknown ty
      in
      { t with descr = Naked_number (ty, kind); }, canonical_name
    | Fabricated ty ->
      let force_to_kind = force_to_kind_fabricated in
      let unknown : ty_fabricated = No_alias Unknown in
      let ty, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty env
          ~kind ~force_to_kind ~unknown ty
      in
      { t with descr = Fabricated ty; }, canonical_name

  let add_or_replace_typing_environment' env name scope_level t =
    let names_to_types =
      Name.Map.add name (scope_level, t) env.names_to_types
    in
    let levels_to_names =
      Scope_level.Map.update scope_level
        (function
           | None -> Some (Name.Set.singleton name)
           | Some names -> Some (Name.Set.add name names))
        env.levels_to_names
    in
    let env =
      { env with
        names_to_types;
        levels_to_names;
      }
    in
    match resolve_aliases (env, t) with
    | _t, None -> env
    | _t, Some canonical_name ->
      add_alias_typing_environment env ~canonical_name ~alias:name

  let add_or_replace_typing_environment env name scope_level t =
    (* CR mshinwell: We should add a comment here explaining where this can
       be used and what it cannot be used for (e.g. changing a name's scope
       level) *)
(*
if Scope_level.to_int scope_level = 2
  && not (Name.Map.mem name t.names_to_types)
then begin
  Format.eprintf "AoR for %a:@ %s\n%!"
    Name.print name
    (Printexc.raw_backtrace_to_string (Printexc.get_callstack 20))
end;
*)
    if !Clflags.flambda_invariant_checks then begin
      invariant_typing_environment env;
      let free_names = free_names_set t in
      if Name.Set.mem name free_names then begin
        Misc.fatal_errorf "Adding binding of %a to@ %a@ would cause a \
            direct circularity in a type.  Environment: %a"
          Name.print name
          print t
          print_typing_environment env
      end
    end;
    add_or_replace_typing_environment' env name scope_level t

  let add_typing_environment env name scope_level ty =
    match Name.Map.find name env.names_to_types with
    | exception Not_found ->
      add_or_replace_typing_environment env name scope_level ty
    | _ty ->
      Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
        Name.print name
        print_typing_environment env

  let singleton0_typing_environment ~resolver name scope_level ty
        ~must_be_closed =
    add_typing_environment
      (create_typing_environment0 ~resolver ~must_be_closed)
      name scope_level ty

  let restrict_to_names0_typing_environment t allowed =
    let names_to_types =
      Name.Map.filter (fun name _ty -> Name.Set.mem name allowed)
        t.names_to_types
    in
    let levels_to_names =
      Scope_level.Map.filter_map t.levels_to_names ~f:(fun _level names ->
        let names = Name.Set.inter names allowed in
        if Name.Set.is_empty names then None
        else Some names)
    in
    let existentials = Name.Set.inter t.existentials allowed in
    let existential_freshening =
      Freshening.restrict_to_names t.existential_freshening allowed
    in
    let t =
      { must_be_closed = t.must_be_closed;
        resolver = t.resolver;
        canonical_names_to_aliases = t.canonical_names_to_aliases;
        names_to_types;
        levels_to_names;
        existentials;
        existential_freshening;
      }
    in
    try
      invariant_typing_environment t;
      t
    with Misc.Fatal_error -> begin
      Format.eprintf "\n%sContext is: \
          restrict_to_names0_typing_environment:%s\
          @ Restricting to: %a@ \nEnvironment:@ %a\n"
        (Misc_color.bold_red ())
        (Misc_color.reset ())
        Name.Set.print allowed
        print_typing_environment t;
      raise Misc.Fatal_error
    end

  let phys_equal env1 env2 =
    env1 == env2
      || (Name.Map.is_empty env1.names_to_types
            && Name.Map.is_empty env2.names_to_types)

  let print = print_typing_environment

  let create ~resolver =
    create_typing_environment0 ~resolver ~must_be_closed:true

  let create_using_resolver_from t = create ~resolver:t.resolver

  let add_or_replace = add_or_replace_typing_environment

  let add = add_typing_environment

  let singleton0 = singleton0_typing_environment

  let singleton ~resolver name scope_level ty =
    singleton0 ~resolver name scope_level ty ~must_be_closed:true

  let find = find_typing_environment

  let scope_level t name =
    match Name.Map.find name t.names_to_types with
    | exception Not_found ->
      Misc.fatal_errorf "scope_level: Cannot find %a in environment:@ %a"
        Name.print name
        print t
    | scope_level, _ty -> scope_level

  (* CR mshinwell: improve efficiency *)
  let find_with_scope_level t name =
    let ty, binding_type = find t name in
    let scope_level = scope_level t name in
    ty, scope_level, binding_type

  (* CR mshinwell: Perhaps this function needs an iteration bound *)
  let rec meet_typing_environment0 (env1 : typing_environment)
        (env2 : typing_environment) : typing_environment =
    let resolver = env1.resolver in
    let equations_from_meet = ref (Equations.create ()) in
    let canonical_names_to_aliases =
      Name.Map.union_merge Name.Set.union
        env1.canonical_names_to_aliases
        env2.canonical_names_to_aliases
    in
    let names_to_types =
      Name.Map.union_merge (fun (level1, ty1) (level2, ty2) ->
          if not (Scope_level.equal level1 level2) then begin
            Misc.fatal_errorf "meet_typing_environment: \
                Scope levels differ for:@ %a@ and:@ %a@ levels1:@ %a@ \
                levels2:@ %a@ env1:@ %a@ env2:@ %a"
              print ty1
              print ty2
              (Scope_level.Map.print Name.Set.print) env1.levels_to_names
              (Scope_level.Map.print Name.Set.print) env2.levels_to_names
              print_typing_environment env1
              print_typing_environment env2
          end;
          let ty, new_equations_from_meet =
            meet ~bias_towards:(env1, ty1) (env2, ty2)
          in
          equations_from_meet := Meet_and_join.meet_equations ~resolver
            new_equations_from_meet !equations_from_meet;
          level1, ty)
        env1.names_to_types
        env2.names_to_types
    in
    let all_levels_to_names =
      Scope_level.Map.union_merge
        (fun names1 names2 -> Name.Set.union names1 names2)
        env1.levels_to_names
        env2.levels_to_names
    in
    let levels_to_names =
      Scope_level.Map.map (fun names ->
          Name.Set.filter (fun name ->
              Name.Map.mem name names_to_types)
            names)
        all_levels_to_names
    in
    let existentials =
      Name.Set.union env1.existentials env2.existentials
    in
    let existential_freshening =
      env1.existential_freshening (* XXX *)
    in
    let env =
      { must_be_closed = env1.must_be_closed || env2.must_be_closed;
        resolver = env1.resolver;
        canonical_names_to_aliases;
        names_to_types;
        levels_to_names;
        existentials;
        existential_freshening;
      }
    in
    Typing_environment0.invariant env;
    let equations_from_meet =
      Equations.to_typing_environment ~resolver:env.resolver
        !equations_from_meet
    in
    meet_typing_environment env equations_from_meet

  and meet_typing_environment env1 env2 =
    if env1 == env2 then env1
    else if is_empty_typing_environment env1 then env2
    else if is_empty_typing_environment env2 then env1
    else
      try
        meet_typing_environment0 env1 env2
      with Misc.Fatal_error -> begin
        Format.eprintf "\n%sContext is: meeting two typing environments:%s\
            @ %a\n\n%sand%s:@ %a\n"
          (Misc_color.bold_red ())
          (Misc_color.reset ())
          print_typing_environment env1
          (Misc_color.bold_red ())
          (Misc_color.reset ())
          print_typing_environment env2;
        raise Misc.Fatal_error
      end

  let replace_meet env name (ty_env, ty) =
    match Name.Map.find name env.names_to_types with
    | exception Not_found ->
      Misc.fatal_errorf "Cannot meet types for name %a which is unbound \
          in the environment: %a"
        Name.print name
        print_typing_environment env
    | scope_level, _existing_ty ->
      let ty_env =
        Typing_environment0.add ty_env name scope_level ty
      in
      Typing_environment0.meet env ty_env

  let add_or_replace_meet t name scope_level ty =
    match Name.Map.find name t.names_to_types with
    | exception Not_found -> add t name scope_level ty
    | _ ->
      (* CR mshinwell: We need to think about this some more.  Is [ty]
         supposed to only have free names in [t]? *)
      replace_meet t name (t, ty)

  let find_opt t name =
    match Name.Map.find name t.names_to_types with
    | exception Not_found -> None
    | _scope_level, ty ->
      let binding_type =
        if Name.Set.mem name t.existentials then Existential
        else Normal
      in
      match binding_type with
      | Normal -> Some (ty, Normal)
      | Existential ->
   (* XXX     let ty = rename_variables t freshening in *)
        Some (ty, Existential)

  let is_existential t name =
    let _ty, binding_type = find t name in
    match binding_type with
    | Normal -> false
    | Existential -> true

  let cut t ~existential_if_defined_at_or_later_than =
(*
Format.eprintf "Cutting environment at %a: %a\n%!"
Scope_level.print existential_if_defined_at_or_later_than
print_typing_environment t;
*)
    let new_existentials =
      Scope_level.Map.fold (fun scope_level names resulting_existentials ->
          let will_be_existential =
            Scope_level.(>=)
              scope_level existential_if_defined_at_or_later_than
          in
          if will_be_existential then
            let non_symbols = Name.variables_only names in
            Name.Set.union non_symbols resulting_existentials
          else
            resulting_existentials)
        t.levels_to_names
        Name.Set.empty
    in
    let existential_freshening =
      Name.Set.fold (fun (name : Name.t) freshening ->
          match name with
          | Symbol _ ->
            Misc.fatal_error "Symbols cannot be existentially bound"
          | Var var ->
            let _new_var, freshening =
              Freshening.add_variable freshening var
            in
            freshening)
        new_existentials
        t.existential_freshening
    in
let result =
    (* XXX we actually need to rename in the domain of [names_to_types] *)
    { must_be_closed = t.must_be_closed;
      resolver = t.resolver;
      canonical_names_to_aliases = t.canonical_names_to_aliases;
      names_to_types = t.names_to_types;
      levels_to_names = t.levels_to_names;
      existentials = Name.Set.union t.existentials new_existentials;
      existential_freshening;
    }
in
(*
Format.eprintf "Result is: %a\n%!"
print_typing_environment result;
*)
    result

  let aliases = aliases_typing_environment

  let meet = Meet_and_join.meet_typing_environment
  let join = Meet_and_join.join_typing_environment

  let restrict_to_names t allowed =
    let allowed = Name_occurrences.everything allowed in
    restrict_to_names0_typing_environment t allowed

  let restrict_to_symbols t =
    let symbols = Name.symbols_only_map t.names_to_types in
    restrict_to_names0_typing_environment t (Name.Map.keys symbols)

  (* CR mshinwell: The "derived" functions should be in Flambda_type *)
  let remove t name =
    let allowed =
      Name.Set.remove name (Name.Map.keys t.names_to_types)
    in
    restrict_to_names0_typing_environment t allowed

  let filter t ~f =
    let allowed =
      Name.Map.fold (fun name ty allowed ->
          if f name ty then Name.Set.add name allowed
          else allowed)
        t.names_to_types
        Name.Set.empty
    in
    restrict_to_names0_typing_environment t allowed

  let is_empty t = Name.Map.is_empty t.names_to_types

  let domain t =
    let domain =
      Name.Set.diff (Name.Map.keys t.names_to_types) t.existentials
    in
    Name_occurrences.create_from_set_in_terms domain

  let add_equations t equations =
    let equations_as_env =
      Meet_and_join.equations_to_typing_environment ~resolver:t.resolver
        equations
    in
    meet t equations_as_env

  let to_equations t =
    let t =
      { t with
        must_be_closed = true;
      }
    in
    { typing_judgements = Some t;
    }

  let resolver t = t.resolver

  let equal ~equal_type
        { must_be_closed = must_be_closed1;
          resolver = _;
          canonical_names_to_aliases = canonical_names_to_aliases1;
          names_to_types = names_to_types1;
          levels_to_names = _;
          existentials = existentials1;
          existential_freshening = _;
        }
        { must_be_closed = must_be_closed2;
          resolver = _;
          canonical_names_to_aliases = canonical_names_to_aliases2;
          names_to_types = names_to_types2;
          levels_to_names = _;
          existentials = existentials2;
          existential_freshening = _;
        } =
    let equal_scope_and_type (scope1, t1) (scope2, t2) =
      Scope_level.equal scope1 scope2 && equal_type t1 t2
    in
    must_be_closed1 = must_be_closed2
      && Name.Map.equal equal_scope_and_type names_to_types1 names_to_types2
      && Name.Map.equal Name.Set.equal canonical_names_to_aliases1
           canonical_names_to_aliases2
      && Name.Set.equal existentials1 existentials2

  let free_names_transitive env t =
    let all_names = ref (Name_occurrences.create ()) in
    let rec loop to_follow =
      all_names := Name_occurrences.union !all_names to_follow;
      match Name_occurrences.choose_and_remove_amongst_everything to_follow with
      | None -> ()
      | Some (name, to_follow) ->
        let t, _binding_type = find env name in
        let names = free_names t in
        loop (Name_occurrences.union to_follow names)
    in
    loop (free_names t);
    !all_names

  let free_names_transitive_list env ts =
    List.fold_left (fun names t ->
        Name_occurrences.union names (free_names_transitive env t))
      (Name_occurrences.create ())
      ts

  let diff ~strictly_more_precise t1 t2 : equations =
    let names_to_types =
      Name.Map.filter (fun name (level1, ty1) ->
          match Name.Map.find name t2.names_to_types with
          | exception Not_found -> true
          | (level2, ty2) ->
            assert (Scope_level.equal level1 level2);
            strictly_more_precise (t1, ty1) ~than:(t2, ty2))
        t1.names_to_types
    in
    let free_names =
      Name.Map.fold (fun _name (_level, ty) free_names ->
          Name_occurrences.union free_names (free_names_transitive t1 ty))
        names_to_types
        (Name_occurrences.create ())
    in
    let t1 =
      { t1 with
        must_be_closed = false;
      }
    in
    let t =
      restrict_to_names0_typing_environment t1 (Name.Map.keys names_to_types)
    in
    let unbound =
      Name.Set.diff (Name_occurrences.everything free_names)
        (Name.Map.keys names_to_types)
    in
    let t =
      Name.Set.fold (fun unbound t ->
          let level, kind =
            match Name.Map.find unbound t1.names_to_types with
            | exception Not_found -> assert false
            | (level, ty) -> level, kind ty
          in
          add_or_replace_typing_environment t unbound level (unknown kind))
        unbound
        t
    in
    let t =
      { t with
        must_be_closed = true;
      }
    in
    { typing_judgements = Some t;
    }

  let invariant = invariant_typing_environment

  let restrict_names_to_those_occurring_in_types t tys =
    let free_names = free_names_transitive_list t tys in
Format.eprintf "Restricting to: %a\n%!"
  Name_occurrences.print free_names;
    restrict_to_names t free_names
end
