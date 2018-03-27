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

  val free_names : flambda_type -> Name_occurrences.t

  val free_names_set : flambda_type -> Name.Set.t

  val unknown : Flambda_kind.t -> t

  val force_to_kind_value : t -> of_kind_value ty

  val force_to_kind_naked_number
     : 'a Flambda_kind.Naked_number.t
    -> t
    -> 'a of_kind_naked_number ty

  val force_to_kind_fabricated : t -> of_kind_fabricated ty

  val kind : flambda_type -> Flambda_kind.t

  module Typing_env_extension : Typing_env_extension_intf.S
    with type env_extension := env_extension
    with type typing_environment := typing_environment
    with type flambda_type := flambda_type

  val join : t_in_context -> t_in_context -> t

  val meet
     : bias_towards:t_in_context
    -> t_in_context
    -> t * Typing_env_extension.t

  val is_empty_typing_environment : typing_environment -> bool
end) (Meet_and_join : sig
  include Meet_and_join_intf.S_for_types
    with type t_in_context := T.t_in_context
    with type env_extension := T.env_extension
    with type flambda_type := T.flambda_type
end) = struct
  type t = T.typing_environment

  open T

  type typing_environment = T.typing_environment
  type env_extension = T.Typing_env_extension.t
  type flambda_type = T.flambda_type
  type t_in_context = T.t_in_context
  type 'a ty = 'a T.ty
  type 'a unknown_or_join = 'a T.unknown_or_join

  type binding_type = Normal | Existential

  let create ~resolver =
    let existential_freshening = Freshening.activate Freshening.empty in
    { resolver;
      canonical_names_to_aliases = Name.Map.empty;
      names_to_types = Name.Map.empty;
      levels_to_names = Scope_level.With_sublevel.Map.empty;
      next_sublevel_by_level = Scope_level.Map.empty;
      existentials = Name.Set.empty;
      existential_freshening;
    }

  let create_using_resolver_from t = create ~resolver:t.resolver

(*
  let _add_alias_typing_environment env ~canonical_name ~alias =
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
*)

  type still_unresolved =
    | Resolved
    | Still_unresolved

  let resolve_aliases_on_ty0 (type a) env ?bound_name ~force_to_kind
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
          let t, _binding_type = find env name in
          continue_resolving t ~canonical_name:(Some name)
        | Export_id export_id ->
          match env.resolver export_id with
          | Some t -> continue_resolving t ~canonical_name
          | None -> ty, None, Still_unresolved
      in
      match ty with
      | No_alias _ -> ty, canonical_name, Resolved
      | Type export_id -> resolve (Name_or_export_id.Export_id export_id)
      | Equals name -> resolve (Name_or_export_id.Name name)
    in
    let seen =
      match bound_name with
      | None -> Name_or_export_id.Set.empty
      | Some bound_name -> Name_or_export_id.Set.singleton (Name bound_name)
    in
    resolve_aliases seen ~canonical_name:None ty

  let resolve_aliases_on_ty env ?bound_name ~force_to_kind ty =
    let t, canonical_name, _still_unresolved =
      resolve_aliases_on_ty0 env ?bound_name ~force_to_kind ty
    in
    t, canonical_name

  let resolve_aliases_and_squash_unresolved_names_on_ty env ?bound_name
        ~force_to_kind ~unknown ty =
    let ty, canonical_name, still_unresolved =
      resolve_aliases_on_ty0 env ?bound_name ~force_to_kind ty
    in
    match still_unresolved with
    | Resolved -> ty, canonical_name
    | Still_unresolved -> unknown, canonical_name

  (* CR mshinwell: choose this function or the one above *)
  let resolve_aliases_and_squash_unresolved_names_on_ty' env ?bound_name
        ~print_ty ~force_to_kind ty
        : _ unknown_or_join * (Name.t option) =
    let ty, canonical_name, _still_unresolved =
      try resolve_aliases_on_ty0 env ?bound_name ~force_to_kind ty
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

  (* CR mshinwell: rename env -> t, t -> ty *)
  let resolve_aliases ?bound_name (env, t) : flambda_type * (Name.t option) =
    match t.descr with
    | Value ty ->
      let force_to_kind = force_to_kind_value in
      let ty, canonical_name =
        resolve_aliases_on_ty env ?bound_name ~force_to_kind ty
      in
      { t with descr = Value ty; }, canonical_name
    | Naked_number (ty, kind) ->
      let force_to_kind = force_to_kind_naked_number kind in
      let ty, canonical_name =
        resolve_aliases_on_ty env ?bound_name ~force_to_kind t
      in
      { t with descr = Naked_number (ty, kind); }, canonical_name
    | Fabricated ty ->
      let force_to_kind = force_to_kind_fabricated in
      let ty, canonical_name =
        resolve_aliases_on_ty env ?bound_name ~force_to_kind ty
      in
      { t with descr = Fabricated ty; }, canonical_name

  let invariant t =
    (* CR mshinwell: Add more checks here *)
    if !Clflags.flambda_invariant_checks then begin
      (* Since [fold] operates in (scope level, sublevel) order, then the
         following check establishes that dependencies between bindings in
         the environment are only in one direction. *)
      fold t ~init:Name.Set.empty
        ~f:(fun names_seen (name : Name.t) (binding_type : binding_type)
                ~scope_level ty ->
          let free_names = T.free_names ty in
          if not (Name.Set.subset free_names names_seen) then begin
            Misc.fatal_errorf "Typing environment is not closed (%a free):@ %a"
              Name.Set.print (Name.Set.diff free_names domain)
              print_typing_environment env
          end;
          begin match name with
          | Var _ -> ()
          | Symbol _ ->
            match binding_type with
            | Normal -> ()
            | Existential ->
              Misc.fatal_errorf "Symbols should never be marked as \
                  existential: %a in@ %a"
                Name.print name
                print_typing_environment t
          end;
          Name.Set.union free_names names_seen);
      (* Checking that alias resolution works also ensures there are no
         cycles via aliases. *)
      Name.Map.iter (fun bound_name (_level, ty) ->
          ignore (Sys.opaque_identity (resolve_aliases ~bound_name (env, ty))))
        t.names_to_types
    end

  let find env name =
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

  let fold t ~init ~f =
    Scope_level.Map.fold (fun level by_sublevel acc ->
          Scope_level.Sublevel.Map.fold (fun sublevel (name, ty) acc ->
            let scope_level = Scope_level.With_sublevel.create level sublevel in
            let binding_type =
              if Name.Set.mem name t.existentials then Existential
              else Normal
            in
            f acc name binding_type scope_level ty)
        by_sublevel
        acc)
      t.levels_to_types
      init

  let allocate_sublevel t level =
    let sublevel =
      match Scope_level.Map.find level t.next_sublevel_by_level with
      | exception Not_found -> Scope_level.Sublevel.initial
      | sublevel -> sublevel
    in
    let next_sublevel_by_level =
      Scope_level.Map.add level (Scope_level.Sublevel.next sublevel)
        t.next_sublevel_by_level
    in
    let t =
      { t with
        next_sublevel_by_level;
      }
    in
    t, sublevel

  let min_level_for_new_binding t =
    let all_levels = Scope_level.Map.keys t.levels_to_types in
    match Scope_level.Set.max_elt_opt all_levels with
    | None -> Scope_level.initial
    | Some level -> level

  type sense =
    | New_equation_must_be_more_precise
    | Existing_equation_must_be_more_precise

  let print_sense ppf (sense : sense) =
    match sense with
    | New_equation_must_be_more_precise ->
      Format.fprintf ppf "New_equation_must_be_more_precise"
    | Existing_equation_must_be_more_precise ->
      Format.fprintf ppf "Existing_equation_must_be_more_precise"

  let invariant_for_any_new_binding t name level
        (entry : typing_environment_entry) =
    let free_names =
      match entry with
      | Definition ty | Equation ty -> free_names ty
      | CSE prim -> Flambda_primitive.free_names prim
    in
    if Name.Set.mem name free_names then begin
      Misc.fatal_errorf "Cannot add binding %a = %a@ as it would produce \
          a circular dependency"
        Name.print name
        print_typing_environment_entry entry
    end;
    let min_level = min_level_for_new_binding t in
    if Scope_level.(<) level min_level then begin
      Misc.fatal_errorf "Cannot add binding %a = %a@ to this environment \
          with scope level %a (minimum permitted level %a):@ %a"
        Name.print name
        print_typing_environment_entry entry
        Scope_level.print level
        Scope_level.print min_level
        print_typing_environment t
    end;
    match find_opt t name with
    | None ->
      begin match entry with
      | Definition _ | CSE _ -> ()
      | Equation _ ->
        Misc.fatal_errorf "Cannot add %a = %a@ for name undefined in \
            environment:@ %a"
          Name.print name
          print_typing_environment_entry entry
          print_typing_environment t
      end
    | Some _ ->
      match entry with
      | Definition _ ->
        Misc.fatal_errorf "Cannot redefine %a = %a@ in environment:@ %a"
          Name.print name
          print_typing_environment_entry entry
          print_typing_environment t
      | Equation _ | CSE _ -> ()

  let rec invariant_for_new_equation t name level ty ~sense =
    let existing_ty, _level = find t name in
    let meet_ty, env_extension =
      Meet_and_join.meet ~bound_name:name (t, existing_ty) (t, ty)
    in
    let ty_must_be_strictly_more_precise, other_ty =
      match sense with
      | New_equation_must_be_more_precise -> ty, existing_ty
      | Existing_equation_must_be_more_precise -> existing_ty, ty
    in
    let as_or_more_precise =
      T.equal meet_ty ty_must_be_strictly_more_precise
    in
    let strictly_more_precise =
      ty_as_or_more_precise && not (T.equal meet_ty other_ty)
    in
    if not ty_strictly_more_precise then
      Misc.fatal_errorf "Cannot add equation %a = %a@ to this environment: \
          as_or_more_precise %b,@ strictly_more_precise %b,@ meet_ty@ %a,@ \
          existing_ty@ %a,@ sense@ %a.@  Env:@ %a"
        Name.print name
        T.print ty
        ty_as_or_more_precise
        ty_strictly_more_precise
        T.print meet_ty
        T.print existing_ty
        print_sense sense
        print_typing_environment t
    end;
    Typing_env_extension.iter env_extension
      ~f:(fun name _binding_type level ty ->
        let level = Scope_level.With_sublevel.level level in
        invariant_for_any_new_binding t name level (Equation ty);
        invariant_for_new_equation t name level ty
          ~sense:Existing_equation_must_be_more_precise)

  let invariant_for_new_binding t name level
        (entry : typing_environment_entry) =
    invariant_for_any_new_binding t name level entry;
    match entry with
    | Definition _ | CSE _ -> ()
    | Equation ty ->
      invariant_for_new_equation t name level ty
        ~sense:New_equation_must_be_more_precise

  let add t (name : Name.t) level ty =
    let binding : typing_environment_entry = Definition ty in
    invariant_for_new_binding t name level entry;



    let t =
      let canonical_name =
        match Name.Map.find name env.names_to_types with
        | exception Not_found -> None
        | _level, t ->
          let _t, canonical_name = resolve_aliases (env, t) in
          canonical_name
      in
      let _t, canonical_name_for_t = resolve_aliases (env, t) in
      let normal_case ~name ~scope_level =
        let names_to_types =
          Name.Map.add name (scope_level, t) env.names_to_types
        in
        let levels_to_names =
          Scope_level.Map.update scope_level
            (function
              | None -> Some (Name.Set.singleton name)
              | Some by_sublevel -> Some (Name.Set.add name names))
            env.levels_to_names
        in
        { env with
          names_to_types;
          levels_to_names;
        }
      in
      match canonical_name, canonical_name_for_t with
      | Some canonical_name, Some canonical_name_for_t
          when Name.equal canonical_name canonical_name_for_t -> env
      | Some _, Some _ -> assert false (* XXX *)
      | None, Some canonical_name_for_t
          when Name.equal name canonical_name_for_t -> env
      | None, Some _
      | None, None -> normal_case ~name ~scope_level
      | Some canonical_name, None ->
        let scope_level, _existing_ty =
          Name.Map.find canonical_name env.names_to_types
        in
        normal_case ~name:canonical_name ~scope_level
    in
    t

(*
let debug () =
  match canonical_name, canonical_name_for_t with
  | None, None -> ()
  | _, _ ->
    Format.eprintf "ADD/REPLACE %a : %a.  CN bound name = %a  \
        CN new type = %a\n.  Existing env: %a%!"
      Name.print name
      T.print t
      (Misc.Stdlib.Option.print Name.print) canonical_name
      (Misc.Stdlib.Option.print Name.print) canonical_name_for_t
      print_typing_environment env
in
begin match name with
| Var var ->
  Variable.debug_when_stamp_matches var ~stamp:21 ~f:debug;
  Variable.debug_when_stamp_matches var ~stamp:30 ~f:debug
| Symbol _ -> ()
end;
*)

(* Alias map is currently unused
    match resolve_aliases (env, t) with
    | _t, None -> env
    | _t, Some canonical_name ->
      add_alias_typing_environment env ~canonical_name ~alias:name
*)

  (* CR mshinwell: We should add a comment here explaining where this can
     be used and what it cannot be used for (e.g. changing a name's scope
     level) *)
  let add_or_replace env name scope_level t =
    if !Clflags.flambda_invariant_checks then begin
      invariant env;
      let free_names = free_names_set t in
      if Name.Set.mem name free_names then begin
        Misc.fatal_errorf "Adding binding of %a to@ %a@ would cause a \
            direct circularity in a type.  Environment: %a"
          Name.print name
          print t
          print_typing_environment env
      end
    end;
    let t = add_or_replace' env name scope_level t in
    invariant t;
    t

  let add_typing_environment env name scope_level ty =
    match Name.Map.find name env.names_to_types with
    | exception Not_found ->
      add_or_replace env name scope_level ty
    | _ty ->
      Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
        Name.print name
        print_typing_environment env

  let singleton0_typing_environment ~resolver name scope_level ty
        ~must_be_closed =
    add_typing_environment
      (create ~resolver ~must_be_closed)
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
      invariant t;
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

  let add_or_replace = add_or_replace

  let add = add_typing_environment

  let singleton0 = singleton0_typing_environment

  let singleton ~resolver name scope_level ty =
    singleton0 ~resolver name scope_level ty ~must_be_closed:true

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
  (* XXX This logic needs to be used for join as well *)
  let rec meet_typing_environment0 (env1 : typing_environment)
        (env2 : typing_environment) : typing_environment =
    let resolver = env1.resolver in
    let canonical_names_to_aliases =
      Name.Map.union_merge Name.Set.union
        env1.canonical_names_to_aliases
        env2.canonical_names_to_aliases
    in
    let names_to_types_in_both =
      Name.Map.inter (fun (level1, ty1) (level2, ty2) ->
          Some (level1, ty1, level2, ty2))
        env1.names_to_types env2.names_to_types
    in
    let names_to_types_only_in_env1 =
      Name.Map.filter (fun name _ ->
          not (Name.Map.mem name names_to_types_in_both))
        env1.names_to_types
    in
    let names_to_types_only_in_env2 =
      Name.Map.filter (fun name _ ->
          not (Name.Map.mem name names_to_types_in_both))
        env2.names_to_types
    in
    let names_to_types_only_in_one_env =
      Name.Map.disjoint_union names_to_types_only_in_env1
        names_to_types_only_in_env2
    in
    let levels_to_names =
      Scope_level.Map.union_merge
        (fun names1 names2 -> Name.Set.union names1 names2)
        env1.levels_to_names
        env2.levels_to_names
    in
    let proto_result_env =
      { env1 with
        names_to_types = names_to_types_only_in_one_env;
        levels_to_names;
      }
    in
    let env_extension_from_meet, env =
      Name.Map.fold
        (fun name (level1, ty1, level2, ty2) (env_extension_from_meet, env) ->
          if not (Scope_level.equal level1 level2) then begin
            Misc.fatal_errorf "meet_typing_environment: \
                Scope levels differ for %a:@ %a@ and@ %a@ \
                levels1:@ %a@ levels2:@ %a@ env1:@ %a@ env2:@ %a"
              Name.print name
              T.print ty1
              T.print ty2
              (Scope_level.Map.print Name.Set.print) env1.levels_to_names
              (Scope_level.Map.print Name.Set.print) env2.levels_to_names
              print env1
              print env2
          end;
          let meet_ty, new_env_extension_from_meet =
            meet ~bias_towards:(env1, ty1) (env2, ty2)
          in
          let env_extension_from_meet =
            Typing_env_extension.meet ~resolver new_env_extension_from_meet env_extension_from_meet
          in
(*
          let env =
            Typing_env_extension.fold new_env_extension_from_meet ~init:env
              ~f:(fun env name level ty ->
                let kind = kind ty in
                let ty = unknown kind in
                if Name.Map.mem name env.names_to_types then env
                else add_or_replace' env name level ty)
          in
*)
          let env =
            add_or_replace' env name level1 meet_ty
          in
          env_extension_from_meet, env)
        names_to_types_in_both
        (Typing_env_extension.create (), proto_result_env)
    in
    let existentials =
      Name.Set.union env1.existentials env2.existentials
    in
    let existential_freshening =
      env1.existential_freshening (* XXX *)
    in
    let env =
      { env with
        must_be_closed = env1.must_be_closed || env2.must_be_closed;
        resolver = env1.resolver;
        canonical_names_to_aliases;
        existentials;
        existential_freshening;
      }
    in
    invariant env;
    let env_extension_from_meet =
      Typing_env_extension.to_typing_environment ~resolver:env.resolver
        env_extension_from_meet
    in
    meet_typing_environment env env_extension_from_meet

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

  let join_typing_environment (t1 : typing_environment)
        (t2 : typing_environment) =
    let canonical_names_to_aliases =
      Name.Map.union_merge Name.Set.union
        t1.canonical_names_to_aliases
        t2.canonical_names_to_aliases
    in
    let names_to_types =
      Name.Map.inter_merge (fun (level1, ty1) (level2, ty2) ->
          if not (Scope_level.equal level1 level2) then begin
            Misc.fatal_errorf "join_typing_environment: \
                Scope levels differ for:@ %a@ and:@ %a"
              T.print ty1
              T.print ty2
          end;
          let ty = join (t1, ty1) (t2, ty2) in
(*
Format.eprintf "JOIN %a and %a -> %a\n%!"
print ty1
print ty2
print ty;
*)
          level1, ty)
        t1.names_to_types
        t2.names_to_types
    in
    let in_t1_only =
      Name.Map.filter (fun name _ ->
          not (Name.Map.mem name names_to_types))
        t1.names_to_types
    in
    let in_t2_only =
      Name.Map.filter (fun name _ ->
          not (Name.Map.mem name names_to_types))
        t2.names_to_types
    in
    let in_one_env_only =
      Name.Map.disjoint_union in_t1_only in_t2_only
    in
    let names_to_types =
      Name.Map.fold (fun name (scope_level, ty) names_to_types ->
          let ty = unknown (kind ty) in
          assert (not (Name.Map.mem name names_to_types));
          Name.Map.add name (scope_level, ty) names_to_types)
        in_one_env_only
        names_to_types
    in
    let all_levels_to_names =
      Scope_level.Map.union_merge
        (fun names1 names2 -> Name.Set.union names1 names2)
        t1.levels_to_names
        t2.levels_to_names
    in
    let levels_to_names =
      Scope_level.Map.map (fun names ->
          Name.Set.filter (fun name ->
              Name.Map.mem name names_to_types)
            names)
        all_levels_to_names
    in
    let existentials =
      Name.Set.union t1.existentials t2.existentials
    in
    let existential_freshening =
      t1.existential_freshening (* XXX *)
    in
    { resolver = t1.resolver;
      canonical_names_to_aliases;
      names_to_types;
      levels_to_names;
      existentials;
      existential_freshening;
    }

  let add_env_extension t env_extension =
    let t' =
      Typing_env_extension.to_typing_environment ~resolver:t.resolver
        env_extension
    in
    meet_typing_environment t t'

  let replace_meet env name (ty_env, ty) =
    match Name.Map.find name env.names_to_types with
    | exception Not_found ->
      Misc.fatal_errorf "Cannot meet types for name %a which is unbound \
          in the environment: %a"
        Name.print name
        print_typing_environment env
    | scope_level, existing_ty ->
      let meet_ty, new_env_extension =
        meet ~bias_towards:(env, existing_ty) (ty_env, ty)
      in
      let env = add_or_replace env name scope_level meet_ty in
      add_env_extension env new_env_extension

  let add_or_replace_meet t name scope_level ty =
    match Name.Map.find name t.names_to_types with
    | exception Not_found -> add t name scope_level ty
    | _ -> replace_meet t name (t, ty)
      (* CR mshinwell: We need to think about this some more.  Is [ty]
         supposed to only have free names in [t]? *)

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

  let mem t name =
    Name.Map.mem name t.names_to_types

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

  let to_env_extension t =
    let t =
      { t with
        must_be_closed = true;
      }
    in
    { typing_judgements = Some t;
    }

  let resolver t = t.resolver

  let equal ~equal_type
        ({ resolver = _;
           canonical_names_to_aliases = canonical_names_to_aliases1;
           names_to_types = names_to_types1;
           levels_to_names = _;
           existentials = existentials1;
           existential_freshening = _;
        } as t1)
        ({ resolver = _;
           canonical_names_to_aliases = canonical_names_to_aliases2;
           names_to_types = names_to_types2;
           levels_to_names = _;
           existentials = existentials2;
           existential_freshening = _;
        } as t2) =
    if t1 == t2 then true
    else
      let equal_scope_and_type (scope1, t1) (scope2, t2) =
        Scope_level.equal scope1 scope2 && equal_type t1 t2
      in
      Name.Map.equal equal_scope_and_type names_to_types1 names_to_types2
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
        let names = T.free_names t in
        loop (Name_occurrences.union to_follow names)
    in
    loop (free_names t);
    !all_names

  let free_names_transitive_list env ts =
    List.fold_left (fun names t ->
        Name_occurrences.union names (free_names_transitive env t))
      (Name_occurrences.create ())
      ts

  let diff ~strictly_more_precise t1 t2 : env_extension =
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
          add_or_replace t unbound level (unknown kind))
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

  let restrict_names_to_those_occurring_in_types t tys =
    let free_names = free_names_transitive_list t tys in
Format.eprintf "Restricting to: %a\n%!"
  Name_occurrences.print free_names;
    restrict_to_names t free_names

  let meet = meet_typing_environment
  let join = join_typing_environment
end
