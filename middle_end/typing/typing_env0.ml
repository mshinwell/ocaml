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

  module Typing_env_extension : Typing_env_extension_intf.S
    with type env_extension := env_extension
    with type typing_environment := typing_environment
    with type typing_environment_entry0 := typing_environment_entry0
    with type flambda_type := flambda_type
end) (Meet_and_join : sig
  include Meet_and_join_intf.S_for_types
    with type t_in_context := T.t_in_context
    with type env_extension := T.env_extension
    with type flambda_type := T.flambda_type
end) (Type_equality : sig
  include Type_equality_intf.S
    with type flambda_type := T.flambda_type
end) = struct
  type t = T.typing_environment

  open T

  type typing_environment = T.typing_environment
  type typing_environment_entry = T.typing_environment_entry
  type env_extension = T.Typing_env_extension.t
  type flambda_type = T.flambda_type
  type t_in_context = T.t_in_context
  type 'a ty = 'a T.ty
  type 'a unknown_or_join = 'a T.unknown_or_join

  type binding_type = Normal | Existential

  let print = print_typing_environment

  let fast_equal env1 env2 =
    env1 == env2
      || (Name.Map.is_empty env1.names_to_types
            && Name.Map.is_empty env2.names_to_types
            && Flambda_primitive.With_fixed_value.Map.is_empty
                 env1.cse_to_names
            && Flambda_primitive.With_fixed_value.Map.is_empty
                 env2.cse_to_names)

  let equal
        ({ resolver = _;
           names_to_types = names_to_types1;
           levels_to_entries = _;
           cse_to_names = cse_to_names1;
           next_sublevel_by_level = _;
           existentials = existentials1;
           existential_freshening = _;
        } as t1)
        ({ resolver = _;
           names_to_types = names_to_types2;
           levels_to_entries = _;
           cse_to_names = cse_to_names2;
           next_sublevel_by_level = _;
           existentials = existentials2;
           existential_freshening = _;
        } as t2) =
    if fast_equal t1 t2 then true
    else
      let equal_entry (entry1 : typing_environment_entry0)
            (entry2 : typing_environment_entry0) =
        match entry1, entry2 with
        | Definition ty1, Definition ty2 ->
          Type_equality.equal ty1 ty2
        | Equation ty1, Equation ty2 ->
          Type_equality.equal ty1 ty2
        | Definition _, Equation _
        | Equation _, Definition _ -> false
      in
      let equal_scope_and_entry (scope1, entry1) (scope2, entry2) =
        Scope_level.With_sublevel.equal scope1 scope2
          && equal_entry entry1 entry2
      in
      Name.Map.equal equal_scope_and_entry names_to_types1 names_to_types2
        && Flambda_primitive.With_fixed_value.Map.equal Name.equal
             cse_to_names1 cse_to_names2
        && Name.Set.equal existentials1 existentials2

  let create ~resolver =
    let existential_freshening = Freshening.activate Freshening.empty in
    { resolver;
      names_to_types = Name.Map.empty;
      cse_to_names = Flambda_primitive.With_fixed_value.Map.empty;
      levels_to_entries = Scope_level.Map.empty;
      next_sublevel_by_level = Scope_level.Map.empty;
      existentials = Name.Set.empty;
      existential_freshening;
    }

  let create_using_resolver_from t = create ~resolver:t.resolver

  let resolver t = t.resolver

  let is_empty t = Name.Map.is_empty t.names_to_types

  let domain t =
    let domain =
      Name.Set.diff (Name.Map.keys t.names_to_types) t.existentials
    in
    Name_occurrences.create_from_set_in_terms domain

  let find_exn t name =
    match Name.Map.find name t.names_to_types with
    | exception Not_found ->
      Misc.fatal_errorf "Cannot find %a in environment:@ %a"
        Name.print name
        print_typing_environment t
    | _scope_level, entry ->
      let binding_type =
        if Name.Set.mem name t.existentials then Existential
        else Normal
      in
      let ty =
        match entry with
        | Definition ty | Equation ty -> ty
      in
      match binding_type with
      | Normal -> ty, Normal
      | Existential ->
   (* XXX     let ty = rename_variables t freshening in *)
        ty, Existential

  type still_unresolved =
    | Resolved
    | Still_unresolved

  let resolve_aliases_on_ty0 (type a) t ?bound_name ~force_to_kind
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
          let ty, _binding_type = find_exn t name in
          continue_resolving ty ~canonical_name:(Some name)
        | Export_id export_id ->
          match t.resolver export_id with
          | Some ty -> continue_resolving ty ~canonical_name
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

  let resolve_aliases_on_ty t ?bound_name ~force_to_kind ty =
    let ty, canonical_name, _still_unresolved =
      resolve_aliases_on_ty0 t ?bound_name ~force_to_kind ty
    in
    ty, canonical_name

  let resolve_aliases_and_squash_unresolved_names_on_ty t ?bound_name
        ~force_to_kind ~unknown ty =
    let ty, canonical_name, still_unresolved =
      resolve_aliases_on_ty0 t ?bound_name ~force_to_kind ty
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

  (* CR mshinwell: It should be explicit in the code (maybe an invariant
     check on the end of this function) that if a canonical name is returned
     then the original type was an [Equals] or a [Type].  This fact
     should also be documented in the interface. *)
  let resolve_aliases ?bound_name (t, ty) : flambda_type * (Name.t option) =
    match ty.descr with
    | Value ty_value ->
      let force_to_kind = force_to_kind_value in
      let ty_value, canonical_name =
        resolve_aliases_on_ty t ?bound_name ~force_to_kind ty_value
      in
      { ty with descr = Value ty_value; }, canonical_name
    | Naked_number (ty_naked_number, kind) ->
      let force_to_kind = force_to_kind_naked_number kind in
      let ty_naked_number, canonical_name =
        resolve_aliases_on_ty t ?bound_name ~force_to_kind ty_naked_number
      in
      { ty with descr = Naked_number (ty_naked_number, kind); },
        canonical_name
    | Fabricated ty_fabricated ->
      let force_to_kind = force_to_kind_fabricated in
      let ty_fabricated, canonical_name =
        resolve_aliases_on_ty t ?bound_name ~force_to_kind ty_fabricated
      in
      { ty with descr = Fabricated ty_fabricated; }, canonical_name

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
      t.levels_to_entries
      init

  let iter t ~f =
    fold t ~init:() ~f:(fun () name binding_type scope_level ty ->
        f name binding_type scope_level ty)

  let invariant t =
    (* CR mshinwell: Add more checks here *)
    if !Clflags.flambda_invariant_checks then begin
      (* Since [fold] operates in (scope level, sublevel) order, then the
         following check establishes that dependencies between bindings in
         the environment are only in one direction. *)
      ignore (Sys.opaque_identity (fold t ~init:Name.Set.empty
        ~f:(fun names_seen (name : Name.t) (binding_type : binding_type)
                scope_level entry ->
          let free_names =
            match entry with
            | Definition ty | Equation ty -> T.free_names_set ty
            | CSE prim -> Flambda_primitive.With_fixed_value.free_names prim
          in
          if not (Name.Set.subset free_names names_seen) then begin
            Misc.fatal_errorf "Typing environment is not closed (%a free):@ %a"
              Name.Set.print (Name.Set.diff free_names names_seen)
              print t
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
                print t
          end;
          Name.Set.union free_names names_seen)) : Name.Set.t);
      (* Checking that alias resolution works also ensures there are no
         cycles via aliases. *)
      Name.Map.iter (fun bound_name (_level, entry) ->
          let ty =
            match (entry : typing_environment_entry0) with
            | Definition ty | Equation ty -> ty
          in
          ignore (Sys.opaque_identity (resolve_aliases ~bound_name (t, ty))))
        t.names_to_types
    end

  let mem t name =
    Name.Map.mem name t.names_to_types

  let scope_level_exn t name =
    match Name.Map.find name t.names_to_types with
    | exception Not_found ->
      Misc.fatal_errorf "scope_level: Cannot find %a in environment:@ %a"
        Name.print name
        print t
    | scope_level, _ty -> scope_level

  (* CR mshinwell: improve efficiency *)
  let find_with_scope_level_exn t name =
    let ty, binding_type = find_exn t name in
    let scope_level = scope_level_exn t name in
    ty, scope_level, binding_type

  let find_opt t name =
    match find_exn t name with
    | exception Not_found -> None
    | ty, binding_type -> Some (ty, binding_type)

  let find_cse t prim =
    match Flambda_primitive.With_fixed_value.create prim with
    | None -> None
    | Some prim ->
      match Flambda_primitive.With_fixed_value.Map.find prim t.cse_to_names with
      | exception Not_found -> None
      | name -> Some name

  let is_existential_exn t name =
    let _ty, binding_type = find_exn t name in
    match binding_type with
    | Normal -> false
    | Existential -> true

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
    let all_levels = Scope_level.Map.keys t.levels_to_entries in
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
      | Definition ty | Equation ty -> free_names_set ty
      | CSE prim -> Flambda_primitive.With_fixed_value.free_names prim
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

  let rec invariant_for_new_equation t name level (ty : flambda_type) ~sense =
    let existing_ty, _binding_type = find_exn t name in
    let meet_ty, env_extension =
      Meet_and_join.meet ~bound_name:name (t, existing_ty) (t, ty)
    in
    let ty_must_be_strictly_more_precise, other_ty =
      match sense with
      | New_equation_must_be_more_precise -> ty, existing_ty
      | Existing_equation_must_be_more_precise -> existing_ty, ty
    in
    let as_or_more_precise =
      Type_equality.equal meet_ty ty_must_be_strictly_more_precise
    in
    let strictly_more_precise =
      as_or_more_precise && not (Type_equality.equal meet_ty other_ty)
    in
    if not strictly_more_precise then begin
      Misc.fatal_errorf "Cannot add equation %a = %a@ to this environment: \
          as_or_more_precise %b,@ strictly_more_precise %b,@ meet_ty@ %a,@ \
          existing_ty@ %a,@ sense@ %a.@  Env:@ %a"
        Name.print name
        T.print ty
        as_or_more_precise
        strictly_more_precise
        T.print meet_ty
        T.print existing_ty
        print_sense sense
        print_typing_environment t
    end;
    Typing_env_extension.iter env_extension
      ~f:(fun name _binding_type level (entry : typing_environment_entry0) ->
        let ty =
          match entry with
          | Definition ty | Equation ty -> ty
        in
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

  let canonical_name t name =
    match find_opt t name with
    | None -> None
    | Some (ty, _binding_type) ->
      let _ty, canonical_name = resolve_aliases (t, ty) in
      match canonical_name with
      | None -> None
      | Some canonical_name ->
        begin
          if Name.equal name canonical_name then begin
            Misc.fatal_errorf "Canonical name for %a is itself in \
                environment:@ %a"
              Name.print name
              print_typing_environment t
          end;
          Some canonical_name
        end

  let add t (name : Name.t) cont_level (binding : typing_environment_entry) =
    invariant_for_new_binding t name cont_level binding;
    let t, sublevel = allocate_sublevel t cont_level in
    let level = Scope_level.With_sublevel.create cont_level sublevel in
    let redundant_possibly_cyclic_alias =
      match binding with
      | Definition _ | CSE _ -> false
      | Equation ty ->
        let canonical_name_for_t = canonical_name t name in
        let _, canonical_name = resolve_aliases (t, ty) in
        match canonical_name, canonical_name_for_t with
        | Some canonical_name, Some canonical_name_for_t
            when Name.equal canonical_name canonical_name_for_t -> true
        | Some _, Some _ -> assert false (* XXX *)
        | None, Some canonical_name_for_t
            when Name.equal name canonical_name_for_t -> true
        | None, Some _ | None, None | Some _, None -> false
    in
    if redundant_possibly_cyclic_alias then begin
      t
    end else begin
      let names_to_types =
        match binding with
        | Definition ty ->
          let entry0 : typing_environment_entry0 = Definition ty in
          Name.Map.add name (level, entry0) t.names_to_types
        | Equation ty ->
          let entry0 : typing_environment_entry0 = Equation ty in
          Name.Map.add name (level, entry0) t.names_to_types
        | CSE _ -> t.names_to_types
      in
      let levels_to_entries =
        Scope_level.Map.update cont_level
          (function
            | None ->
              let by_sublevel =
                Scope_level.Sublevel.Map.singleton sublevel (name, binding)
              in
              Some by_sublevel
            | Some by_sublevel ->
              assert (not (Scope_level.Sublevel.Map.mem sublevel by_sublevel));
              let by_sublevel =
                Scope_level.Sublevel.Map.add sublevel (name, binding)
                  by_sublevel
              in
              Some by_sublevel)
          t.levels_to_entries
      in
      let cse_to_names =
        match binding with
        | Definition _ | Equation _ -> t.cse_to_names
        | CSE prim ->
          match
            Flambda_primitive.With_fixed_value.Map.find prim t.cse_to_names
          with
          | exception Not_found ->
            Flambda_primitive.With_fixed_value.Map.singleton prim name
          | _name -> t.cse_to_names  (* Keep the furthest-out binding. *)
      in
      let t =
        { t with
          names_to_types;
          levels_to_entries;
          cse_to_names;
        }
      in
      invariant t;
      t
    end

  let singleton ~resolver name scope_level binding =
    add (create ~resolver) name scope_level binding

  let restrict_to_names0 t allowed =
    let names_to_types =
      Name.Map.filter (fun name _ty -> Name.Set.mem name allowed)
        t.names_to_types
    in
    let levels_to_entries =
      Scope_level.Map.filter_map t.levels_to_entries
        ~f:(fun _cont_level by_sublevel ->
          let by_sublevel =
            Scope_level.Sublevel.Map.filter_map by_sublevel
              ~f:(fun _sublevel ((name, _) as entry) ->
                if Name.Set.mem name allowed then Some entry
                else None)
          in
          if Scope_level.Sublevel.Map.is_empty by_sublevel then None
          else Some by_sublevel)
    in
    let existentials = Name.Set.inter t.existentials allowed in
    let existential_freshening =
      Freshening.restrict_to_names t.existential_freshening allowed
    in
    let cse_to_names =
      Flambda_primitive.With_fixed_value.Map.filter (fun prim name ->
          let names_in_prim =
            Flambda_primitive.With_fixed_value.free_names prim
          in
          let names = Name.Set.add name names_in_prim in
          Name.Set.is_empty (Name.Set.diff names allowed))
        t.cse_to_names
    in
    let t =
      { resolver = t.resolver;
        names_to_types;
        cse_to_names;
        levels_to_entries;
        next_sublevel_by_level = t.next_sublevel_by_level;
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

  let restrict_to_names t allowed =
    let allowed = Name_occurrences.everything allowed in
    restrict_to_names0 t allowed

  let restrict_to_symbols t =
    let symbols = Name.symbols_only_map t.names_to_types in
    restrict_to_names0 t (Name.Map.keys symbols)

  let filter t ~f =
    let allowed =
      Name.Map.fold (fun name ty allowed ->
          if f name ty then Name.Set.add name allowed
          else allowed)
        t.names_to_types
        Name.Set.empty
    in
    restrict_to_names0 t allowed

  let remove t name =
    let allowed = Name.Set.remove name (Name.Map.keys t.names_to_types) in
    restrict_to_names0 t allowed

  let meet (t1 : typing_environment) (t2 : typing_environment)
        : typing_environment =
    if fast_equal t1 t2 then t1
    else if is_empty t1 then t2
    else if is_empty t2 then t1
    else
      let resolver = t1.resolver in
      let t =
        Scope_level.Map.fold_intersection_and_remainder
          t1.levels_to_entries t2.levels_to_entries
          ~init:(create ~resolver)
          ~inter:(fun cont_level by_sublevel1 by_sublevel2 t ->
            Scope_level.Sublevel.fold_intersection_and_remainder
              by_sublevel1 by_sublevel2
              ~init:t
              ~inter:(fun sublevel (name1, ty) (name2, ty2) t ->
                assert (Name.equal name1 name2);
                let meet_ty, env_extension =
                  Meet_and_join.meet (t1, ty1) (t2, ty2)
                in
                let binding_type = binding_type t name1 in
                let t =
                  add_with_binding_type t name binding_type
                    cont_level sublevel (Definition meet_ty)
                in
                Typing_env_extension.fold env_extension
                  ~init:t
                  ~f:(fun t name binding_type cont_level sublevel ty ->
                    add_with_binding_type t name binding_type
                      cont_level sublevel (Equation ty)))
              ~rem:(fun t t1_or_t2 sublevel (name, ty) ->
                let binding_type = binding_type t1_or_t2 name in
                add_with_binding_type t name binding_type
                  cont_level sublevel (Definition ty)))
          ~rem:(fun t t1_or_t2 cont_level by_sublevel ->
            Scope_level.Sublevel.fold (fun sublevel (name, ty) t ->
                let binding_type = binding_type t1_or_t2 name in
                add_with_binding_type t name binding_type
                  cont_level sublevel (Definition ty))
              by_sublevel
              t)
      in
      let cse_to_names =
        Flambda_primitive.With_fixed_value.Map.union_merge
          (fun name1 name2 ->
            let level1 = scope_level_exn t1 name1 in
            let level2 = scope_level_exn t2 name2 in
            (* Keep the outermost binding. *)
            if Scope_level.With_sublevel.(>) level1 level2 then name2
            else name1)
          t1.cse_to_names t2.cse_to_names
      in
      let existentials = Name.Set.union t1.existentials t2.existentials in
      let existential_freshening = t1.existential_freshening (* XXX *) in
      let t =
        { t with
          cse_to_names;
          existentials;
          existential_freshening;
        }
      in
      invariant t;
      t

  let join (t1 : typing_environment) (t2 : typing_environment)
        : typing_environment =
    if fast_equal t1 t2 then t1
    else if is_empty t1 then create_using_resolver_from t1
    else if is_empty t2 then create_using_resolver_from t1
    else
      let t =
        Scope_level.Map.intersection_fold
          t1.levels_to_entries t2.levels_to_entries
          ~init:(create ~resolver)
          ~inter:(fun cont_level by_sublevel1 by_sublevel2 t ->
            Scope_level.Sublevel.intersection_fold
              by_sublevel1 by_sublevel2
              ~init:t
              ~inter:(fun sublevel (name1, ty) (name2, ty2) t ->
                assert (Name.equal name1 name2);
                let join_ty =
                  Meet_and_join.join (t1, ty1) (t2, ty2)
                in
                let binding_type = binding_type t name1 in
                add_with_binding_type t name binding_type
                  cont_level sublevel (Definition join_ty)))
      in
      let cse_to_names =
        Flambda_primitive.With_fixed_value.Map.inter_merge
          (fun name1 name2 ->
            let level1 = scope_level_exn t1 name1 in
            let level2 = scope_level_exn t2 name2 in
            (* Keep the outermost binding. *)
            if Scope_level.With_sublevel.(>) level1 level2 then name2
            else name1)
          t1.cse_to_names t2.cse_to_names
      in
      let existentials =
        Name.Set.filter (fun name -> Name.Map.mem name names_to_types)
          Name.Set.union t1.existentials t2.existentials
      in
      let existential_freshening = t1.existential_freshening (* XXX *) in
      let t =
        { t with
          names_to_types;
          cse_to_names;
          levels_to_entries;
          existentials;
          existential_freshening;
        }
      in
      invariant t;
      t

  let add_env_extension t env_extension =
    let t' =
      Typing_env_extension.to_typing_environment ~resolver:t.resolver
        env_extension
    in
    meet t t'

  let to_env_extension t : env_extension =
    { typing_judgements = Some t;
    }

  let cut t ~existential_if_defined_at_or_later_than =
(*
Format.eprintf "Cutting environment at %a: %a\n%!"
Scope_level.print existential_if_defined_at_or_later_than
print_typing_environment t;
*)
    let new_existentials =
      Scope_level.Map.fold
        (fun scope_level by_sublevel resulting_existentials ->
          let will_be_existential =
            Scope_level.(>=)
              scope_level existential_if_defined_at_or_later_than
          in
          if will_be_existential then
            let names =
              Name.Set.of_list (
                List.map (fun (name, _ty) -> name)
                  (Scope_level.Sublevel.Map.data by_sublevel))
            in
            let non_symbols = Name.variables_only names in
            Name.Set.union non_symbols resulting_existentials
          else
            resulting_existentials)
        t.levels_to_entries
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
    let existentials = Name.Set.union t.existentials new_existentials in
    let cse_to_names =
      (* CSE equations may never reference existentially-bound names. *)
      Flambda_primitive.With_fixed_value.Map.filter (fun prim name ->
          let names_in_prim =
            Flambda_primitive.With_fixed_value.free_names prim
          in
          let names = Name.Set.add name names_in_prim in
          Name.Set.is_empty (Name.Set.inter names existentials))
        t.cse_to_names
    in
    let t =
      (* XXX fix freshening as required *)
      { resolver = t.resolver;
        names_to_types = t.names_to_types;
        levels_to_entries = t.levels_to_entries;
        cse_to_names;
        existentials;
        existential_freshening;
      }
    in
(*
Format.eprintf "Result is: %a\n%!" print t
*)
    invariant t;
    t

  let free_names_transitive t ty =
    let all_names = ref (Name_occurrences.create ()) in
    let rec loop to_follow =
      all_names := Name_occurrences.union !all_names to_follow;
      match Name_occurrences.choose_and_remove_amongst_everything to_follow with
      | None -> ()
      | Some (name, to_follow) ->
        let ty, _binding_type = find t name in
        let names = T.free_names ty in
        loop (Name_occurrences.union to_follow names)
    in
    loop (free_names ty);
    !all_names

  let free_names_transitive_list t tys =
    List.fold_left (fun names ty ->
        Name_occurrences.union names (free_names_transitive t ty))
      (Name_occurrences.create ())
      tys

  let restrict_names_to_those_occurring_in_types t tys =
    let free_names = free_names_transitive_list t tys in
    restrict_to_names t free_names

  let diff t1 t2 : env_extension =
    let names_to_types =
      Name.Map.filter (fun name (level1, ty1) ->
          match Name.Map.find name t2.names_to_types with
          | exception Not_found -> true
          | (level2, ty2) ->
            assert (Scope_level.equal level1 level2);
            Meet_and_join.strictly_more_precise (t1, ty1) ~than:(t2, ty2))
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
    let t = restrict_to_names0 t1 (Name.Map.keys names_to_types) in
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
    { typing_judgements = Some t;
    }
end
