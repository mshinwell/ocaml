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
end) (Typing_env_extension : sig
  include Typing_env_extension_intf.S
    with type env_extension := T.env_extension
    with type typing_environment := T.typing_environment
    with type flambda_type := T.flambda_type
end) (Meet_and_join : sig
  include Meet_and_join_intf.S_for_types
    with type typing_environment := T.typing_environment
    with type env_extension := T.env_extension
    with type flambda_type := T.flambda_type
end) (Type_equality : sig
  include Type_equality_intf.S
    with type flambda_type := T.flambda_type
end) = struct
  open T

  type t = T.typing_environment

  type typing_environment = T.typing_environment
  type typing_environment_entry = T.typing_environment_entry
  type typing_environment_entry0 = T.typing_environment_entry0
  type env_extension = Typing_env_extension.t
  type flambda_type = T.flambda_type
  type t_in_context = T.t_in_context
  type 'a ty = 'a T.ty
  type 'a unknown_or_join = 'a T.unknown_or_join

  let print = print_typing_environment

  let create ~resolver =
    { resolver;
      aliases = Simple.Map.empty;
      names_to_types = Name.Map.empty;
      cse = Flambda_primitive.With_fixed_value.Map.empty;
      levels_to_entries = Scope_level.Map.empty;
      next_sublevel_by_level = Scope_level.Map.empty;
      were_existentials = Name.Set.empty;
    }

  let create_using_resolver_from t = create ~resolver:t.resolver

  let resolver t = t.resolver

  let is_empty t = Name.Map.is_empty t.names_to_types

  let domain t =
    Name_occurrences.create_from_set_in_terms
      (Name.Map.keys t.names_to_types)

  let find_exn t name
        : flambda_type * Flambda_type0_internal_intf.binding_type =
    (* CR mshinwell: Maybe this should cause a fatal error and we shouldn't
       rely on catching the exception *)
    let _scope_level, entry = Name.Map.find name t.names_to_types in
    let binding_type : Flambda_type0_internal_intf.binding_type =
      if Name.Set.mem name t.were_existentials then Was_existential
      else Normal
    in
    let ty =
      match entry with
      | Definition ty | Equation ty -> ty
    in
    ty, binding_type

  type still_unresolved =
    | Resolved
    | Still_unresolved

  let resolve_aliases_on_ty0 (type a) t ?bound_name ~force_to_kind
        (ty : a ty)
        : (a ty) * (Name.t option) * Name_or_export_id.Set.t
            * still_unresolved =
    let rec resolve_aliases names_seen ~canonical_name (ty : a ty) =
      let resolve (name : Name_or_export_id.t) : _ * _ * _ * still_unresolved =
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
          | None -> ty, None, Name_or_export_id.Set.empty, Still_unresolved
      in
      match ty with
      | No_alias _ -> ty, canonical_name, names_seen, Resolved
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
    let ty, canonical_name, names_seen, _still_unresolved =
      resolve_aliases_on_ty0 t ?bound_name ~force_to_kind ty
    in
    ty, canonical_name, names_seen

  let resolve_aliases_and_squash_unresolved_names_on_ty' env ?bound_name
        ~print_ty ~force_to_kind ty
        : _ unknown_or_join * (Name.t option) =
    let ty, canonical_name, _names_seen, _still_unresolved =
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
      let ty_value, canonical_name, _names_seen =
        resolve_aliases_on_ty t ?bound_name ~force_to_kind ty_value
      in
      { ty with descr = Value ty_value; }, canonical_name
    | Naked_number (ty_naked_number, kind) ->
      let force_to_kind = force_to_kind_naked_number kind in
      let ty_naked_number, canonical_name, _names_seen =
        resolve_aliases_on_ty t ?bound_name ~force_to_kind ty_naked_number
      in
      { ty with descr = Naked_number (ty_naked_number, kind); },
        canonical_name
    | Fabricated ty_fabricated ->
      let force_to_kind = force_to_kind_fabricated in
      let ty_fabricated, canonical_name, _names_seen =
        resolve_aliases_on_ty t ?bound_name ~force_to_kind ty_fabricated
      in
      { ty with descr = Fabricated ty_fabricated; }, canonical_name

  let fold_all t ~init ~f =
    Scope_level.Map.fold (fun level by_sublevel acc ->
        Scope_level.Sublevel.Map.fold (fun sublevel (name, entry) acc ->
          let scope_level = Scope_level.With_sublevel.create level sublevel in
          let binding_type : Flambda_type0_internal_intf.binding_type =
            if Name.Set.mem name t.were_existentials then Was_existential
            else Normal
          in
          f acc name binding_type scope_level entry)
        by_sublevel
        acc)
      t.levels_to_entries
      init

  let fold t ~init ~f =
    Scope_level.Map.fold (fun level by_sublevel acc ->
        Scope_level.Sublevel.Map.fold (fun sublevel (name, entry) acc ->
          let entry0 =
            match entry with
            | CSE _ -> None
            | Definition ty ->
              Some ((Definition ty) : typing_environment_entry0)
            | Equation ty ->
              Some ((Equation ty) : typing_environment_entry0)
          in
          match entry0 with
          | None -> acc
          | Some entry0 ->
            let scope_level = Scope_level.With_sublevel.create level sublevel in
            let binding_type : Flambda_type0_internal_intf.binding_type =
              if Name.Set.mem name t.were_existentials then Was_existential
              else Normal
            in
            f acc name binding_type scope_level entry0)
        by_sublevel
        acc)
      t.levels_to_entries
      init

  let iter t ~f =
    fold t ~init:() ~f:(fun () name binding_type scope_level entry ->
        f name binding_type scope_level entry)

  let invariant t =
    (* CR mshinwell: Add more checks here *)
    if !Clflags.flambda_invariant_checks then begin
      (* Since [fold] operates in (scope level, sublevel) order, then the
         following check establishes that dependencies between bindings in
         the environment are only in one direction. *)
      ignore (fold_all t ~init:Name.Set.empty
        ~f:(fun names_seen (name : Name.t)
                (_binding_type : Flambda_type0_internal_intf.binding_type)
                _scope_level entry ->
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
          match entry with
          | Definition _ -> Name.Set.add name names_seen
          | Equation _ | CSE _ -> names_seen) : Name.Set.t);
      (* Checking that alias resolution works also ensures there are no
         cycles via aliases. *)
      Name.Map.iter (fun bound_name (_level, entry) ->
          let ty =
            match (entry : typing_environment_entry0) with
            | Definition ty | Equation ty -> ty
          in
          ignore (Sys.opaque_identity (resolve_aliases ~bound_name (t, ty))))
        t.names_to_types;
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

  let find_cse (t : t) prim =
    match Flambda_primitive.With_fixed_value.create prim with
    | None -> None
    | Some prim ->
      match Flambda_primitive.With_fixed_value.Map.find prim t.cse with
      | exception Not_found -> None
      | name -> Some name

  let was_existential_exn t name =
    let _ty, binding_type = find_exn t name in
    match binding_type with
    | Normal -> false
    | Was_existential -> true

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

  let _min_level_for_new_binding t =
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

  let invariant_for_any_new_binding t name _level
        (entry : typing_environment_entry) =
    let free_names =
      match entry with
      | Definition ty | Equation ty -> free_names_set ty
      | CSE prim -> Flambda_primitive.With_fixed_value.free_names prim
    in
    if Name.Set.mem name free_names then begin
      Misc.fatal_errorf "Cannot add binding@ %a = %a@ as it would produce \
          a circular dependency"
        Name.print name
        print_typing_environment_entry entry
    end;
    (* CR mshinwell: Unsure about levels for symbols yet
    ...well, we need to add them with a lower level than the current one...
    let min_level = min_level_for_new_binding t in
    if (not (Scope_level.equal level Scope_level.for_symbols))
      && Scope_level.(<) level min_level
    then begin
      Misc.fatal_errorf "Cannot add binding@ %a = %a@ to this environment \
          with scope level %a (minimum permitted level %a):@ %a"
        Name.print name
        print_typing_environment_entry entry
        Scope_level.print level
        Scope_level.print min_level
        print_typing_environment t
    end;
*)
    match find_opt t name with
    | None ->
      begin match entry with
      | Definition _ | CSE _ -> ()
      | Equation _ ->
        Misc.fatal_errorf "Cannot add@ %a = %a@ for name undefined in \
            environment:@ %a"
          Name.print name
          print_typing_environment_entry entry
          print_typing_environment t
      end
    | Some _ ->
      match entry with
      | Definition _ ->
        Misc.fatal_errorf "Cannot redefine@ %a = %a@ in environment:@ %a"
          Name.print name
          print_typing_environment_entry entry
          print_typing_environment t
      | Equation _ | CSE _ -> ()

  let invariant_for_new_equation t name (ty : flambda_type) ~sense =
    let existing_ty, _binding_type = find_exn t name in
    let meet_ty, _env_extension = Meet_and_join.meet t existing_ty ty in
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
    end
(* XXX Not sure about this part
    Typing_env_extension.iter env_extension
      ~f:(fun name _binding_type level (entry : typing_environment_entry0) ->
        let ty =
          match entry with
          | Definition ty | Equation ty -> ty
        in
        let level = Scope_level.With_sublevel.level level in
        invariant_for_any_new_binding t name level (Equation ty);
        invariant_for_new_equation t name ty
          ~sense:Existing_equation_must_be_more_precise)
*)
  let _ = ignore Existing_equation_must_be_more_precise

  let invariant_for_new_binding t name level
        (entry : typing_environment_entry) =
    invariant_for_any_new_binding t name level entry;
    match entry with
    | Definition _ | CSE _ -> ()
    | Equation ty ->
      invariant_for_new_equation t name ty
        ~sense:New_equation_must_be_more_precise

(*
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
*)

  let aliases_of_simple (t : t) (simple : Simple.t) =
    match Simple.Map.find simple t.aliases with
    | exception Not_found ->
      begin match simple with
      | Const _ | Discriminant _ -> Name.Set.empty
      | Name name ->
        Misc.fatal_errorf "Typing_env.aliases_of_name: unbound name %a"
          Name.print name
      end
    | aliases -> aliases

  let add t (name : Name.t) cont_level (binding : typing_environment_entry) =
    invariant_for_new_binding t name cont_level binding;
    let alias =
      match binding with
      | Definition ty | Equation ty -> T.get_alias ty
      | CSE _ -> None
    in
    let equation_with_reverse_alias_already_present =
      match binding with
      | Equation _ ->
        begin match alias with
        | None -> false
        | Some alias ->
          Name.Set.mem alias (aliases_of_simple t (Simple.name name))
        end
      | Definition _ | CSE _ -> false
    in
    if equation_with_reverse_alias_already_present then begin
      t
    end else begin
      let aliases =
        match alias with
        | None -> t.aliases
        | Some alias ->
          Simple.Map.update (Simple.name alias) (function
              | None -> Some (Name.Set.singleton name)
              | Some aliases -> Some (Name.Set.add name aliases))
            t.aliases
      in
      let t, sublevel = allocate_sublevel t cont_level in
      let level = Scope_level.With_sublevel.create cont_level sublevel in
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
      let cse =
        match binding with
        | Definition _ | Equation _ -> t.cse
        | CSE prim ->
          match
            Flambda_primitive.With_fixed_value.Map.find prim t.cse
          with
          | exception Not_found ->
            Flambda_primitive.With_fixed_value.Map.singleton prim
              (Simple.name name)
          | _name -> t.cse  (* Keep the furthest-out binding. *)
      in
      let t =
        { t with
          aliases;
          names_to_types;
          levels_to_entries;
          cse;
        }
      in
      invariant t;
      t
    end

  let singleton ~resolver name scope_level binding =
    add (create ~resolver) name scope_level binding

  let max_level t =
    match Scope_level.Map.max_binding_opt t.levels_to_entries with
    | None -> Scope_level.initial
    | Some (level, _) -> level

  let restrict_to_names0 (t : t) allowed =
    let aliases =
      Simple.Map.filter_map (fun (simple : Simple.t) aliases ->
          let aliases =
            Name.Set.filter (fun name -> Name.Set.mem name allowed) aliases
          in
          match simple with
          | Name name ->
            if Name.Set.mem name allowed then Some aliases
            else None
          | Const _ | Discriminant _ -> Some aliases)
        t.aliases
    in
    let names_to_types =
      Name.Map.filter (fun name _ty -> Name.Set.mem name allowed)
        t.names_to_types
    in
    let levels_to_entries =
      Scope_level.Map.filter_map (fun _cont_level by_sublevel ->
          let by_sublevel =
            Scope_level.Sublevel.Map.filter_map
              (fun _sublevel ((name, _) as entry) ->
                if Name.Set.mem name allowed then Some entry
                else None)
            by_sublevel
          in
          if Scope_level.Sublevel.Map.is_empty by_sublevel then None
          else Some by_sublevel)
        t.levels_to_entries
    in
    let were_existentials = Name.Set.inter t.were_existentials allowed in
    let cse =
      Flambda_primitive.With_fixed_value.Map.filter
        (fun prim (simple : Simple.t) ->
          let names_in_prim =
            Flambda_primitive.With_fixed_value.free_names prim
          in
          let names =
            match simple with
            | Name name -> Name.Set.add name names_in_prim
            | Const _ | Discriminant _ -> names_in_prim
          in
          Name.Set.is_empty (Name.Set.diff names allowed))
        t.cse
    in
    let t =
      { resolver = t.resolver;
        aliases;
        names_to_types;
        cse;
        levels_to_entries;
        next_sublevel_by_level = t.next_sublevel_by_level;
        were_existentials;
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

  (* XXX [don't_freshen] needs sorting properly *)
  let rec add_or_meet_env_extension' ?don't_freshen ?freshening
        t env_extension scope_level =
    let original_t = t in
    let original_freshening = freshening in
    let rename_name (name : Name.t) freshening =
      match Name.Map.find name freshening with
      | exception Not_found -> name
      | name -> name
    in
    let rename_simple (simple : Simple.t) freshening =
      match simple with
      | Name name -> Simple.name (rename_name name freshening)
      | Const _ | Discriminant _ -> simple
    in
    let add_equation t freshening name ty =
      let name = rename_name name freshening in
      let ty = T.rename_variables ty freshening in
      match find_opt t name with
      | None -> add t name scope_level (Equation ty)
      | Some (existing_ty, _binding_type) ->
        let meet =
          let meet_ty, meet_env_extension =
            Meet_and_join.meet t ty existing_ty
          in
          let as_or_more_precise = Type_equality.equal meet_ty ty in
          let strictly_more_precise =
            as_or_more_precise && not (Type_equality.equal meet_ty existing_ty)
          in
          if strictly_more_precise then Some (meet_ty, meet_env_extension)
          else None
        in
        match meet with
        | None -> t
        | Some (new_ty, new_env_extension)->
          let t, _freshening =
            add_or_meet_env_extension' ?don't_freshen
              ?freshening:original_freshening
              t new_env_extension scope_level
          in
          add t name scope_level (Equation new_ty)
    in
    let add_definition t freshening (name : Name.t) ty =
      let ty = T.rename_variables ty freshening in
      let freshening, fresh_name =
        match don't_freshen with
        | None ->
          let fresh_name = Name.rename name in
          let freshening = Name.Map.add name fresh_name freshening in
          freshening, fresh_name
        | Some () ->
          freshening, name
      in
(*
Format.eprintf "Opening existential %a -> %a\n%!"
  Name.print name Name.print fresh_name;
*)
      if mem t fresh_name then
        freshening, add_equation t freshening name ty
      else
        let t = add t fresh_name scope_level (Definition ty) in
        let t =
          match don't_freshen with
          | None ->
            begin match fresh_name with
            | Var _ -> ()
            | Symbol sym ->
              Misc.fatal_errorf "Definitions of symbols should never occur \
                  in environment extensions: symbol %a, env@ %a,@ \
                  env_extension@ %a"
                Symbol.print sym
                print original_t
                Typing_env_extension.print env_extension
            end;
            { t with
              were_existentials = Name.Set.add fresh_name t.were_existentials;
            }
          | Some () -> t
        in
        freshening, t
    in
    let add_cse (t : t) freshening bound_to prim =
      let bound_to = rename_simple bound_to freshening in
      let prim =
        Flambda_primitive.With_fixed_value.rename_names prim
          freshening
      in
      match
        Flambda_primitive.With_fixed_value.Map.find prim t.cse
      with
      | exception Not_found ->
        let t =
          match bound_to with
          | Name bound_to -> add t bound_to scope_level (CSE prim)
          | Const _ | Discriminant _ -> t
        in
        let cse =
          Flambda_primitive.With_fixed_value.Map.add prim bound_to t.cse
        in
        { t with cse; }
      | _bound_to ->
        (* As above, keep the outer binding. *)
        t
    in
    let freshening, t =
      let freshening =
        match freshening with
        | None -> Name.Map.empty
        | Some freshening -> Freshening.name_substitution freshening
      in
      List.fold_left (fun (freshening, t) (name, ty) ->
          add_definition t freshening name ty)
        (freshening, t)
        (List.rev env_extension.first_definitions)
    in
    let freshening, t =
      Scope_level.Map.fold
        (fun _level by_sublevel (freshening, t) ->
          Scope_level.Sublevel.Map.fold
            (fun _sublevel
                 ((name : Name.t), (entry : typing_environment_entry))
                 (freshening, t) ->
              match entry with
              | Definition ty -> add_definition t freshening name ty
              | Equation ty ->
                let t = add_equation t freshening name ty in
                freshening, t
              | CSE prim ->
                let t = add_cse t freshening (Simple.name name) prim in
                freshening, t)
            by_sublevel
            (freshening, t))
        env_extension.at_or_after_cut_point
        (freshening, t)
    in
    let t =
      List.fold_left (fun t (name, ty) ->
          add_equation t freshening name ty)
        t
        env_extension.last_equations_rev
    in
    let t =
      Flambda_primitive.With_fixed_value.Map.fold (fun prim bound_to t ->
          add_cse t freshening bound_to prim)
        env_extension.cse
        t
    in
    t, freshening

  let add_or_meet_env_extension t env_extension scope_level =
    let t, _freshening =
      add_or_meet_env_extension' t env_extension scope_level
    in
    t

  let add_equation t name scope_level ty =
    if not (mem t name) then begin
      Misc.fatal_errorf "Typing_env.replace_meet: name %a not bound in:@ %a"
        Name.print name
        print t
    end;
    let env_extension =
      Typing_env_extension.add_equation Typing_env_extension.empty name ty
    in
    add_or_meet_env_extension t env_extension scope_level

  let cut t ~existential_if_defined_at_or_later_than : env_extension =
    (* CR mshinwell: Add a split which only returns one map, the side we
       would like. *)
    let _before_cut_point, at_cut_point, after_cut_point =
      Scope_level.Map.split existential_if_defined_at_or_later_than
        t.levels_to_entries
    in
    let at_or_after_cut_point =
      match at_cut_point with
      | None -> after_cut_point
      | Some by_sublevel ->
        Scope_level.Map.add existential_if_defined_at_or_later_than
          by_sublevel after_cut_point
    in
    { first_definitions = [];
      at_or_after_cut_point;
      last_equations_rev = [];
      cse = Flambda_primitive.With_fixed_value.Map.empty;
    }
end
