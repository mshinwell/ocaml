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

type binding_type = Normal | Was_existential

type cached = {
  names_to_types : Flambda_types.t Name.Map.t;
  cse : Simple.t Flambda_primitive.With_fixed_value.Map.t;
  were_existentials : Name.Set.t;
  aliases : Name.Set.t Simple.Map.t;
}

type one_level = {
  level : Typing_env_level.t;
  just_after_level : cached;
}

type t = {
  resolver : (Export_id.t -> Flambda_types.t option);
  prev_levels : one_level Scope_level.Map.t;
  (* CR mshinwell: Rename [Scope_level] to [Scope] and remove this
     pair. *)
  current_level : Scope_level.t * one_level;
}

let is_empty t =
  Typing_env_level.is_empty ((snd t.current_level).level)
    && Scope_level.Map.for_all (fun _level (one_level : one_level) ->
           Typing_env_level.is_empty one_level.level)
         t.prev_levels

let _print_cached ~cache ppf
      { names_to_types; cse; were_existentials; aliases; } =
  Format.fprintf ppf
    "@[<hov 1>(\
        @[<hov 1>(names_to_types@ %a)@]@ \
        @[<hov 1>(cse@ %a)@]@ \
        @[<hov 1>(were_existentials@ %a)@]@ \
        @[<hov 1>(aliases@ %a)@])@]"
    (Name.Map.print (Type_printers.print_with_cache ~cache)) names_to_types
    (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
    Name.Set.print were_existentials
    (Simple.Map.print Name.Set.print) aliases

let print_one_level ~cache:_ ppf { level; just_after_level = _; } =
  Typing_env_level.print ppf level

let print_with_cache ~cache ppf
      ({ resolver = _; prev_levels; current_level; } as t) =
  if is_empty t then
    Format.pp_print_string ppf "Empty"
  else
    Printing_cache.with_cache cache ppf "env" t (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(\
            @[<hov 1>(prev_levels@ %a)@]@ \
            @[<hov 1>(current_level@ (%a@ %a))@])@]"
        (Scope_level.Map.print (print_one_level ~cache)) prev_levels
        Scope_level.print (fst current_level)
        (print_one_level ~cache) (snd current_level))

(*
      (* CR mshinwell: Add flag to disable this filtering *)
      let names_to_levels =
        Name.Map.filter (fun name _entry ->
            not (Name.is_predefined_exception name))
          names_to_levels
      in
      let levels_to_entries =
        Scope_level.Map.filter_map (fun _cont_level by_sublevel ->
            let by_sublevel =
              Scope_level.Sublevel.Map.filter_map
                (fun _sublevel ((name, _) as entry) ->
                  if not (Name.is_predefined_exception name) then Some entry
                  else None)
              by_sublevel
            in
            if Scope_level.Sublevel.Map.is_empty by_sublevel then None
            else Some by_sublevel)
          levels_to_entries
      in
      if Name.Set.is_empty just_after_current_level.were_existentials
          && Flambda_primitive.With_fixed_value.Map.is_empty
               just_after_current_level.cse
      then
        Format.fprintf ppf
          "@[<hov 1>(\
              @[<hov 1>(aliases@ %a)@]@ \
              @[<hov 1>(levels@ %a)@]@ \
              @[<hov 1>(names_to_types@ %a)@])@]"
          (Simple.Map.print Name.Set.print) aliases
          (Scope_level.Map.print Typing_env_level.print) levels
          (Name.Map.print (Type_printers.print_with_cache ~cache)
            names_to_types)
      else if Name.Set.is_empty were_existentials then
        Format.fprintf ppf
          "@[<hov 1>(\
              @[<hov 1>(aliases@ %a)@]@ \
              @[<hov 1>(levels@ %a)@]@ \
              @[<hov 1>(cse@ %a)@]@ \
              @[<hov 1>(names_to_types@ %a)@])@]"
          (Simple.Map.print Name.Set.print) aliases
          (Scope_level.Map.print Typing_env_level.print) levels
          (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
          (Name.Map.print (Type_printers.print_with_cache ~cache)
            names_to_types)
      else
        Format.fprintf ppf
          "@[<hov 1>(\
              @[<hov 1>(aliases@ %a)@]@ \
              @[<hov 1>(levels@ %a)@]@ \
              @[<hov 1>(cse@ %a)@]@ \
              @[<hov 1>(names_to_types@ %a)@]@ \
              @[<hov 1>(were_existentials@ %a)@])@]"
          (Simple.Map.print Name.Set.print) aliases
          (Scope_level.Map.print Typing_env_level.print) levels
          (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
          (Name.Map.print (Type_printers.print_with_cache ~cache)
            names_to_types)
          Name.Set.print were_existentials)
*)

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let empty_one_level () =
  let just_after_level =
    { names_to_types = Name.Map.empty;
      cse = Flambda_primitive.With_fixed_value.Map.empty;
      were_existentials = Name.Set.empty;
      aliases = Simple.Map.empty;
    }
  in
  { level = Typing_env_level.empty ();
    just_after_level;
  }

let create ~resolver =
  { resolver;
    prev_levels = Scope_level.Map.empty;
    current_level = (Scope_level.initial, empty_one_level ());
  }

let create_using_resolver_from t = create ~resolver:t.resolver

let resolver t = t.resolver

let increment_scope_level_to t level =
  if Scope_level.(<=) level (fst t.current_level) then begin
    Misc.fatal_errorf "Invalid new level %a:@ %a"
      Scope_level.print level
      print t
  end;
  let one_level =
    { level = Typing_env_level.empty ();
      just_after_level = (snd t.current_level).just_after_level;
    }
  in
  let current_level = Scope_level.next (fst t.current_level) in
  let prev_levels =
    Scope_level.Map.add current_level (snd t.current_level) t.prev_levels
  in
  { t with
    prev_levels;
    current_level = (level, one_level);
  }

let increment_scope_level t =
  increment_scope_level_to t (Scope_level.next (fst t.current_level))

let fast_equal t1 t2 =
  t1 == t2

let domain t =
  let names =
    Name.Set.fold (fun name bindable_names ->
        Bindable_name.Set.add (Name name) bindable_names)
      (Name.Map.keys (snd t.current_level).just_after_level.names_to_types)
      Bindable_name.Set.empty
  in
  Name_occurrences.create_from_set_in_terms names

let find_exn t name : Flambda_types.t * binding_type =
  (* CR mshinwell: Maybe this should cause a fatal error and we shouldn't
     rely on catching the exception *)
  let ty =
    Name.Map.find name (snd t.current_level).just_after_level.names_to_types
  in
  let binding_type : binding_type =
    if Name.Set.mem name
         (snd t.current_level).just_after_level.were_existentials
    then
      Was_existential
    else
      Normal
  in
  ty, binding_type

type still_unresolved =
  | Resolved
  | Still_unresolved

let resolve_aliases_on_ty0 (type a) t ?bound_name ~force_to_kind
      ~print_ty (ty : a Flambda_types.ty)
      : (a Flambda_types.ty) * (Simple.t option) * Name_or_export_id.Set.t
          * still_unresolved =
  let rec resolve_aliases names_seen ~canonical_simple
        (ty : a Flambda_types.ty) =
    let resolve (name : Name_or_export_id.t)
          : _ * _ * _ * still_unresolved =
      if Name_or_export_id.Set.mem name names_seen then begin
        Misc.fatal_errorf "Loop on %a whilst resolving aliases"
          Name_or_export_id.print name
      end;
      let continue_resolving t ~canonical_simple =
        let names_seen = Name_or_export_id.Set.add name names_seen in
        let ty = force_to_kind t in
        resolve_aliases names_seen ~canonical_simple ty
      in
      match name with
      | Name name ->
        begin match find_exn t name with
        | exception Not_found ->
          Misc.fatal_errorf "Unbound name %a whilst resolving aliases \
              for type:@ %a@ in environment:@ %a"
            Name.print name
            print_ty ty
            print t
        | ty, _ ->
          continue_resolving ty ~canonical_simple:(Some (Simple.name name))
        end
      | Export_id export_id ->
        match t.resolver export_id with
        | Some ty -> continue_resolving ty ~canonical_simple
        | None -> ty, None, Name_or_export_id.Set.empty, Still_unresolved
    in
    match ty with
    | No_alias _ -> ty, canonical_simple, names_seen, Resolved
    | Type export_id -> resolve (Name_or_export_id.Export_id export_id)
    | Equals (Name name) -> resolve (Name_or_export_id.Name name)
    | Equals ((Const _ | Discriminant _) as simple) ->
      ty, Some simple, names_seen, Resolved
  in
  let seen =
    match bound_name with
    | None -> Name_or_export_id.Set.empty
    | Some bound_name -> Name_or_export_id.Set.singleton (Name bound_name)
  in
  let canonical_simple =
    match bound_name with
    | None -> None
    | Some bound_name -> Some (Simple.name bound_name)
  in
  resolve_aliases seen ~canonical_simple ty

let resolve_aliases_on_ty t ?bound_name ~force_to_kind ~print_ty ty =
  let ty, canonical_name, names_seen, _still_unresolved =
    resolve_aliases_on_ty0 t ?bound_name ~force_to_kind ~print_ty ty
  in
  ty, canonical_name, names_seen

let resolve_aliases_and_squash_unresolved_names_on_ty' env ?bound_name
      ~print_ty ~force_to_kind ty
      : _ Flambda_types.unknown_or_join * (Simple.t option) =
  let ty, canonical_name, _names_seen, _still_unresolved =
    try resolve_aliases_on_ty0 env ?bound_name ~force_to_kind ~print_ty ty
    with Misc.Fatal_error -> begin
      Format.eprintf "\n%sContext is: \
          resolve_aliases_and_squash_unresolved_names_on_ty':%s\
          @ %a@ Environment:@ %a\n"
        (Misc_color.bold_red ())
        (Misc_color.reset ())
        print_ty ty
        print env;
      raise Misc.Fatal_error
    end
  in
  match ty with
  | No_alias uoj -> uoj, canonical_name
  | Type _ | Equals _ -> Unknown, canonical_name

let resolve_aliases ?bound_name t (ty : Flambda_types.t)
      : Flambda_types.t * (Simple.t option) =
  match ty with
  | Value ty_value ->
    let force_to_kind = Flambda_type0_core.force_to_kind_value in
    let ty_value, canonical_name, _names_seen =
      resolve_aliases_on_ty t ?bound_name ~force_to_kind
        ~print_ty:Type_printers.print_ty_value
        ty_value
    in
    Value ty_value, canonical_name
  | Naked_number (ty_naked_number, kind) ->
    let force_to_kind =
      Flambda_type0_core.force_to_kind_naked_number kind
    in
    let ty_naked_number, canonical_name, _names_seen =
      resolve_aliases_on_ty t ?bound_name ~force_to_kind
        ~print_ty:Type_printers.print_ty_naked_number
        ty_naked_number
    in
    Naked_number (ty_naked_number, kind), canonical_name
  | Fabricated ty_fabricated ->
    let force_to_kind = Flambda_type0_core.force_to_kind_fabricated in
    let ty_fabricated, canonical_name, _names_seen =
      resolve_aliases_on_ty t ?bound_name ~force_to_kind
        ~print_ty:Type_printers.print_ty_fabricated
        ty_fabricated
    in
    Fabricated ty_fabricated, canonical_name

(*
let fold_all t ~init ~f =
  Scope_level.Map.fold (fun level by_sublevel acc ->
      Scope_level.Sublevel.Map.fold (fun sublevel (name, entry) acc ->
        let scope_level = Scope_level.With_sublevel.create level sublevel in
        let binding_type : binding_type =
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
          | Definition kind ->
            Some ((Definition kind) : typing_env_entry0)
          | Equation ty ->
            Some ((Equation ty) : typing_env_entry0)
        in
        match entry0 with
        | None -> acc
        | Some entry0 ->
          let scope_level =
            Scope_level.With_sublevel.create level sublevel
          in
          let binding_type : binding_type =
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
    ignore (fold_all t ~init:Bindable_name.Set.empty
      ~f:(fun names_seen (name : Name.t)
              (_binding_type : binding_type)
              _scope_level entry ->
        let free_names =
          match entry with
          | Definition _kind -> Bindable_name.Set.empty
          | Equation ty ->
            Name_occurrences.everything (Type_free_names.free_names ty)
          | CSE prim ->
            Name_occurrences.everything
              (Flambda_primitive.With_fixed_value.free_names prim)
        in
        if not (Bindable_name.Set.subset free_names names_seen) then begin
          Misc.fatal_errorf "Typing environment is not closed \
              (%a free):@ %a"
            Bindable_name.Set.print
            (Bindable_name.Set.diff free_names names_seen)
            print t
        end;
        match entry with
        | Definition _ -> Bindable_name.Set.add (Name name) names_seen
        | Equation _ | CSE _ -> names_seen) : Bindable_name.Set.t);
    (* Checking that alias resolution works also ensures there are no
       cycles via aliases. *)
    Name.Map.iter (fun bound_name (_level, entry) ->
        match (entry : typing_env_entry0) with
        | Definition _kind -> ()
        | Equation ty ->
          ignore (Sys.opaque_identity (resolve_aliases ~bound_name t ty)))
      t.names_to_levels;
  end
*)

let invariant _t = ()

let mem t name =
  Name.Map.mem name (snd t.current_level).just_after_level.names_to_types

(*
let scope_level_exn t name =
  match Name.Map.find name t.names_to_levels with
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
*)

let find_opt t name =
  match find_exn t name with
  | exception Not_found -> None
  | ty, binding_type -> Some (ty, binding_type)

let find_cse (t : t) prim =
  match Flambda_primitive.With_fixed_value.create prim with
  | None -> None
  | Some prim ->
    match
      Flambda_primitive.With_fixed_value.Map.find
        prim (snd t.current_level).just_after_level.cse
    with
    | exception Not_found -> None
    | name -> Some name

let was_existential_exn t name =
  let _ty, binding_type = find_exn t name in
  match binding_type with
  | Normal -> false
  | Was_existential -> true

(*
let _min_level_for_new_binding t =
  let all_levels = Scope_level.Map.keys t.levels_to_entries in
  match Scope_level.Set.max_elt_opt all_levels with
  | None -> Scope_level.initial
  | Some level -> level
*)

type sense =
  | New_equation_must_be_more_precise
  | Existing_equation_must_be_more_precise

let _print_sense ppf (sense : sense) =
  match sense with
  | New_equation_must_be_more_precise ->
    Format.fprintf ppf "New_equation_must_be_more_precise"
  | Existing_equation_must_be_more_precise ->
    Format.fprintf ppf "Existing_equation_must_be_more_precise"

let invariant_for_new_equation t name ty =
  let free_names = Type_free_names.free_names ty in
  if Name_occurrences.mem free_names (Name name) then begin
    Misc.fatal_errorf "Cannot add equation@ %a = %a@ as it would produce \
        a circular dependency"
      Name.print name
      Type_printers.print ty
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
      print_typing_env_entry entry
      Scope_level.print level
      Scope_level.print min_level
      print_typing_environment t
  end;
*)
  match find_opt t name with
  | None ->
    Misc.fatal_errorf "Cannot add equation@ %a = %a@ for name undefined in \
        environment:@ %a"
      Name.print name
      Type_printers.print ty
      print t
  | Some _ -> ()

(* XXX Invariant check: [current_level] should be the maximum key in
   [levels] *)

(*
let invariant_for_new_equation t name (ty : Flambda_types.t) ~sense =
  let existing_ty, _binding_type = find_exn t name in
  let meet_ty, _env_extension =
    let meet_env =
      Meet_env.create t
        ~perm_left:(Name_permutation.create ())
        ~perm_right:(Name_permutation.create ())
    in
    Both_meet_and_join.meet meet_env existing_ty ty
  in
  (* XXX This should be done by [equal] *)
  (* XXX And we need to think about this (likewise similar code in
     [add_env_extension'].  If correctness is hinging on
     aliases being preserved, could we cause unsoundness by failing to add
     an alias due to not being able to prove that the corresponding type is
     more precise? *)
  let meet_ty, _ = Typing_env.resolve_aliases t meet_ty in
  let existing_ty, _ = Typing_env.resolve_aliases t existing_ty in
  let ty, _ = Typing_env.resolve_aliases t ty in
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
      Type_printers.print ty
      as_or_more_precise
      strictly_more_precise
      Type_printers.print meet_ty
      Type_printers.print existing_ty
      print_sense sense
      print t
  end
(* XXX Not sure about this part
  Typing_env_extension.iter env_extension
    ~f:(fun name _binding_type level (entry : typing_env_entry0) ->
      let ty =
        match entry with
        | Definition ty | Equation ty -> ty
      in
      let level = Scope_level.With_sublevel.level level in
      invariant_for_any_new_binding t name level (Equation ty);
      invariant_for_new_equation t name ty
        ~sense:Existing_equation_must_be_more_precise)
*)

let invariant_for_new_binding t name level
      (entry : typing_env_entry) =
  invariant_for_any_new_binding t name level entry;
  match entry with
  | Definition _ | CSE _ -> ()
  | Equation ty ->
    invariant_for_new_equation t name ty
      ~sense:New_equation_must_be_more_precise
*)
let _ = ignore New_equation_must_be_more_precise
let _ = ignore Existing_equation_must_be_more_precise

let aliases_of_simple (t : t) (simple : Simple.t) =
  match
    Simple.Map.find simple (snd t.current_level).just_after_level.aliases
  with
  | exception Not_found ->
    begin match simple with
    | Const _ | Discriminant _ -> Name.Set.empty
    | Name name -> Name.Set.singleton name
    end
  | aliases ->
    begin match simple with
    | Const _ | Discriminant _ -> aliases
    | Name name -> Name.Set.add name aliases
    end

let add_definition t name kind =
  if mem t name then begin
    Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
      Name.print name
      print t
  end;
  let level =
    Typing_env_level.add_definition (snd t.current_level).level name kind
  in
  let names_to_types =
    Name.Map.add name (Flambda_type0_core.unknown kind)
      (snd t.current_level).just_after_level.names_to_types
  in
  let just_after_level =
    { (snd t.current_level).just_after_level with
      names_to_types;
    }
  in
  let current_level =
    { level;
      just_after_level;
    }
  in
  let t =
    { t with
      current_level = (fst t.current_level, current_level);
    }
  in
  invariant t;
  t

let add_equation t name ty =
  if not (mem t name) then begin
    Misc.fatal_errorf "Cannot add equation on unbound name %a in \
        environment:@ %a"
      Name.print name
      print t
  end;
  invariant_for_new_equation t name ty;
  let alias = Flambda_type0_core.get_alias ty in
  let equation_with_reverse_alias_already_present =
    match alias with
    | None | Some (Const _ | Discriminant _) -> false
    | Some (Name alias) ->
      Name.Set.mem alias (aliases_of_simple t (Simple.name name))
  in
  if equation_with_reverse_alias_already_present then
    t
  else
    let level =
      Typing_env_level.add_or_replace_equation (snd t.current_level).level
        name ty
    in
    let names_to_types =
      Name.Map.add name ty
        (snd t.current_level).just_after_level.names_to_types
    in
    let just_after_level =
      { (snd t.current_level).just_after_level with
        names_to_types;
      }
    in
    let current_level =
      { level;
        just_after_level;
      }
    in
    let t =
      { t with
        current_level = (fst t.current_level, current_level);
      }
    in
    invariant t;
    t

let add_cse t simple prim =
  match
    Flambda_primitive.With_fixed_value.Map.find prim
      (snd t.current_level).just_after_level.cse
  with
  | exception Not_found ->
    let level =
      Typing_env_level.add_cse (snd t.current_level).level simple prim
    in
    let cse =
      Flambda_primitive.With_fixed_value.Map.add prim simple
        (snd t.current_level).just_after_level.cse
    in
    let just_after_level =
      { (snd t.current_level).just_after_level with
        cse;
      }
    in
    let current_level =
      { level;
        just_after_level;
      }
    in
    let t =
      { t with
        current_level = (fst t.current_level, current_level);
      }
    in
    invariant t;
    t
  | _ ->
   (* Keep the existing (furthest-out) binding. *)
   t

let rec add_opened_env_extension t level : t =
  let t_before_equations =
    Name.Map.fold (fun name kind t ->
        add_definition t name kind)
      (Typing_env_level.defined_names level)
      t
  in
  let t =
    Name.Map.fold (fun name ty t ->
        match find_opt t name with
        | None -> add_equation t name ty
        | Some (existing_ty, _binding_type) ->
          let meet_ty, meet_env_extension =
            let meet_env =
              Meet_env.create t
                ~perm_left:(Name_permutation.create ())
                ~perm_right:(Name_permutation.create ())
            in
            Both_meet_and_join.meet meet_env ty existing_ty
          in
          let t = add_env_extension t meet_env_extension in
          let as_or_strictly_less_precise =
            Type_equality.equal ~bound_name:(Some name)
              t_before_equations t_before_equations
              meet_ty existing_ty
          in
          let strictly_more_precise = not as_or_strictly_less_precise in
Format.eprintf "AOMOEE: name %a,@ meet_ty %a,@ existing_ty %a,@ SMP %b\n%!"
Name.print name
Type_printers.print meet_ty
Type_printers.print existing_ty
strictly_more_precise;
          if strictly_more_precise then add_equation t name meet_ty
          else t)
      (Typing_env_level.equations level)
      t_before_equations
  in
  Flambda_primitive.With_fixed_value.Map.fold (fun prim simple t ->
      add_cse t simple prim)
    (Typing_env_level.cse level)
    t

and add_env_extension t env_extension : t =
  Typing_env_extension.pattern_match env_extension
    ~f:(fun level -> add_opened_env_extension t level)

let current_level t = fst (t.current_level)
let current_level_data t = snd (t.current_level)

let cut0 t ~existential_if_defined_at_or_later_than:min_level =
  if Scope_level.(>) min_level (current_level t) then
    Typing_env_level.empty ()
  else
    let all_levels =
      Scope_level.Map.add (current_level t) (current_level_data t)
        t.prev_levels
    in
    let strictly_less, at_min_level, strictly_greater =
      Scope_level.Map.split min_level all_levels
    in
    let at_or_after_cut =
      match at_min_level with
      | None -> strictly_greater
      | Some typing_env_level ->
        Scope_level.Map.add min_level typing_env_level strictly_greater
    in
    let t =
      if Scope_level.Map.is_empty strictly_less then
        create ~resolver:t.resolver
      else
        let current_level, current_level_data =
          Scope_level.Map.max_binding strictly_less
        in
        let prev_levels =
          Scope_level.Map.remove current_level strictly_less
        in
        { resolver = t.resolver;
          prev_levels;
          current_level = (current_level, current_level_data);
        }
    in
    invariant t;
    let meet_env =
      Meet_env.create t
        ~perm_left:(Name_permutation.create ())
        ~perm_right:(Name_permutation.create ())
    in
    Scope_level.Map.fold (fun _level one_level result ->
        Typing_env_level.meet meet_env one_level.level result)
      at_or_after_cut
      (Typing_env_level.empty ())

let cut t ~existential_if_defined_at_or_later_than =
  let level = cut0 t ~existential_if_defined_at_or_later_than in
  Typing_env_extension.create level
