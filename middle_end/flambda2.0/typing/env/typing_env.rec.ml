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

module Cached = struct
  type t = {
    names_to_types : Flambda_types.t Name.Map.t;
    aliases : Aliases.t;
  }

  let print_with_cache ~cache ppf { names_to_types; aliases; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(names_to_types@ %a)@]@ \
        @[<hov 1>(aliases@ %a)@]\
        )@]"
      (Name.Map.print (Type_printers.print_with_cache ~cache)) names_to_types
      Aliases.print aliases

  let invariant t =
    let canonical_names = Aliases.canonical_names t.aliases in
    Name.Set.iter (fun name ->
        match Name.Map.find name t.names_to_types with
        | exception Not_found ->
          Misc.fatal_errorf "Canonical name %a not in [names_to_types]"
            Name.print name
        | ty ->
          match Flambda_type0_core.get_alias ty with
          | None -> ()
          | Some alias_of ->
            Misc.fatal_errorf "Canonical name %a has an alias type: =%a"
              Name.print name
              Name.print alias_of)
      canonical_names

  let empty =
    { names_to_types = Name.Map.empty;
      aliases = Simple.Map.empty;
    }

  let create ~names_to_types aliases =
    { names_to_types;
      aliases;
    }

  let defined_names t = Name.Map.keys t.names_to_types
end

module One_level = struct
  type t = {
    scope : Scope.t;
    level : Typing_env_level.t;
    just_after_level : Cached.t;
  }

  let print_with_cache ~cache:_ ppf
        { scope = _; level; just_after_level = _; } =
    Typing_env_level.print ppf level

  let create scope level ~just_after_level =
    { scope;
      level;
      just_after_level;
    }

  let scope t = t.scope
  let level t = t.level
  let just_after_level t = t.just_after_level

  let empty_one_level =
    { level = Typing_env_level.empty;
      just_after_level = Cached.empty;
    }

  let is_empty t = Typing_env_level.is_empty t.level

  let next_scope t = Scope.next t.scope

  let defined_names t = Cached.defined_names t.just_after_level
end

type t = {
  resolver : (Export_id.t -> Flambda_types.t option);
  prev_levels : One_level.t Scope.Map.t;
  current_level : One_level.t;
}

let print_with_cache ~cache ppf
      ({ resolver = _; prev_levels; current_level; } as t) =
  if is_empty t then
    Format.pp_print_string ppf "Empty"
  else
    Printing_cache.with_cache cache ppf "env" t (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(\
            @[<hov 1>(prev_levels@ %a)@]@ \
            @[<hov 1>(current_level@ %a)@]\
            )@]"
        (Scope.Map.print (One_level.print_with_cache ~cache)) prev_levels
        (One_level.print_with_cache ~cache) current_level)

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant ?force t =
  if !Clflags.flambda_invariants || Option.is_some (force : unit option)
  then begin
    let no_empty_prev_levels =
      Scope.Map.for_all (fun level -> not (One_level.is_empty level))
        t.prev_levels
    in
    if not no_empty_prev_levels then begin
      Misc.fatal_errorf "Typing environment contains [prev_levels] that are \
          empty:@ %a"
        print t
    end;
    let current_scope = One_level.scope t.current_level in
    let max_prev_scope =
      Scope.Map.fold (fun one_level _ max_prev_scope ->
          Scope_level.max (One_level.scope one_level) max_prev_scope)
        t.prev_levels
        Scope_level.initial
    in
    if Scope_level.(<=) current_scope max_prev_scope then begin
      Misc.fatal_errorf "Typing environment contains a [current_level] with a \
          scope that is not strictly greater than all scopes in \
          [prev_levels]:@ %a"
        print t
    end
  end

let invariant_should_fail t =
  invariant ~force:() t;
  Misc.fatal_errorf "[invariant] should have failed:@ %a" print t

let is_empty t =
  One_level.is_empty t.current_level
    && Scope.Map.is_empty t.prev_levels

let create ~resolver =
  { resolver;
    prev_levels = Scope.Map.empty;
    current_level = One_level.empty;
  }

let create_using_resolver_from t = create ~resolver:t.resolver

let resolver t = t.resolver

let current_scope t = One_level.scope t.current_level

let names_to_types t =
  Cached.names_to_types (One_level.just_after_level t.current_level)

let aliases t =
  Cached.aliases (One_level.just_after_level t.current_level)

let increment_scope_level_to t scope =
  let current_scope = current_scope t in
  if Scope.(<=) scope current_scope then begin
    Misc.fatal_errorf "New level %a must exceed %a:@ %a"
      Scope.print level
      Scope.print current_scope
      print t
  end;
  let prev_levels =
    Scope.Map.add current_scope t.current_level t.prev_levels
  in
  let current_level =
    One_level.create scope Typing_env_level.empty
      ~just_after_level:(One_level.just_after_level t.current_level)
  in
  { t with
    prev_levels;
    current_level;
  }

let increment_scope_level t =
  increment_scope_level_to t (One_level.next_scope t.current_level)

let fast_equal t1 t2 =
  t1 == t2

let domain t =
  Name_occurrences.create_from_set_in_terms
    (Cached.domain (One_level.just_after_level t.current_level))

let find t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment:@ %a"
      print t
  | ty -> ty

let find_opt t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found -> None
  | ty -> Some ty

let mem t name =
  Name.Map.mem name (names_to_types t)

let with_current_level t ~current_level =
  let t = { t with current_level; } in
  invariant t;
  t

let add_definition t name kind =
  if mem t name then begin
    Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
      Name.print name
      print t
  end;
  let level =
    Typing_env_level.add_definition (One_level.level t.current_level) name kind
  in
  let just_after_level =
    let names_to_types =
      Name.Map.add name (Flambda_type0_core.unknown kind) (names_to_types t)
    in
    Cached.with_names_to_types (One_level.just_after_level t.current_level)
      ~names_to_types
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level t ~current_level

(* CR mshinwell: This should check that precision is not decreasing. *)
let invariant_for_new_equation _t _name _ty = ()

let add_equation t name ty =
  if not (mem t name) then begin
    Misc.fatal_errorf "Cannot add equation on unbound name %a in \
        environment:@ %a"
      Name.print name
      print t
  end;
  invariant_for_new_equation t name ty;
  let level =
    Typing_env_level.add_or_replace_equation
      (One_level.level t.current_level) name ty
  in
  let just_after_level =
    let aliases =
      let aliases = aliases t in
      match Flambda_type0_core.get_alias ty with
      | None -> aliases
      | Some alias_of -> Aliases.add aliases name ~alias_of
    in
    let names_to_types = Name.Map.add name ty (names_to_types t) in
    Cached.create ~names_to_types aliases
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level t ~current_level

let rec add_env_extension starting_t level : t =
  if not (Name.Map.is_empty (Typing_env_level.defined_names level)) then begin
    (* The full type system will remove this restriction. *)
    Misc.fatal_errorf "Typing environment extensions cannot define names:@ %a"
      Typing_env_level.print level
  end;
  Name.Map.fold (fun name ty t ->
      match find_opt t name with
      | None -> add_equation t name ty
      | Some existing_ty ->
        let meet_ty, meet_env_extension =
          let meet_env = Meet_env.create t in
          Api_meet_and_join.meet meet_env ty existing_ty
        in
        let t = add_env_extension t meet_env_extension in
        let as_or_strictly_less_precise =
          Type_equality.equal ~bound_name:(Some name)
            starting_t starting_t
            meet_ty existing_ty
        in
        let strictly_more_precise = not as_or_strictly_less_precise in
        if strictly_more_precise then add_equation t name meet_ty
        else t)
    (Typing_env_level.equations level)
    starting_t

let cut t ~unknown_if_defined_at_or_later_than:min_level =
  if Scope.(>) min_level (current_level t) then
    Typing_env_level.empty
  else
    let all_levels =
      Scope.Map.add (current_level t) (current_level_data t)
        t.prev_levels
    in
    let strictly_less, at_min_level, strictly_greater =
      Scope.Map.split min_level all_levels
    in
    let at_or_after_cut =
      match at_min_level with
      | None -> strictly_greater
      | Some typing_env_level ->
        Scope.Map.add min_level typing_env_level strictly_greater
    in
    let t =
      if Scope.Map.is_empty strictly_less then
        create ~resolver:t.resolver
      else
        let current_level, current_level_data =
          Scope.Map.max_binding strictly_less
        in
        let prev_levels =
          Scope.Map.remove current_level strictly_less
        in
        { resolver = t.resolver;
          prev_levels;
          current_level = (current_level, current_level_data);
        }
    in
    invariant t;
    let meet_env = Meet_env.create t in
    let vars_in_scope_at_cut =
      match Scope.Map.max_binding_opt strictly_less with
      | None -> Name.Set.empty
      | Some level -> Name.set_to_var_set (One_level.defined_names level)
    in
    Scope.Map.fold (fun _scope level result ->
        let level =
          (* Since environment extensions are not allowed to define names at
             the moment, any [Equals] aliases to names not in scope at the cut
             point have to be squashed to "Unknown". *)
          Typing_env_level.erase_aliases level ~allowed:vars_in_scope_at_cut
        in
        Typing_env_level.meet meet_env level result)
      at_or_after_cut
      Typing_env_level.empty

let resolve_any_toplevel_alias_on_ty0 (type a) t
      ~force_to_kind ~print_ty (ty : a Flambda_types.ty)
      : (a Flambda_types.unknown_or_join) * (Simple.t option) =
  let force_to_unknown_or_join typ =
    match force_to_kind typ with
    | No_alias unknown_or_join -> unknown_or_join
    | Type _ | Alias _ ->
      Misc.fatal_errorf "Expected [No_alias]:@ %a" Type_printers.print typ
  in
  match ty with
  | No_alias unknown_or_join -> unknown_or_join, None
  | Type _export_id -> Misc.fatal_error ".cmx loading not yet implemented"
  | Equals ((Const const) as simple) ->
    let const_type = Flambda_type0_core.type_for_const const in
    let ty = force_to_unknown_or_join const_type in
    ty, Some simple
  | Equals ((Discriminant discriminant) as simple) ->
    let ty =
      force_to_unknown_or_join
        (Flambda_type0_core.this_discriminant discriminant)
    in
    ty, Some simple
  | Equals (Name name) ->
    let name = Aliases.get_canonical_name (aliases t) name in
    let ty = force_to_kind (find t name) in
    match ty with
    | No_alias unknown_or_join -> unknown_or_join, Some (Simple.name name)
    | Type _export_id -> Misc.fatal_error ".cmx loading not yet implemented"
    | Equals _ -> invariant_should_fail t

let resolve_any_toplevel_alias_on_ty (type a) t ~force_to_kind ~print_ty
      (ty : a Flambda_types.ty)
      : (a Flambda_types.ty) * (Simple.t option) =
  match ty with
  | No_alias _ -> ty, None
  | Type _export_id -> Misc.fatal_error ".cmx loading not yet implemented"
  | Equals ((Const _ | Discriminant _) as simple) -> ty, Some simple
  | Equals (Name name) ->
    let name = Aliases.get_canonical_name (aliases t) name in
    let ty = force_to_kind (find t name) in
    match ty with
    | No_alias _ -> ty, Some (Simple.name name)
    | Type _export_id -> Misc.fatal_error ".cmx loading not yet implemented"
    | Equals _ -> invariant_should_fail t

let resolve_any_toplevel_alias t (ty : Flambda_types.t)
      : Flambda_types.t * (Simple.t option) =
  match ty with
  | Value ty_value ->
    let ty_value, canonical_simple =
      resolve_aliases_on_ty t env
        ~force_to_kind:Flambda_type0_core.force_to_kind_value
        ~print_ty:Type_printers.print_ty_value
        ty_value
    in
    Value ty_value, canonical_simple
  | Naked_number (ty_naked_number, kind) ->
    let ty_naked_number, canonical_simple =
      resolve_aliases_on_ty t env
        ~force_to_kind:(Flambda_type0_core.force_to_kind_naked_number kind)
        ~print_ty:Type_printers.print_ty_naked_number
        ty_naked_number
    in
    Naked_number (ty_naked_number, kind), canonical_simple
  | Fabricated ty_fabricated ->
    let ty_fabricated, canonical_simple =
      resolve_aliases_on_ty t env
        ~force_to_kind:Flambda_type0_core.force_to_kind_fabricated
        ~print_ty:Type_printers.print_ty_fabricated
        ty_fabricated
    in
    Fabricated ty_fabricated, canonical_simple
