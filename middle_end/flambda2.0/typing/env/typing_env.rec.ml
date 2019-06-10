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

module Binding_time : sig
  type t

(*
  val print : Format.formatter -> t -> unit
*)

  val symbols : t
  val earliest_var : t
  val succ : t -> t

  val strictly_earlier : t -> than:t -> bool
  val equal : t -> t -> bool
end = struct
  include Numbers.Int

  let strictly_earlier t ~than =
    t < than

  let symbols = 0
  let earliest_var = 1

  let succ t =
    if t < earliest_var then
      Misc.fatal_error "Cannot increment binding time for symbols"
    else
      t + 1
end

(* CR mshinwell: Add signatures to these submodules. *)
module Cached = struct
  type t = {
    names_to_types : (Flambda_types.t * Binding_time.t) Name.Map.t;
    aliases : Aliases.t;
  }

(*
  let _print_with_cache ~cache ppf { names_to_types; aliases; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(names_to_types@ %a)@]@ \
        @[<hov 1>(aliases@ %a)@]\
        )@]"
      (Name.Map.print (Type_printers.print_with_cache ~cache)) names_to_types
      Aliases.print aliases

  let _invariant t =
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
              Simple.print alias_of)
      canonical_names
*)

  let empty =
    { names_to_types = Name.Map.empty;
      aliases = Aliases.empty;
    }

  let create ~names_to_types aliases =
    { names_to_types;
      aliases;
    }

  let names_to_types t = t.names_to_types
  let aliases t = t.aliases

  (* CR mshinwell: Add type lookup function *)

  let binding_time t name =
    match Name.Map.find name t.names_to_types with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound name %a" Name.print name
    | (_ty, time) -> time

  let domain t = Name.Map.keys t.names_to_types
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

  let create_empty scope =
    { scope;
      level = Typing_env_level.empty;
      just_after_level = Cached.empty;
    }

  let scope t = t.scope
  let level t = t.level
  let just_after_level t = t.just_after_level

  let is_empty t = Typing_env_level.is_empty t.level

  let next_scope t = Scope.next t.scope
end

type t = {
  resolver : (Export_id.t -> Flambda_types.t option);
  defined_symbols : Symbol.Set.t;
  prev_levels : One_level.t Scope.Map.t;
  (* CR mshinwell: hold list of symbol definitions, then change defined_names
     to variables, then remove artificial symbol precedence *)
  current_level : One_level.t;
  next_binding_time : Binding_time.t;
}

let is_empty t =
  One_level.is_empty t.current_level
    && Scope.Map.is_empty t.prev_levels
    && Symbol.Set.is_empty t.defined_symbols

let print_with_cache ~cache ppf
      ({ resolver = _; prev_levels; current_level; next_binding_time = _;
         defined_symbols;
       } as t) =
  if is_empty t then
    Format.pp_print_string ppf "Empty"
  else
    Printing_cache.with_cache cache ppf "env" t (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(\
            @[<hov 1>(prev_levels@ %a)@]@ \
            @[<hov 1>(current_level@ %a)@]@ \
            @[<hov 1>(defined_symbols@ %a)@]\
            )@]"
        (Scope.Map.print (One_level.print_with_cache ~cache)) prev_levels
        (One_level.print_with_cache ~cache) current_level
        Symbol.Set.print defined_symbols)

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant0 ?force t =
  if !Clflags.flambda_invariant_checks || Option.is_some (force : unit option)
  then begin
(* CR mshinwell: Fix things so this check passes, or delete it.
    let no_empty_prev_levels =
      Scope.Map.for_all (fun _scope level -> not (One_level.is_empty level))
        t.prev_levels
    in
    if not no_empty_prev_levels then begin
      Misc.fatal_errorf "Typing environment contains [prev_levels] that are \
          empty:@ %a"
        print t
    end;
*)
    let current_scope = One_level.scope t.current_level in
    let max_prev_scope =
      Scope.Map.fold (fun scope _level max_prev_scope ->
          Scope.max scope max_prev_scope)
        t.prev_levels
        Scope.initial
    in
    if Scope.(<=) current_scope max_prev_scope then begin
      Misc.fatal_errorf "Typing environment contains a [current_level] with a \
          scope that is not strictly greater than all scopes in \
          [prev_levels]:@ %a"
        print t
    end
  end

let invariant_should_fail t =
  invariant0 ~force:() t;
  Misc.fatal_errorf "[invariant] should have failed:@ %a" print t

let invariant t : unit = invariant0 t

let resolver t = t.resolver

let current_scope t = One_level.scope t.current_level

let names_to_types t =
  Cached.names_to_types (One_level.just_after_level t.current_level)

let aliases t =
  Cached.aliases (One_level.just_after_level t.current_level)

let create ~resolver =
  { resolver;
    prev_levels = Scope.Map.empty;
    current_level = One_level.create_empty Scope.initial;
    next_binding_time = Binding_time.earliest_var;
    defined_symbols = Symbol.Set.empty;
  }

let create_using_resolver_from t = create ~resolver:t.resolver

let increment_scope_to t scope =
  let current_scope = current_scope t in
  if Scope.(<=) scope current_scope then begin
    Misc.fatal_errorf "New level %a must exceed %a:@ %a"
      Scope.print scope
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

let increment_scope t =
  increment_scope_to t (One_level.next_scope t.current_level)

let domain0 t =
  Cached.domain (One_level.just_after_level t.current_level)

let domain t =
  Name_occurrences.create_names_in_types (domain0 t)

let var_domain t = Name.set_to_var_set (domain0 t)

let find t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment:@ %a"
      Name.print name
      print t
  (* CR mshinwell: Should this resolve aliases? *)
  | ty, _binding_time -> ty

let find_opt t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found -> None
  | ty, _binding_time -> Some ty

let mem t name =
  Name.Map.mem name (names_to_types t)

let with_current_level t ~current_level =
  let t = { t with current_level; } in
  invariant t;
  t

let with_current_level_and_next_binding_time t ~current_level
      next_binding_time =
  let t = { t with current_level; next_binding_time; } in
  invariant t;
  t

let cached t = One_level.just_after_level t.current_level

let defined_earlier t (simple : Simple.t) ~(than : Simple.t) =
  match simple, than with
  | (Const _ | Discriminant _), (Const _ | Discriminant _) ->
    Simple.compare simple than <= 0
  | (Const _ | Discriminant _), Name _ -> true
  | Name _, (Const _ | Discriminant _) -> false
  | Name name1, Name name2 ->
    if Name.equal name1 name2 then
      false
    else
      let time1 = Cached.binding_time (cached t) name1 in
      let time2 = Cached.binding_time (cached t) name2 in
      if Binding_time.equal time1 time2 then begin
          Misc.fatal_errorf "Unequal names with same binding time: \
              %a and %a:@ %a"
            Name.print name1
            Name.print name2
            print t
        end;
      Binding_time.strictly_earlier time1 ~than:time2

let add_variable_definition t var kind =
  let name = Name.var var in
  if mem t name then begin
    Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
      Name.print name
      print t
  end;
  let level =
    Typing_env_level.add_definition (One_level.level t.current_level) var kind
  in
  let just_after_level =
    let aliases =
      Aliases.add_canonical_name (aliases t) name
    in
(*
Format.eprintf "Aliases after defining %a:@ %a\n%!" Name.print name Aliases.print aliases;
*)
    let names_to_types =
      Name.Map.add name (Flambda_type0_core.unknown kind, t.next_binding_time)
        (names_to_types t)
    in
    Cached.create ~names_to_types aliases
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level_and_next_binding_time t ~current_level
    (Binding_time.succ t.next_binding_time)

let add_symbol_definition t sym kind =
  let name = Name.symbol sym in
  let just_after_level =
    let aliases =
      Aliases.add_canonical_name (aliases t) name
    in
    let names_to_types =
      Name.Map.add name (Flambda_type0_core.unknown kind, Binding_time.symbols)
        (names_to_types t)
    in
    Cached.create ~names_to_types aliases
  in
  let current_level =
    One_level.create (current_scope t) (One_level.level t.current_level)
      ~just_after_level
  in
  with_current_level t ~current_level

let add_definition t (name : Name.t) kind =
  match name with
  | Var var -> add_variable_definition t var kind
  | Symbol sym -> add_symbol_definition t sym kind

(* CR mshinwell: This should check that precision is not decreasing. *)
let invariant_for_new_equation t name ty =
  let defined_names = domain t in
  let free_names = Type_free_names.free_names ty in
  if not (Name_occurrences.subset free_names defined_names) then begin
    Misc.fatal_errorf "New equation@ %a@ =@ %a@ has unbound names:@ %a"
      Name.print name
      Type_printers.print ty
      print t
  end

let add_equation t name ty =
  if not (mem t name) then begin
    Misc.fatal_errorf "Cannot add equation on unbound name %a in \
        environment:@ %a"
      Name.print name
      print t
  end;
(*
Format.eprintf "Trying to add equation %a = %a\n%!"
  Name.print name
  Type_printers.print ty;
*)
  let aliases, simple, ty =
    let aliases = aliases t in
    match Flambda_type0_core.get_alias ty with
    | None -> aliases, Simple.name name, ty
    | Some alias_of ->
      match
        Aliases.add aliases (Simple.name name) alias_of
          ~defined_earlier:(fun simple ~than -> defined_earlier t simple ~than)
      with
      | None, aliases -> aliases, alias_of, ty
      | (Some { canonical_name; alias_of; }), aliases ->
(*
Format.eprintf "For name %a, Aliases returned CN=%a, alias_of=%a\n%!"
  Name.print name
  Name.print canonical_name
  Name.print alias_of;
*)
        let kind = Flambda_type0_core.kind ty in
        let ty =
          Flambda_type0_core.alias_type_of kind (Simple.name canonical_name)
        in
        aliases, Simple.name alias_of, ty
  in
(*
Format.eprintf "Aliases after adding equation %a = %a:@ %a\n%!"
  Simple.print simple
  Type_printers.print ty
  Aliases.print aliases;
*)
  match simple with
  | Const _ | Discriminant _ -> t
  | Name name ->
    invariant_for_new_equation t name ty;
    let level =
      Typing_env_level.add_or_replace_equation
        (One_level.level t.current_level) name ty
    in
    (* CR mshinwell: remove second lookup *)
    let binding_time = Cached.binding_time (cached t) name in
    let just_after_level =
      let names_to_types =
        Name.Map.add name (ty, binding_time) (names_to_types t)
      in
      Cached.create ~names_to_types aliases
    in
    let current_level =
      One_level.create (current_scope t) level ~just_after_level
    in
    with_current_level t ~current_level

let rec add_env_extension starting_t level : t =
  if not (Variable.Map.is_empty (Typing_env_level.defined_vars level)) then
  begin
    (* The full type system will remove this restriction. *)
    Misc.fatal_errorf "Typing environment extensions cannot define variables:\
        @ %a"
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

let cut t ~unknown_if_defined_at_or_later_than:min_scope =
  let current_scope = current_scope t in
  if Scope.(>) min_scope current_scope then
    Typing_env_level.empty, var_domain t
  else
    let all_levels =
      Scope.Map.add current_scope t.current_level t.prev_levels
    in
    let strictly_less, at_min_scope, strictly_greater =
      Scope.Map.split min_scope all_levels
    in
    let at_or_after_cut =
      match at_min_scope with
      | None -> strictly_greater
      | Some typing_env_level ->
        Scope.Map.add min_scope typing_env_level strictly_greater
    in
    let t =
      if Scope.Map.is_empty strictly_less then
        create ~resolver:t.resolver
      else
        let current_scope, current_level =
          Scope.Map.max_binding strictly_less
        in
        let prev_levels =
          Scope.Map.remove current_scope strictly_less
        in
        { resolver = t.resolver;
          prev_levels;
          current_level;
          next_binding_time = t.next_binding_time;
          defined_symbols = t.defined_symbols;
        }
    in
    invariant t;
(*
Format.eprintf "Cutting env, %a onwards:@ %a@ backtrace:@ %s\n%!"
  Scope.print min_scope
  print orig_t
  (Printexc.raw_backtrace_to_string (Printexc.get_callstack 15));
*)
    let meet_env = Meet_env.create t in
    let vars_in_scope_at_cut = Name.set_to_var_set (domain0 t) in
    let env_extension =
      Scope.Map.fold (fun _scope one_level result ->
(*
Format.eprintf "Folding in scope %a\n%!" Scope.print scope;
Format.eprintf "Level is:@ %a\n%!" Typing_env_level.print (One_level.level one_level);
*)
          let level =
            (* Since environment extensions are not allowed to define names at
               the moment, any [Equals] aliases to names not in scope at the cut
               point have to be squashed to "Unknown". *)
            One_level.level one_level
            |> Typing_env_level.remove_definitions_and_equations_thereon
            |> Typing_env_level.erase_aliases ~allowed:vars_in_scope_at_cut
          in
(*
Format.eprintf "Level for meet:@ %a\n%!" Typing_env_level.print level;
*)
          Typing_env_level.meet meet_env level result)
        at_or_after_cut
        Typing_env_level.empty
    in
(*
Format.eprintf "Portion cut off:@ %a\n%!" Typing_env_extension.print env_extension;
*)
    env_extension, vars_in_scope_at_cut

let get_canonical_name t name =
  match Aliases.get_canonical_name (aliases t) name with
  | None ->
    Misc.fatal_errorf "Cannot get canonical name for unbound \
        name %a:@ %a"
      Name.print name
      print t
  | Some name -> name

let aliases_of_simple t simple = Aliases.aliases_of_simple (aliases t) simple

let resolve_any_toplevel_alias_on_ty0 (type a) t
      ~(force_to_kind : Flambda_types.t -> a Flambda_types.ty)
      ~print_ty (ty : a Flambda_types.ty)
      : (a Flambda_types.unknown_or_join) * (Simple.t option) =
  let force_to_unknown_or_join typ =
    match force_to_kind typ with
    | No_alias unknown_or_join -> unknown_or_join
    | Type _ | Equals _ ->
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
    let name = get_canonical_name t name in
    let ty = force_to_kind (find t name) in
    match ty with
    | No_alias unknown_or_join -> unknown_or_join, Some (Simple.name name)
    | Type _export_id -> Misc.fatal_error ".cmx loading not yet implemented"
    | Equals _ ->
      Format.eprintf "@[<hov 1>%s>> Trying to resolve toplevel alias on%s:\
          @ %a@ %sCurrent aliases:%s@ %a@]\n"
        (Misc.Color.bold_red ())
        (Misc.Color.reset ())
        print_ty ty
        (Misc.Color.bold_red ())
        (Misc.Color.reset ())
        Aliases.print (aliases t);
      invariant_should_fail t

let resolve_any_toplevel_alias_on_ty (type a) t
      ~(force_to_kind : Flambda_types.t -> a Flambda_types.ty)
      ~print_ty (ty : a Flambda_types.ty)
      : (a Flambda_types.ty) * (Simple.t option) =
  match ty with
  | No_alias _ -> ty, None
  | Type _export_id -> Misc.fatal_error ".cmx loading not yet implemented"
  | Equals ((Const _ | Discriminant _) as simple) -> ty, Some simple
  | Equals (Name name) ->
    let name = get_canonical_name t name in
    let ty = force_to_kind (find t name) in
    match ty with
    | No_alias _ -> ty, Some (Simple.name name)
    | Type _export_id -> Misc.fatal_error ".cmx loading not yet implemented"
    | Equals _ ->
      Format.eprintf "@[<hov 1>%s>> Trying to resolve toplevel alias on%s:\
          @ %a@ %sCanonical name:%s %a@ %sCurrent aliases:%s@ %a@]\n"
        (Misc.Color.bold_red ())
        (Misc.Color.reset ())
        print_ty ty
        (Misc.Color.bold_red ())
        (Misc.Color.reset ())
        Name.print name
        (Misc.Color.bold_red ())
        (Misc.Color.reset ())
        Aliases.print (aliases t);
        invariant_should_fail t

let resolve_any_toplevel_alias t (ty : Flambda_types.t)
      : Flambda_types.t * (Simple.t option) =
  match ty with
  | Value ty_value ->
    let ty_value, canonical_simple =
      resolve_any_toplevel_alias_on_ty t
        ~force_to_kind:Flambda_type0_core.force_to_kind_value
        ~print_ty:Type_printers.print_ty_value
        ty_value
    in
    Value ty_value, canonical_simple
  | Naked_number (ty_naked_number, kind) ->
    let ty_naked_number, canonical_simple =
      resolve_any_toplevel_alias_on_ty t
        ~force_to_kind:(Flambda_type0_core.force_to_kind_naked_number kind)
        ~print_ty:Type_printers.print_ty_naked_number
        ty_naked_number
    in
    Naked_number (ty_naked_number, kind), canonical_simple
  | Fabricated ty_fabricated ->
    let ty_fabricated, canonical_simple =
      resolve_any_toplevel_alias_on_ty t
        ~force_to_kind:Flambda_type0_core.force_to_kind_fabricated
        ~print_ty:Type_printers.print_ty_fabricated
        ty_fabricated
    in
    Fabricated ty_fabricated, canonical_simple

let resolve_type t (ty : Flambda_types.t)
      : Flambda_types.resolved_t * (Simple.t option) =
  match ty with
  | Value ty_value ->
    let unknown_or_join, canonical_simple =
      resolve_any_toplevel_alias_on_ty0 t
        ~force_to_kind:Flambda_type0_core.force_to_kind_value
        ~print_ty:Type_printers.print_ty_value
        ty_value
    in
    Resolved_value unknown_or_join, canonical_simple
  | Naked_number (ty_naked_number, kind) ->
    let unknown_or_join, canonical_simple =
      resolve_any_toplevel_alias_on_ty0 t
        ~force_to_kind:(Flambda_type0_core.force_to_kind_naked_number kind)
        ~print_ty:Type_printers.print_ty_naked_number
        ty_naked_number
    in
    Resolved_naked_number (unknown_or_join, kind), canonical_simple
  | Fabricated ty_fabricated ->
    let unknown_or_join, canonical_simple =
      resolve_any_toplevel_alias_on_ty0 t
        ~force_to_kind:Flambda_type0_core.force_to_kind_fabricated
        ~print_ty:Type_printers.print_ty_fabricated
        ty_fabricated
    in
    Resolved_fabricated unknown_or_join, canonical_simple

let create_using_resolver_and_symbol_bindings_from t =
  let names_to_types =
    Name.Map.filter_map (names_to_types t)
      ~f:(fun (name : Name.t) (typ, binding_time) ->
        match name with
        | Var _ -> None
        | Symbol _ ->
          let typ =
            Type_erase_aliases.erase_aliases typ ~allowed:Variable.Set.empty
          in
          Some (typ, binding_time))
  in
  let t =
    Name.Map.fold (fun name (typ, _binding_time) t ->
        add_definition t name (Flambda_type0_core.kind typ))
      names_to_types
      (create_using_resolver_from t)
  in
  Name.Map.fold (fun name (typ, _binding_time) t ->
      add_equation t name typ)
    names_to_types
    t
