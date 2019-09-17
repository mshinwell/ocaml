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

module Aliases = Aliases.Make (Alias)

(* CR mshinwell: Add signatures to these submodules. *)
module Cached : sig
  type t

  val empty : t

  val names_to_types
     : t
    -> (Type_grammar.t * Binding_time.t * Name_occurrence_kind.t) Name.Map.t

  val aliases : t -> Aliases.t

  val cse : t -> Simple.t Flambda_primitive.Eligible_for_cse.Map.t

  val binding_time : t -> Name.t -> Binding_time.t

  val domain : t -> Name.Set.t

  val var_domain : t -> Variable.Set.t

  val add_or_replace_binding
     : t
    -> Name.t
    -> Type_grammar.t
    -> Binding_time.t
    -> Name_occurrence_kind.t
    -> new_aliases:Aliases.t
    -> t

  val with_cse : t -> cse:Simple.t Flambda_primitive.Eligible_for_cse.Map.t -> t
end = struct
  type t = {
    names_to_types :
      (Type_grammar.t * Binding_time.t * Name_occurrence_kind.t) Name.Map.t;
    domain : Name.Set.t;
    var_domain : Variable.Set.t;
    aliases : Aliases.t;
    cse : Simple.t Flambda_primitive.Eligible_for_cse.Map.t;
  }

(*
  let _print_with_cache ~cache ppf { names_to_types; aliases; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(names_to_types@ %a)@]@ \
        @[<hov 1>(aliases@ %a)@]\
        )@]"
      (Name.Map.print (Type_grammar.print_with_cache ~cache)) names_to_types
      Aliases.print aliases

  let _invariant t =
    let canonical_names = Aliases.canonical_names t.aliases in
    Name.Set.iter (fun name ->
        match Name.Map.find name t.names_to_types with
        | exception Not_found ->
          Misc.fatal_errorf "Canonical name %a not in [names_to_types]"
            Name.print name
        | ty ->
          match Type_grammar.get_alias ty with
          | None -> ()
          | Some alias_of ->
            Misc.fatal_errorf "Canonical name %a has an alias type: =%a"
              Name.print name
              Simple.print alias_of)
      canonical_names
*)

  (* CR mshinwell: add [invariant] function *)

  let empty =
    { names_to_types = Name.Map.empty;
      domain = Name.Set.empty;
      var_domain = Variable.Set.empty;
      aliases = Aliases.empty;
      cse = Flambda_primitive.Eligible_for_cse.Map.empty;
    }

  let names_to_types t = t.names_to_types
  let domain t = t.domain
  let var_domain t = t.var_domain
  let aliases t = t.aliases
  let cse t = t.cse

  let add_or_replace_binding t (name : Name.t) ty binding_time
        name_occurrence_kind ~new_aliases =
    let names_to_types =
      Name.Map.add name (ty, binding_time, name_occurrence_kind)
        t.names_to_types
    in
    let domain = Name.Set.add name t.domain in
    let var_domain =
      match name with
      | Var var -> Variable.Set.add var t.var_domain
      | Symbol _ -> t.var_domain
    in
    { names_to_types;
      domain;
      var_domain;
      aliases = new_aliases;
      cse = t.cse;
    }

  (* CR mshinwell: Add type lookup function *)

  let binding_time t name =
    match Name.Map.find name t.names_to_types with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound name %a" Name.print name
    | (_ty, time, _name_occurrence_kind) -> time

  let with_cse t ~cse = { t with cse; }
end

module One_level = struct
  type t = {
    scope : Scope.t;
    level : Typing_env_level.t;
    just_after_level : Cached.t;
  }

  let print_with_cache ~cache:_ ppf
        { scope; level; just_after_level = _; } =
    Format.fprintf ppf "@[<hov 1>\
        @[<hov 1>(scope@ %a)@]@ \
        @[<hov 1>(level@ %a)@]\
        @]"
      Scope.print scope
      Typing_env_level.print level

  let create scope level ~just_after_level =
    { scope;
      level;
      just_after_level;
    }

  let create_empty scope =
    { scope;
      level = Typing_env_level.empty ();
      just_after_level = Cached.empty;
    }

  let scope t = t.scope
  let level t = t.level
  let just_after_level t = t.just_after_level

  let is_empty t = Typing_env_level.is_empty t.level

  let next_scope t = Scope.next t.scope
end

type t = {
  resolver : (Export_id.t -> Type_grammar.t option);
  defined_symbols : Flambda_kind.t Symbol.Map.t;
  prev_levels : One_level.t Scope.Map.t;
  (* CR mshinwell: hold list of symbol definitions, then change defined_names
     to variables, then remove artificial symbol precedence *)
  current_level : One_level.t;
  next_binding_time : Binding_time.t;
}

let is_empty t =
  One_level.is_empty t.current_level
    && Scope.Map.is_empty t.prev_levels
    && Symbol.Map.is_empty t.defined_symbols

(* CR mshinwell: Should print name occurrence kinds *)
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
            @[<hov 1>(defined_symbols@ %a)@]@ \
            @[<hov 1>(aliases@ %a)@]\
            )@]"
        (Scope.Map.print (One_level.print_with_cache ~cache)) prev_levels
        (One_level.print_with_cache ~cache) current_level
        (Symbol.Map.print K.print) defined_symbols
        Aliases.print
        (Cached.aliases (One_level.just_after_level current_level)))

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant0 ?force _t =
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
    let current_scope = One_level.scope t.current_level in
    let max_prev_scope =
      Scope.Map.fold (fun scope _level max_prev_scope ->
          Scope.max scope max_prev_scope)
        t.prev_levels
        Scope.initial
    in
    if (not (is_empty t))
      && Scope.(<=) current_scope max_prev_scope
    then begin
      Misc.fatal_errorf "Typing environment contains a [current_level] with a \
          scope (%a) that is not strictly greater than all scopes in \
          [prev_levels] (%a):@ %a"
        Scope.print current_scope
        Scope.print max_prev_scope
        print t
    end
*)
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
    defined_symbols = Symbol.Map.empty;
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
    One_level.create scope (Typing_env_level.empty ())
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
  Name_occurrences.create_names (domain0 t) Name_occurrence_kind.in_types

let var_domain t =
  Cached.var_domain (One_level.just_after_level t.current_level)

let find t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment:@ %a"
      Name.print name
      print t
  (* CR mshinwell: Should this resolve aliases? *)
  | ty, _binding_time, _name_occurrence_kind -> ty

let _find_with_occurrence_kind t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment:@ %a"
      Name.print name
      print t
  (* CR mshinwell: Should this resolve aliases? *)
  | ty, _binding_time, name_occurrence_kind -> ty, name_occurrence_kind

let find_with_binding_time t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment:@ %a"
      Name.print name
      print t
  | ty, binding_time, _name_occurrence_kind -> ty, binding_time

let find_name_occurrence_kind t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment:@ %a"
      Name.print name
      print t
  (* CR mshinwell: Should this resolve aliases? *)
  | _ty, _binding_time, name_occurrence_kind -> name_occurrence_kind

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

let add_variable_definition t var kind name_occurrence_kind =
  let name = Name.var var in
  if mem t name then begin
    Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
      Name.print name
      print t
  end;
  let level =
    Typing_env_level.add_definition (One_level.level t.current_level) var kind
      t.next_binding_time
  in
  let just_after_level =
    let aliases =
      let canonical =
        Alias.create kind (Simple.name name) t.next_binding_time
          name_occurrence_kind
      in
      Aliases.add_canonical_element (aliases t) canonical
    in
    Cached.add_or_replace_binding (cached t)
      name (Type_grammar.unknown kind)
      t.next_binding_time name_occurrence_kind
      ~new_aliases:aliases
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level_and_next_binding_time t ~current_level
    (Binding_time.succ t.next_binding_time)

let add_symbol_definition t sym kind =
  let name_occurrence_kind = Name_occurrence_kind.normal in
  let name = Name.symbol sym in
  let just_after_level =
    let aliases =
      let canonical =
        Alias.create kind (Simple.name name) Binding_time.symbols
          name_occurrence_kind
      in
      Aliases.add_canonical_element (aliases t) canonical
    in
    Cached.add_or_replace_binding (cached t)
      name (Type_grammar.unknown kind)
      Binding_time.symbols name_occurrence_kind
      ~new_aliases:aliases
  in
  let current_level =
    One_level.create (current_scope t) (One_level.level t.current_level)
      ~just_after_level
  in
  let t = with_current_level t ~current_level in
  { t with
    defined_symbols = Symbol.Map.add sym kind t.defined_symbols;
  }

let alias_of_simple t simple =
  let kind, binding_time =
    match Simple.descr simple with
    | Const const ->
      Type_grammar.kind_for_const const,
        Binding_time.consts_and_discriminants
    | Discriminant _ ->
      K.fabricated, Binding_time.consts_and_discriminants
    | Name name ->
      let ty, binding_time = find_with_binding_time t name in
      Type_grammar.kind ty, binding_time
  in
  Alias.create kind simple binding_time

let add_definition t (name : Name_in_binding_pos.t) kind =
  let occurrence_kind = Name_in_binding_pos.occurrence_kind name in
  match Name_in_binding_pos.name name with
  | Var var -> add_variable_definition t var kind occurrence_kind
  | Symbol sym ->
    if not (Name_occurrence_kind.equal occurrence_kind
      Name_occurrence_kind.normal)
    then begin
      Misc.fatal_errorf "Cannot define symbol %a with occurrence kind that \
          is not `normal'"
        Name_in_binding_pos.print name
    end;
    add_symbol_definition t sym kind

let invariant_for_new_equation _ _ _ = ()
(* XXX guard
(* CR mshinwell: This should check that precision is not decreasing. *)
let invariant_for_new_equation t name ty =
  let defined_names = domain t in
  let free_names = Type_free_names.free_names ty in
  if not (Name_occurrences.subset_domain free_names defined_names) then begin
    let unbound_names = Name_occurrences.diff free_names (domain t) in
    Misc.fatal_errorf "New equation@ %a@ =@ %a@ has unbound names@ (%a):@ %a"
      Name.print name
      Type_grammar.print ty
      Name_occurrences.print unbound_names
      print t
  end
*)

let add_equation0 t aliases name name_occurrence_kind ty =
  invariant_for_new_equation t name ty;
  let level =
    Typing_env_level.add_or_replace_equation
      (One_level.level t.current_level) name ty
  in
  let just_after_level = One_level.just_after_level t.current_level in
  (* CR mshinwell: remove second lookup *)
  let binding_time = Cached.binding_time just_after_level name in
  let just_after_level =
    Cached.add_or_replace_binding just_after_level
      name ty binding_time name_occurrence_kind
      ~new_aliases:aliases
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level t ~current_level

let add_equation t name ty =
(*
Format.eprintf "Adding equation %a : %a\n%!"
  Name.print name
  Type_grammar.print ty;
  *)
  let name_occurrence_kind = find_name_occurrence_kind t name in
  (* XXX Needs to be guarded
  let free_names = Type_free_names.free_names ty in
  if not (Name_occurrences.subset_domain free_names (domain t))
  then begin
    let unbound_names = Name_occurrences.diff free_names (domain t) in
    Misc.fatal_errorf "Cannot add equation, involving unbound names@ (%a),@ on \
        name@ %a =@ %a@ (free names %a) in environment with domain %a:@ %a"
      Name_occurrences.print unbound_names
      Name.print name
      Type_grammar.print ty
      Name_occurrences.print free_names
      Name_occurrences.print (domain t)
      print t
  end;
  *)
  begin match Type_grammar.get_alias ty with
  | None -> ()
  | Some simple ->
    match Simple.descr simple with
    | Name name' ->
      if Name.equal name name' then begin
        Misc.fatal_errorf "Directly recursive equation@ %a = %a@ \
            disallowed:@ %a"
          Name.print name
          Type_grammar.print ty
          print t
      end
    | _ -> ()
  end;
  (*
Format.eprintf "Trying to add equation %a = %a\n%!"
  Name.print name
  Type_grammar.print ty;
Format.eprintf "Aliases before adding equation:@ %a\n%!"
  Aliases.print (aliases t);
  *)
  let aliases, simple, rec_info, ty =
    let aliases = aliases t in
    match Type_grammar.get_alias ty with
    | None -> aliases, Simple.name name, None, ty
    | Some alias_of ->
      let kind = Type_grammar.kind ty in
      let alias =
        let binding_time = Cached.binding_time (cached t) name in
        Alias.create_name kind name binding_time name_occurrence_kind
      in
      let rec_info = Simple.rec_info alias_of in
      let alias_of =
        let name_occurrence_kind =
          match Simple.descr alias_of with
          | Const _ | Discriminant _ -> Name_occurrence_kind.normal
          | Name alias_of -> find_name_occurrence_kind t alias_of
        in
        alias_of_simple t alias_of name_occurrence_kind
      in
      match Aliases.add aliases alias alias_of with
      | None, aliases -> aliases, Alias.simple alias_of, rec_info, ty
      | (Some { canonical_element; alias_of; }), aliases ->
(*
Format.eprintf "For name %a, Aliases returned CN=%a, alias_of=%a\n%!"
   Name.print name
   Alias.print canonical_element
   Alias.print alias_of; 
   *)
        let kind = Type_grammar.kind ty in
        let ty =
          Type_grammar.alias_type_of kind
            (Alias.simple canonical_element)
        in
        aliases, Alias.simple alias_of, rec_info, ty
  in
  let ty =
    match rec_info with
    | None -> ty
    | Some rec_info ->
      match Type_grammar.apply_rec_info ty rec_info with
      | Bottom -> Type_grammar.bottom (Type_grammar.kind ty)
      | Ok ty -> ty
  in
(*
Format.eprintf "Now really adding equation %a = %a\n%!"
  Simple.print simple
  Type_grammar.print ty;
  *)
(*
 * Format.eprintf "Aliases after adding equation %a = %a:@ %a\n%!"
 *   Simple.print simple
 *   Type_grammar.print ty
 *   Aliases.print aliases; *)
  match Simple.descr simple with
  | Const _ | Discriminant _ -> t
  | Name name ->
    add_equation0 t aliases name name_occurrence_kind ty

let add_cse t prim ~bound_to =
  let level =
    Typing_env_level.add_cse (One_level.level t.current_level) prim ~bound_to
  in
  let cached = cached t in
  let cse =
    let cse = Cached.cse cached in
    match Flambda_primitive.Eligible_for_cse.Map.find prim cse with
    | exception Not_found ->
      Flambda_primitive.Eligible_for_cse.Map.add prim bound_to cse
    | _bound_to -> cse
  in
  let just_after_level = Cached.with_cse cached ~cse in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level t ~current_level

let find_cse t prim =
  let cse = Cached.cse (cached t) in
  match Flambda_primitive.Eligible_for_cse.Map.find prim cse with
  | exception Not_found -> None
  | bound_to -> Some bound_to

let find_cse_rev t ~bound_to =
  (* CR mshinwell: Store reverse map and add abstract type. *)
  let cse = Cached.cse (cached t) in
  let rev_cse =
    Flambda_primitive.Eligible_for_cse.Map.bindings cse
    |> List.map (fun (equation, bound_to) -> bound_to, equation)
    |> Simple.Map.of_list
  in
  match Simple.Map.find bound_to rev_cse with
  | exception Not_found -> None
  | equation -> Some equation

let add_env_extension_from_level t level : t =
  let t =
    List.fold_left (fun t (var, kind) ->
        add_variable_definition t var kind Name_occurrence_kind.in_types)
      t
      (Typing_env_level.defined_vars_in_order level)
  in
  let t =
    Name.Map.fold (fun name ty t ->
        (* CR mshinwell: Do we actually need the "more precise" check here?
            Shouldn't the extensions always be as or more precise? *)
        add_equation t name ty)
      (Typing_env_level.equations level)
      t
  in
  Flambda_primitive.Eligible_for_cse.Map.fold (fun prim bound_to t ->
      add_cse t prim ~bound_to)
    (Typing_env_level.cse level)
    t

let add_env_extension t env_extension =
  Typing_env_extension.pattern_match env_extension ~f:(fun level ->
    add_env_extension_from_level t level)

let cut t ~unknown_if_defined_at_or_later_than:min_scope =
  let current_scope = current_scope t in
  let original_t = t in
  if Scope.(>) min_scope current_scope then
    Typing_env_level.empty (), var_domain t
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
        let t = create ~resolver:t.resolver in
        Symbol.Map.fold (fun symbol kind t ->
            add_symbol_definition t symbol kind)
          original_t.defined_symbols
          t
      else
        let current_scope, current_level =
          Scope.Map.max_binding strictly_less
        in
        let prev_levels =
          Scope.Map.remove current_scope strictly_less
        in
        let t =
          { resolver = t.resolver;
            prev_levels;
            current_level;
            next_binding_time = t.next_binding_time;
            defined_symbols = t.defined_symbols;
          }
        in
        Symbol.Map.fold (fun symbol kind t ->
            if not (mem t (Name.symbol symbol)) then
              add_symbol_definition t symbol kind
            else
              t)
          original_t.defined_symbols
          t
    in
    invariant t;
(* Format.eprintf "Cutting env, %a onwards:@ %a@ backtrace:@ %s\n%!"
 *   Scope.print min_scope
 *   print original_t
 *   (Printexc.raw_backtrace_to_string (Printexc.get_callstack 15)); *)
    let level =
      Scope.Map.fold (fun _scope one_level result ->
          Typing_env_level.concat result (One_level.level one_level))
        at_or_after_cut
        (Typing_env_level.empty ())
    in
(* Format.eprintf "Portion cut off:@ %a\n%!" Typing_env_extension.print env_extension; *)
    let vars_in_scope_at_cut = Name.set_to_var_set (domain0 t) in
    level, vars_in_scope_at_cut

let cut_and_n_way_join definition_typing_env ts_and_use_ids
      ~unknown_if_defined_at_or_later_than =
  let after_cuts =
    List.map (fun (t, use_id, interesting_vars) ->
        let level, _in_scope = cut t ~unknown_if_defined_at_or_later_than in
        t, use_id, interesting_vars, level)
      ts_and_use_ids
  in
  let level, extra_params_and_args =
    Typing_env_level.n_way_join definition_typing_env after_cuts
  in
  let env_extension = Typing_env_extension.create level in
  env_extension, extra_params_and_args

let find_name_occurrence_kind_of_simple t simple =
  match Simple.descr simple with
  | Const _ | Discriminant _ -> Name_occurrence_kind.normal
  | Name name -> find_name_occurrence_kind t name

let get_canonical_simple0 t ?min_occurrence_kind simple : _ Or_bottom.t * _ =
  let newer_rec_info =
    let newer_rec_info = Simple.rec_info simple in
    match Simple.descr simple with
    | Const _ | Discriminant _ -> newer_rec_info
    | Name name ->
      let ty = find t name in
      match Type_grammar.get_alias ty with
      | None -> newer_rec_info
      | Some simple ->
        match Simple.rec_info simple with
        | None -> newer_rec_info
        | Some rec_info ->
          match newer_rec_info with
          | None -> Some rec_info
          | Some newer_rec_info ->
            Some (Rec_info.merge rec_info ~newer:newer_rec_info)
  in
  let occurrence_kind = find_name_occurrence_kind_of_simple t simple in
  let alias = alias_of_simple t simple occurrence_kind in
  let kind = Alias.kind alias in
  let min_order_within_equiv_class =
    match min_occurrence_kind with
    | None -> occurrence_kind
    | Some occurrence_kind -> occurrence_kind
  in
  let result =
    Aliases.get_canonical_element (aliases t) alias
      ~min_order_within_equiv_class
  in 
  match result with
  | None -> Ok None, kind
  | Some alias ->
    let simple = Alias.simple alias in
    (* CR mshinwell: Check that [simple] has no [Rec_info] on it *)
    match Simple.merge_rec_info simple ~newer_rec_info with
    | None -> Bottom, kind
    | Some simple ->
      let rec_info = Simple.rec_info simple in
      match Simple.descr simple with
      | Const _ | Discriminant _ -> Ok (Some (simple, rec_info)), kind
      | Name name ->
        (* CR mshinwell: Do we have to return [Bottom]? *)
        let ty = find t name in
        if Type_grammar.is_obviously_bottom ty
        then Bottom, kind
        else Ok (Some (simple, rec_info)), kind

let get_canonical_simple_with_kind t ?min_occurrence_kind simple =
  let result, kind = get_canonical_simple0 t ?min_occurrence_kind simple in
  let result =
    Or_bottom.map result ~f:(fun result ->
      Option.map (fun (simple, _rec_info) -> simple) result)
  in
  result, kind

let get_canonical_simple t ?min_occurrence_kind simple =
  fst (get_canonical_simple_with_kind t ?min_occurrence_kind simple)

let get_alias_then_canonical_simple t ?min_occurrence_kind typ : _ Or_bottom.t =
  match Type_grammar.get_alias typ with
  | None -> Ok None
  | Some simple -> get_canonical_simple t ?min_occurrence_kind simple

let aliases_of_simple t ~min_occurrence_kind simple =
  let name_occurrence_kind = find_name_occurrence_kind_of_simple t simple in
  let alias = alias_of_simple t simple name_occurrence_kind in
  let newer_rec_info = Simple.rec_info simple in
  Alias.Set.fold (fun alias simples ->
      let name_occurrence_kind = Alias.name_occurrence_kind alias in
      if Name_occurrence_kind.compare name_occurrence_kind
        min_occurrence_kind < 0
      then
        simples
      else
        let simple = Alias.simple alias in
        match Simple.merge_rec_info simple ~newer_rec_info with
        | None -> simples
        | Some simple -> Simple.Set.add simple simples)
    (Aliases.get_aliases (aliases t) alias)
    Simple.Set.empty

let aliases_of_simple_allowable_in_types t simple =
  aliases_of_simple t ~min_occurrence_kind:Name_occurrence_kind.in_types simple

let defined_earlier t simple ~than =
  Alias.defined_earlier
    (alias_of_simple t simple
      (find_name_occurrence_kind_of_simple t simple))
    ~than:(alias_of_simple t than
      (find_name_occurrence_kind_of_simple t than))

let create_using_resolver_and_symbol_bindings_from t =
  let original_t = t in
  let names_to_types = names_to_types t in
  let t =
    Name.Map.fold
      (fun (name : Name.t) (typ, _binding_time, occurrence_kind) t ->
        match name with
        | Var _ -> t
        | Symbol _ ->
          let name = Name_in_binding_pos.create name occurrence_kind in
          add_definition t name (Type_grammar.kind typ))
      names_to_types
      (create_using_resolver_from t)
  in
  Name.Map.fold
    (fun (name : Name.t) (typ, _binding_time, _occurrence_kind) t ->
      match name with
      | Var _ -> t
      | Symbol _ ->
        let env_extension, typ =
          Type_grammar.make_suitable_for_environment typ original_t
            ~suitable_for:t
        in
        let t = add_env_extension t env_extension in
        add_equation t name typ)
    names_to_types
    t

let free_variables_transitive t typ =
  let rec free_variables_transitive typ ~seen =
    let free_variables =
      Name_occurrences.variables (Type_grammar.free_names typ)
    in
    let seen = Variable.Set.union seen free_variables in
    Variable.Set.fold (fun var result ->
        if Variable.Set.mem var seen then result
        else
          let typ = find t (Name.var var) in
          Variable.Set.union (free_variables_transitive typ ~seen) result)
      free_variables
      free_variables
  in
  free_variables_transitive typ ~seen:Variable.Set.empty
