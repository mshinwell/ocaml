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

  val print_name_modes
     : restrict_to:Name.Set.t
    -> Format.formatter
    -> t
    -> unit

  val empty : t

  val names_to_types
     : t
    -> (Type_grammar.t * Binding_time.t * Name_mode.t) Name.Map.t

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
    -> Name_mode.t
    -> new_aliases:Aliases.t
    -> t

  val with_cse : t -> cse:Simple.t Flambda_primitive.Eligible_for_cse.Map.t -> t
end = struct
  type t = {
    names_to_types :
      (Type_grammar.t * Binding_time.t * Name_mode.t) Name.Map.t;
    domain : Name.Set.t;
    var_domain : Variable.Set.t;
    aliases : Aliases.t;
    cse : Simple.t Flambda_primitive.Eligible_for_cse.Map.t;
  }

  let print_kind_and_mode ppf (ty, _, mode) =
    let kind = Type_grammar.kind ty in
    Format.fprintf ppf ":: %a %a"
      Flambda_kind.print kind
      Name_mode.print mode

  let print_name_modes ~restrict_to ppf t =
    Name.Map.print print_kind_and_mode ppf
      (Name.Map.filter (fun name _ -> Name.Set.mem name restrict_to)
        t.names_to_types)

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
        name_mode ~new_aliases =
    let names_to_types =
      Name.Map.add name (ty, binding_time, name_mode)
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
    | (_ty, time, _name_mode) -> time

  let with_cse t ~cse = { t with cse; }
end

module One_level = struct
  type t = {
    scope : Scope.t;
    level : Typing_env_level.t;
    just_after_level : Cached.t;
  }

  let print_with_cache ~cache:_ ppf
        { scope = _; level; just_after_level; } =
    let restrict_to = Typing_env_level.defined_names level in
    if Name.Set.is_empty restrict_to then
      Format.fprintf ppf "@[<hov 0>\
          %a\
          @]"
        Typing_env_level.print level
    else
      Format.fprintf ppf "@[<hov 0>\
          @[<hov 1>(defined_vars@ %a)@]@ \
          %a\
          @]"
        (Cached.print_name_modes ~restrict_to) just_after_level
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

  let defines_name_but_no_equations t name =
    Typing_env_level.defines_name_but_no_equations t.level name
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
(* CR mshinwell: Add option to print [aliases] *)
let print_with_cache ~cache ppf
      ({ resolver = _; prev_levels; current_level; next_binding_time = _;
         defined_symbols;
       } as t) =
  if is_empty t then
    Format.pp_print_string ppf "Empty"
  else
    let levels =
      Scope.Map.add (One_level.scope current_level) current_level prev_levels
    in
    let levels =
      Scope.Map.filter (fun _ level -> not (One_level.is_empty level))
        levels
    in
    Printing_cache.with_cache cache ppf "env" t (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(\
            @[<hov 1>(defined_symbols@ %a)@]@ \
            @[<hov 1>(levels@ %a)@]\
            )@]"
        (Symbol.Map.print K.print) defined_symbols
        (Scope.Map.print (One_level.print_with_cache ~cache)) levels)

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

let domain0 t =
  Cached.domain (One_level.just_after_level t.current_level)

let var_domain t =
  Cached.var_domain (One_level.just_after_level t.current_level)

let name_domain t =
  Name.Set.union (Name.set_of_var_set (var_domain t))
    (Name.set_of_symbol_set (Symbol.Map.keys t.defined_symbols))

let find t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment:@ %a"
      Name.print name
      print t
  (* CR mshinwell: Should this resolve aliases? *)
  | ty, _binding_time, _name_mode -> ty

let find_params t params =
  List.map (fun param -> find t (Kinded_parameter.name param)) params

let find_with_name_mode t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment:@ %a"
      Name.print name
      print t
  (* CR mshinwell: Should this resolve aliases? *)
  | ty, _binding_time, name_mode -> ty, name_mode

let find_with_binding_time_and_mode t name =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment:@ %a"
      Name.print name
      print t
  | ty, binding_time, name_mode -> ty, binding_time, name_mode

let mem t name =
  Name.Map.mem name (names_to_types t)

let mem_simple t simple =
  match Simple.descr simple with
  | Name name -> mem t name
  | Const _ -> true

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

let add_variable_definition t var kind name_mode =
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
          name_mode
      in
      Aliases.add_canonical_element (aliases t) canonical
    in
    Cached.add_or_replace_binding (cached t)
      name (Type_grammar.unknown kind)
      t.next_binding_time name_mode
      ~new_aliases:aliases
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level_and_next_binding_time t ~current_level
    (Binding_time.succ t.next_binding_time)

let add_symbol_definition t sym kind =
  let name_mode = Name_mode.normal in
  let name = Name.symbol sym in
  let just_after_level =
    let aliases =
      let canonical =
        Alias.create kind (Simple.name name) Binding_time.symbols
          name_mode
      in
      Aliases.add_canonical_element (aliases t) canonical
    in
    Cached.add_or_replace_binding (cached t)
      name (Type_grammar.unknown kind)
      Binding_time.symbols name_mode
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
  let kind, binding_time, mode =
    match Simple.descr simple with
    | Const const ->
      Type_grammar.kind_for_const const,
        Binding_time.consts_and_discriminants,
        Name_mode.normal
    | Name name ->
      let ty, binding_time, mode = find_with_binding_time_and_mode t name in
      Type_grammar.kind ty, binding_time, mode
  in
  Alias.create kind simple binding_time mode

let add_definition t (name : Name_in_binding_pos.t) kind =
  let name_mode = Name_in_binding_pos.name_mode name in
  match Name_in_binding_pos.name name with
  | Var var -> add_variable_definition t var kind name_mode
  | Symbol sym ->
    if not (Name_mode.equal name_mode Name_mode.normal) then begin
      Misc.fatal_errorf "Cannot define symbol %a with name mode that \
          is not `normal'"
        Name_in_binding_pos.print name
    end;
    add_symbol_definition t sym kind

let invariant_for_new_equation t name ty =
  if !Clflags.flambda_invariant_checks then begin
    (* CR mshinwell: This should check that precision is not decreasing. *)
    let defined_names =
      Name_occurrences.create_names (domain0 t) Name_mode.in_types
    in
    let free_names = Type_grammar.free_names ty in
    if not (Name_occurrences.subset_domain free_names defined_names) then begin
      let unbound_names = Name_occurrences.diff free_names defined_names in
      Misc.fatal_errorf "New equation@ %a@ =@ %a@ has unbound names@ (%a):@ %a"
        Name.print name
        Type_grammar.print ty
        Name_occurrences.print unbound_names
        print t
    end
  end

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

let add_equation0 t aliases name name_mode ty =
  invariant_for_new_equation t name ty;
  if Type_grammar.is_obviously_unknown ty
    && One_level.defines_name_but_no_equations t.current_level name
  then t
  else
    let level =
      Typing_env_level.add_or_replace_equation
        (One_level.level t.current_level) name ty
    in
    let just_after_level = One_level.just_after_level t.current_level in
    (* CR mshinwell: remove second lookup *)
    let binding_time = Cached.binding_time just_after_level name in
    let just_after_level =
      Cached.add_or_replace_binding just_after_level
        name ty binding_time name_mode
        ~new_aliases:aliases
    in
    let current_level =
      One_level.create (current_scope t) level ~just_after_level
    in
    with_current_level t ~current_level

let rec add_equation t name ty =
(*
Format.eprintf "Adding equation %a : %a from:\n%s\n%!"
  Name.print name
  Type_grammar.print ty
  (Printexc.raw_backtrace_to_string (Printexc.get_callstack 5));
*)
  let existing_ty, name_mode = find_with_name_mode t name in
  if not (K.equal (Type_grammar.kind existing_ty) (Type_grammar.kind ty))
  then begin
    Misc.fatal_errorf "Cannot add equation %a = %a@ given existing binding \
        %a = %a@ whose type is of a different kind:@ %a"
      Name.print name
      Type_grammar.print ty
      Name.print name
      Type_grammar.print existing_ty
      print t
  end;
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
    | Const _ -> ()
  end;
  (*
Format.eprintf "Trying to add equation %a = %a\n%!"
  Name.print name
  Type_grammar.print ty;
Format.eprintf "Aliases before adding equation:@ %a\n%!"
  Aliases.print (aliases t);
  *)
  let aliases, simple, name_mode, rec_info, t, ty =
    let aliases = aliases t in
    match Type_grammar.get_alias ty with
    | None -> aliases, Simple.name name, name_mode, None, t, ty
    | Some alias_of ->
      let kind = Type_grammar.kind ty in
      let alias =
        let binding_time = Cached.binding_time (cached t) name in
        Alias.create_name kind name binding_time name_mode
      in
      let rec_info = Simple.rec_info alias_of in
      let alias_of = alias_of_simple t alias_of in
      match Aliases.add aliases alias alias_of with
      | None, aliases ->
        aliases, Alias.simple alias_of, Alias.name_mode alias_of,
          rec_info, t, ty
      | (Some { canonical_element; alias_of; }), aliases ->
(*
Format.eprintf "For name %a, Aliases returned CN=%a, alias_of=%a\n%!"
   Name.print name
   Alias.print canonical_element
   Alias.print alias_of; 
   *)
        let kind = Type_grammar.kind ty in
        assert (K.equal kind (Alias.kind canonical_element));
        let ty =
          Type_grammar.alias_type_of kind
            (Alias.simple canonical_element)
        in
        aliases, Alias.simple alias_of, Alias.name_mode alias_of,
          rec_info, t, ty
  in
  (* Beware: if we're about to add the equation on a name which is different
     from the one that the caller passed in, then we need to make sure that the
     type we assign to that name is the most precise available. This
     necessitates calling [meet].

     For example, suppose [p] is defined earlier than [x], with [p] of type
     [Unknown] and [x] of type [ty]. If the caller says that the best type of
     [p] is now to be "= x", then this function will add an equation on [x]
     rather than [p], due to the definition ordering. However we should not just
     say that [x] has type "= p", as that would forget any existing information
     about [x]. Instead we should say that [x] has type "(= p) meet ty".

     Note also that [p] and [x] may have different name modes! *)
  let ty, t =
    match Simple.descr simple with
    | Name eqn_name when not (Name.equal name eqn_name) ->
      let env = Meet_env.create t in
      let existing_ty = find t eqn_name in
(*
      Format.eprintf "%a = new ty %a, existing ty %a, for meet\n%!"
        Simple.print simple
        Type_grammar.print ty
        Type_grammar.print existing_ty;
*)
      begin match Type_grammar.meet env ty existing_ty with
      | Bottom -> Type_grammar.bottom_like ty, t
      | Ok (meet_ty, env_extension) ->
(*
        Format.eprintf "meet_ty is %a\n%!" Type_grammar.print meet_ty;
*)
        meet_ty, add_env_extension t ~env_extension
      end
    | Name _ | Const _ -> ty, t
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
  match Simple.descr simple with
  | Const _ -> t
  | Name name -> add_equation0 t aliases name name_mode ty

and add_env_extension_from_level t level : t =
  let t =
    List.fold_left (fun t (var, kind) ->
        add_variable_definition t var kind Name_mode.in_types)
      t
      (Typing_env_level.defined_vars_in_order level)
  in
  let t =
    Name.Map.fold (fun name ty t -> add_equation t name ty)
      (Typing_env_level.equations level)
      t
  in
  Flambda_primitive.Eligible_for_cse.Map.fold (fun prim bound_to t ->
      add_cse t prim ~bound_to)
    (Typing_env_level.cse level)
    t

and add_env_extension t ~env_extension =
  Typing_env_extension.pattern_match env_extension ~f:(fun level ->
    add_env_extension_from_level t level)

let add_definitions_of_params t ~params =
  List.fold_left (fun t param ->
      let name =
        Name_in_binding_pos.create (Kinded_parameter.name param)
          Name_mode.normal
      in
      add_definition t name (Kinded_parameter.kind param))
    t
    params

let add_equations_on_params t ~params ~param_types =
  if List.compare_lengths params param_types <> 0 then begin
    Misc.fatal_errorf "Mismatch between number of [params] and \
        [param_types]:@ (%a)@ and@ %a"
      Kinded_parameter.List.print params
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Type_grammar.print)
      param_types
  end;
  List.fold_left2 (fun t param param_type ->
      add_equation t (Kinded_parameter.name param) param_type)
    t
    params param_types

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

let meet_equation t name ty =
  let existing_ty = find t name in
  Format.eprintf "For %a, new type is %a, existing type %a\n%!"
    Name.print name
    Type_grammar.print ty
    Type_grammar.print existing_ty;
  match Type_grammar.meet (Meet_env.create t) ty existing_ty with
  | Bottom -> add_equation t name (Type_grammar.bottom_like ty)
  | Ok (meet_ty, env_extension) ->
    let t = add_env_extension t ~env_extension in
    add_equation t name meet_ty

(* CR mshinwell: May not need these "meet" functions *)

let meet_equations_on_params t ~params ~param_types =
  if List.compare_lengths params param_types <> 0 then begin
    Misc.fatal_errorf "Mismatch between number of [params] and \
        [param_types]:@ (%a)@ and@ %a"
      Kinded_parameter.List.print params
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Type_grammar.print)
      param_types
  end;
  List.fold_left2 (fun t param param_type ->
      meet_equation t (Kinded_parameter.name param) param_type)
    t
    params param_types

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
  (* CR mshinwell: Can't [unknown_if_defined_at_or_later_than] just be
     computed by this function? *)
  let after_cuts =
    List.map (fun (t, use_id, interesting_vars) ->
        let level, _in_scope = cut t ~unknown_if_defined_at_or_later_than in
        t, use_id, interesting_vars, level)
      ts_and_use_ids
  in
  let level, extra_params_and_args =
    Typing_env_level.n_way_join ~initial_env_at_join:definition_typing_env
      after_cuts
  in
  let env_extension = Typing_env_extension.create level in
  env_extension, extra_params_and_args

let get_canonical_simple0 t ?min_name_mode simple : _ Or_bottom.t * _ =
  let newer_rec_info =
    let newer_rec_info = Simple.rec_info simple in
    match Simple.descr simple with
    | Const _ -> newer_rec_info
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
  let alias = alias_of_simple t simple in
  let kind = Alias.kind alias in
  let min_order_within_equiv_class =
    match min_name_mode with
    | None -> Alias.name_mode alias
    | Some name_mode -> name_mode
  in
  let result =
    try
      Aliases.get_canonical_element (aliases t) alias
        ~min_order_within_equiv_class
    with Misc.Fatal_error -> begin
      Format.eprintf "\n%sContext is:%s typing environment@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        print t;
      raise Misc.Fatal_error
    end
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
      | Const _ -> Ok (Some (simple, rec_info)), kind
      | Name name ->
        (* CR mshinwell: Do we have to return [Bottom]? *)
        let ty = find t name in
        if Type_grammar.is_obviously_bottom ty
        then Bottom, kind
        else Ok (Some (simple, rec_info)), kind

let get_canonical_simple_with_kind t ?min_name_mode simple =
  let result, kind = get_canonical_simple0 t ?min_name_mode simple in
  let result =
    Or_bottom.map result ~f:(fun result ->
      Option.map (fun (simple, _rec_info) -> simple) result)
  in
  result, kind

let get_canonical_simple t ?min_name_mode simple =
  fst (get_canonical_simple_with_kind t ?min_name_mode simple)

let get_alias_then_canonical_simple t ?min_name_mode typ : _ Or_bottom.t =
  match Type_grammar.get_alias typ with
  | None -> Ok None
  | Some simple -> get_canonical_simple t ?min_name_mode simple

let aliases_of_simple0 t ~min_name_mode simple =
  let alias = alias_of_simple t simple in
  Alias.Set.fold (fun alias result ->
      let name_mode = Alias.name_mode alias in
      match Name_mode.compare_partial_order name_mode min_name_mode with
      | None -> result
      | Some c ->
        if c >= 0 then Alias.Set_ordered_by_binding_time.add alias result
        else result)
    (Aliases.get_aliases (aliases t) alias)
    Alias.Set_ordered_by_binding_time.empty

let aliases_of_simple t ~min_name_mode simple =
  let newer_rec_info = Simple.rec_info simple in
  Alias.Set_ordered_by_binding_time.fold (fun alias simples ->
      let simple = Alias.simple alias in
      match Simple.merge_rec_info simple ~newer_rec_info with
      | None -> simples
      | Some simple -> Simple.Set.add simple simples)
    (aliases_of_simple0 t ~min_name_mode simple)
    Simple.Set.empty

let aliases_of_simple_allowable_in_types t simple =
  aliases_of_simple t ~min_name_mode:Name_mode.in_types simple

let earliest_alias_of_simple_satisfying t ~min_name_mode ~allowed_vars
      simple =
  let aliases =
    Alias.Set_ordered_by_binding_time.filter (fun alias ->
        match Simple.descr (Alias.simple alias) with
        | Name (Var var) -> Variable.Set.mem var allowed_vars
        | Name (Symbol _) | Const _ -> true)
      (aliases_of_simple0 t ~min_name_mode simple)
  in
  Option.map Alias.simple
    (Alias.Set_ordered_by_binding_time.min_elt_opt aliases)

(* CR mshinwell: This function is a performance bottleneck. *)
let create_using_resolver_and_symbol_bindings_from t =
  let original_t = t in
  let names_to_types = names_to_types t in
  let t =
    Symbol.Map.fold (fun symbol kind t ->
        add_symbol_definition t symbol kind)
      t.defined_symbols
      (create_using_resolver_from t)
  in
  Name.Map.fold
    (fun (name : Name.t) (typ, _binding_time, _name_mode) t ->
      match name with
      | Var _ -> t
      | Symbol _ ->
        let level, typ =
          Type_grammar.make_suitable_for_environment0 typ original_t
            ~suitable_for:t (Typing_env_level.empty ())
        in
        let t = add_env_extension_from_level t level in
        add_equation t name typ)
    names_to_types
    t

let rec free_names_transitive_of_type_of_name t name ~result =
  let result = Name.Set.add name result in
  let typ = find t name in
  free_names_transitive0 t typ ~result

and free_names_transitive0 t typ ~result =
  let free_names = Name_occurrences.names (Type_grammar.free_names typ) in
  let to_traverse = Name.Set.diff free_names result in
  if Name.Set.is_empty to_traverse then result
  else
    Name.Set.fold (fun name result ->
        free_names_transitive_of_type_of_name t name ~result)
      to_traverse
      result

let free_names_transitive t typ =
  free_names_transitive0 t typ ~result:Name.Set.empty

let free_variables_transitive t typ =
(*
  Format.eprintf "Computing FV transitive of %a in:@ %a"
    Type_grammar.print typ
    print t;
    *)
  let vars = Name.set_to_var_set (free_names_transitive t typ) in
  (*
  Format.eprintf "FV transitive of %a = %a\n%!"
    Type_grammar.print typ
    Variable.Set.print vars;
    *)
  vars
