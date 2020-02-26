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

  val add_or_replace_binding
     : t
    -> Name.t
    -> Type_grammar.t
    -> Binding_time.t
    -> Name_mode.t
    -> new_aliases:Aliases.t
    -> t

  val replace_variable_binding
     : t
    -> Variable.t
    -> Type_grammar.t
    -> new_aliases:Aliases.t
    -> t

  val with_cse : t -> cse:Simple.t Flambda_primitive.Eligible_for_cse.Map.t -> t
end = struct
  type t = {
    names_to_types :
      (Type_grammar.t * Binding_time.t * Name_mode.t) Name.Map.t;
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
      aliases = Aliases.empty;
      cse = Flambda_primitive.Eligible_for_cse.Map.empty;
    }

  let names_to_types t = t.names_to_types
  let aliases t = t.aliases
  let cse t = t.cse

  (* CR mshinwell: At least before the following two functions were split
     (used to be add-or-replace), the [names_to_types] map addition was a
     major source of allocation. *)

  let add_or_replace_binding t (name : Name.t) ty binding_time name_mode ~new_aliases =
    let names_to_types =
      Name.Map.add name (ty, binding_time, name_mode) t.names_to_types
    in
    { names_to_types;
      aliases = new_aliases;
      cse = t.cse;
    }

  let replace_variable_binding t var ty ~new_aliases =
    let names_to_types =
      Name.Map.replace (Name.var var)
        (function (_old_ty, binding_time, name_mode) ->
          ty, binding_time, name_mode)
        t.names_to_types
    in
    { names_to_types;
      aliases = new_aliases;
      cse = t.cse;
    }

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

(*
  let defines_name_but_no_equations t name =
    Typing_env_level.defines_name_but_no_equations t.level name
*)
end

type t = {
  resolver : (Export_id.t -> Type_grammar.t option);
  defined_symbols : Symbol.Set.t;
  code_age_relation : Code_age_relation.t;
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

(* CR mshinwell: Should print name occurrence kinds *)
(* CR mshinwell: Add option to print [aliases] *)
let print_with_cache ~cache ppf
      ({ resolver = _; prev_levels; current_level; next_binding_time = _;
         defined_symbols; code_age_relation;
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
            @[<hov 1>(code_age_relation@ %a)@]@ \
            @[<hov 1>(levels@ %a)@]\
            )@]"
        Symbol.Set.print defined_symbols
        Code_age_relation.print code_age_relation
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
    defined_symbols = Symbol.Set.empty;
    code_age_relation = Code_age_relation.empty;
  }

let create_using_resolver_from t = create ~resolver:t.resolver

let increment_scope t =
  let current_scope = current_scope t in
  let prev_levels =
    Scope.Map.add current_scope t.current_level t.prev_levels
  in
  let current_level =
    One_level.create (Scope.next current_scope) (Typing_env_level.empty ())
      ~just_after_level:(One_level.just_after_level t.current_level)
  in
  { t with
    prev_levels;
    current_level;
  }

let defined_symbols t = t.defined_symbols

let initial_symbol_type0 =
  lazy (Type_grammar.any_value ())

let initial_symbol_type =
  lazy (Type_grammar.any_value (), Binding_time.symbols, Name_mode.normal)

let find_with_binding_time_and_mode t name =
  let [@inline always] var var =
    Misc.fatal_errorf "Variable %a not bound in typing environment:@ %a"
      Variable.print var
      print t
  in
  let [@inline always] symbol sym =
    if Symbol.Set.mem sym t.defined_symbols then
      Lazy.force initial_symbol_type
    else
      Misc.fatal_errorf "Symbol %a not bound in typing environment:@ %a"
        Symbol.print sym
        print t
  in
  match Name.Map.find name (names_to_types t) with
  | exception Not_found -> Name.pattern_match name ~var ~symbol
  | result -> result

let find t name =
  let [@inline always] var var =
    Misc.fatal_errorf "Variable %a not bound in typing environment:@ %a"
      Variable.print var
      print t
  in
  let [@inline always] symbol sym =
    if Symbol.Set.mem sym t.defined_symbols then
      Lazy.force initial_symbol_type0
    else
      Misc.fatal_errorf "Symbol %a not bound in typing environment:@ %a"
        Symbol.print sym
        print t
  in
  match Name.Map.find name (names_to_types t) with
  | exception Not_found -> Name.pattern_match name ~var ~symbol
  | ty, _, _ -> ty

let find_params t params =
  List.map (fun param -> find t (Kinded_parameter.name param)) params

let mem t name =
  Name.pattern_match name
    ~var:(fun _var -> Name.Map.mem name (names_to_types t))
    ~symbol:(fun sym -> Symbol.Set.mem sym t.defined_symbols)

let mem_simple t simple =
  Simple.pattern_match simple
    ~name:(fun name -> mem t name)
    ~const:(fun _ -> true)

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
  if !Clflags.flambda_invariant_checks && mem t name then begin
    Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
      Name.print name
      print t
  end;
  let level =
    Typing_env_level.add_definition (One_level.level t.current_level) var kind
  in
  let just_after_level =
    Cached.add_or_replace_binding (cached t)
      name (Type_grammar.unknown kind)
      t.next_binding_time name_mode
      ~new_aliases:(aliases t)
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level_and_next_binding_time t ~current_level
    (Binding_time.succ t.next_binding_time)

let add_symbol_definition t sym =
  (* CR mshinwell: check for redefinition when invariants enabled? *)
  { t with
    defined_symbols = Symbol.Set.add sym t.defined_symbols;
  }

let add_symbol_definitions t syms =
  { t with
    defined_symbols = Symbol.Set.union syms t.defined_symbols;
  }

let alias_of_simple t simple =
  let [@inline always] const const =
    let binding_time = Binding_time.consts_and_discriminants in
    let mode = Name_mode.normal in
    Alias.create (Simple.const const) binding_time mode
  in
  let [@inline always] name name =
    let _ty, binding_time, mode = find_with_binding_time_and_mode t name in
    Name.pattern_match name
      ~var:(fun _var -> Alias.create simple binding_time mode)
      ~symbol:(fun sym -> Alias.create_symbol sym)
  in
  Simple.pattern_match simple ~const ~name

let alias_of_simple_with_kind t simple =
  let [@inline always] const const =
    let binding_time = Binding_time.consts_and_discriminants in
    let mode = Name_mode.normal in
    Alias.create (Simple.const const) binding_time mode,
      Type_grammar.kind (Type_grammar.type_for_const const)
  in
  let [@inline always] name name =
    let ty, binding_time, mode = find_with_binding_time_and_mode t name in
    let kind = Type_grammar.kind ty in
    Name.pattern_match name
      ~var:(fun _var -> Alias.create simple binding_time mode, kind)
      ~symbol:(fun sym -> Alias.create_symbol sym, kind)
  in
  Simple.pattern_match simple ~const ~name

let add_definition t (name : Name_in_binding_pos.t) kind =
  let name_mode = Name_in_binding_pos.name_mode name in
  Name.pattern_match (Name_in_binding_pos.name name)
    ~var:(fun var -> add_variable_definition t var kind name_mode)
    ~symbol:(fun sym ->
      if not (Name_mode.equal name_mode Name_mode.normal) then begin
        Misc.fatal_errorf "Cannot define symbol %a with name mode that \
            is not `normal'"
          Name_in_binding_pos.print name
      end;
      add_symbol_definition t sym)

let invariant_for_new_equation t name ty =
  if !Clflags.flambda_invariant_checks then begin
    (* CR mshinwell: This should check that precision is not decreasing. *)
    (* XXX [domain] needs to have [defined_symbols] unioned in *)
    let domain = Name.Map.keys (names_to_types t) in
    let defined_names =
      Name_occurrences.create_names domain Name_mode.in_types
    in
    (* CR mshinwell: It's a shame we can't check code IDs here. *)
    let free_names =
      Name_occurrences.without_code_ids (Type_grammar.free_names ty)
    in
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
  let cse = Cached.cse cached in
  match Flambda_primitive.Eligible_for_cse.Map.find prim cse with
  | exception Not_found ->
    let cse = Flambda_primitive.Eligible_for_cse.Map.add prim bound_to cse in
    let just_after_level = Cached.with_cse cached ~cse in
    let current_level =
      One_level.create (current_scope t) level ~just_after_level
    in
    with_current_level t ~current_level
  | _bound_to -> t

let rec add_equation0 t aliases name ty =
  invariant_for_new_equation t name ty;
  let level =
    Typing_env_level.add_or_replace_equation
      (One_level.level t.current_level) name ty
  in
  let just_after_level =
    Name.pattern_match name
      ~var:(fun var ->
        Cached.replace_variable_binding 
          (One_level.just_after_level t.current_level)
          var ty ~new_aliases:aliases)
      ~symbol:(fun _ ->
        Cached.add_or_replace_binding 
          (One_level.just_after_level t.current_level)
          name ty Binding_time.symbols Name_mode.normal
          ~new_aliases:aliases)
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level t ~current_level

and add_equation t name ty =
  if !Clflags.flambda_invariant_checks then begin
    let existing_ty = find t name in
    if not (K.equal (Type_grammar.kind existing_ty) (Type_grammar.kind ty))
    then begin
      Misc.fatal_errorf "Cannot add equation %a = %a@ given existing binding \
          %a = %a@ whose type is of a different kind:@ %a"
        Name.print name
        Type_grammar.print ty
        Name.print name
        Type_grammar.print existing_ty
        print t
    end
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
  end;
  if !Clflags.flambda_invariant_checks then begin
    match Type_grammar.get_alias_exn ty with
    | exception Not_found -> ()
    | simple ->
      Simple.pattern_match simple
        ~name:(fun name' ->
          if Name.equal name name' then begin
            Misc.fatal_errorf "Directly recursive equation@ %a = %a@ \
                disallowed:@ %a"
              Name.print name
              Type_grammar.print ty
              print t
          end)
        ~const:(fun _ -> ())
  end;
  let aliases, simple, rec_info, t, ty =
    let aliases = aliases t in
    match Type_grammar.get_alias_exn ty with
    | exception Not_found -> aliases, Simple.name name, None, t, ty
    | alias_of ->
      let alias =
        Name.pattern_match name
          ~var:(fun _var ->
            let _typ, binding_time, name_mode =
              find_with_binding_time_and_mode t name
            in
            Alias.create (Simple.name name) binding_time name_mode)
          ~symbol:(fun sym -> Alias.create_symbol sym)
      in
      let rec_info = Simple.rec_info alias_of in
      let alias_of = alias_of_simple t alias_of in
      let ({ canonical_element; alias_of; t = aliases; } : Aliases.add_result) =
        Aliases.add aliases alias alias_of
      in
      let kind = Type_grammar.kind ty in
      (* assert (K.equal kind (Alias.kind canonical_element)); *)
      let ty =
        Type_grammar.alias_type_of kind
          (Alias.simple canonical_element)
      in
      aliases, Alias.simple alias_of, rec_info, t, ty
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
    let [@inline always] name eqn_name =
      if Name.equal name eqn_name then ty, t
      else
        let env = Meet_env.create t in
        let existing_ty = find t eqn_name in
        match Type_grammar.meet env ty existing_ty with
        | Bottom -> Type_grammar.bottom_like ty, t
        | Ok (meet_ty, env_extension) ->
          meet_ty, add_env_extension t env_extension
    in
    Simple.pattern_match simple ~name ~const:(fun _ -> ty, t)
  in
  let ty =
    match rec_info with
    | None -> ty
    | Some rec_info ->
      match Type_grammar.apply_rec_info ty rec_info with
      | Bottom -> Type_grammar.bottom (Type_grammar.kind ty)
      | Ok ty -> ty
  in
  let [@inline always] name name = add_equation0 t aliases name ty in
  Simple.pattern_match simple ~name ~const:(fun _ -> t)

and add_env_extension_from_level t level : t =
  let t =
    Variable.Map.fold (fun var kind t ->
        add_variable_definition t var kind Name_mode.in_types)
      (Typing_env_level.defined_vars level)
      t
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

and add_env_extension t env_extension =
  let [@inline always] f level =
    (add_env_extension_from_level [@inlined never]) t level
  in
  Typing_env_extension.pattern_match env_extension ~f

(* These version is outside the [let rec] and thus does not cause
   [caml_apply*] to be used when calling from outside this module. *)
let add_equation t name ty = add_equation t name ty

let add_env_extension t env_extension = add_env_extension t env_extension

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
  if !Clflags.flambda_invariant_checks
    && List.compare_lengths params param_types <> 0
  then begin
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

let add_to_code_age_relation t ~newer ~older =
  let code_age_relation =
    Code_age_relation.add t.code_age_relation ~newer ~older
  in
  { t with code_age_relation; }

let code_age_relation t = t.code_age_relation

let with_code_age_relation t code_age_relation =
  { t with code_age_relation; }

(* CR mshinwell: Change the name of the labelled argument *)
let cut t ~unknown_if_defined_at_or_later_than:min_scope =
  let current_scope = current_scope t in
  if Scope.(>) min_scope current_scope then
    Typing_env_level.empty ()
  else
    let _strictly_less, at_min_scope, strictly_greater =
      Scope.Map.split min_scope t.prev_levels
    in
    let at_or_after_cut =
      match at_min_scope with
      | None -> strictly_greater
      | Some typing_env_level ->
        Scope.Map.add min_scope typing_env_level strictly_greater
    in
    let at_or_after_cut =
      Scope.Map.add current_scope t.current_level at_or_after_cut
    in
    Scope.Map.fold (fun _scope one_level result ->
        Typing_env_level.concat result (One_level.level one_level))
      at_or_after_cut
      (Typing_env_level.empty ())

let cut_and_n_way_join definition_typing_env ts_and_use_ids ~params
      ~unknown_if_defined_at_or_later_than =
  (* CR mshinwell: Can't [unknown_if_defined_at_or_later_than] just be
     computed by this function? *)
  let after_cuts =
    List.map (fun (t, use_id, use_kind) ->
        let level = cut t ~unknown_if_defined_at_or_later_than in
        t, use_id, use_kind, level)
      ts_and_use_ids
  in
  let level, extra_params_and_args =
    Typing_env_level.n_way_join ~env_at_fork:definition_typing_env
      after_cuts ~params
  in
  let env_extension = Typing_env_extension.create level in
  env_extension, extra_params_and_args

let get_canonical_simple_with_kind_exn t ?min_name_mode simple =
  let alias, kind = alias_of_simple_with_kind t simple in
  let newer_rec_info =
    let newer_rec_info = Simple.rec_info simple in
    match newer_rec_info with
    | None -> None
    | Some newer_rec_info ->
      Simple.pattern_match simple
        ~const:(fun _ -> Some newer_rec_info)
        ~name:(fun name ->
          match Type_grammar.get_alias_exn (find t name) with
          | exception Not_found -> Some newer_rec_info
          | simple ->
            match Simple.rec_info simple with
            | None -> Some newer_rec_info
            | Some rec_info ->
              Some (Rec_info.merge rec_info ~newer:newer_rec_info))
  in
  let min_order_within_equiv_class =
    match min_name_mode with
    | None -> Alias.name_mode alias
    | Some name_mode -> name_mode
  in
  match
    Aliases.get_canonical_element_exn (aliases t) alias
      ~min_order_within_equiv_class
  with
  | exception Misc.Fatal_error ->
    if !Clflags.flambda2_context_on_error then begin
      Format.eprintf "\n%sContext is:%s typing environment@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        print t
    end;
    raise Misc.Fatal_error
  | alias ->
    let simple = Alias.simple alias in
    match newer_rec_info with
    | None -> simple, kind
    | Some _ ->
      match Simple.merge_rec_info simple ~newer_rec_info with
      | None -> raise Not_found
      | Some simple -> simple, kind

let get_canonical_simple_exn t ?min_name_mode simple =
  (* Duplicated from above to eliminate the allocation of the returned pair. *)
  let alias = alias_of_simple t simple in
  let newer_rec_info =
    let newer_rec_info = Simple.rec_info simple in
    match newer_rec_info with
    | None -> None
    | Some newer_rec_info ->
      Simple.pattern_match simple
        ~const:(fun _ -> Some newer_rec_info)
        ~name:(fun name ->
          match Type_grammar.get_alias_exn (find t name) with
          | exception Not_found -> Some newer_rec_info
          | simple ->
            match Simple.rec_info simple with
            | None -> Some newer_rec_info
            | Some rec_info ->
              Some (Rec_info.merge rec_info ~newer:newer_rec_info))
  in
  let min_order_within_equiv_class =
    match min_name_mode with
    | None -> Alias.name_mode alias
    | Some name_mode -> name_mode
  in
  match
    Aliases.get_canonical_element_exn (aliases t) alias
      ~min_order_within_equiv_class
  with
  | exception Misc.Fatal_error ->
    if !Clflags.flambda2_context_on_error then begin
      Format.eprintf "\n%sContext is:%s typing environment@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        print t
    end;
    raise Misc.Fatal_error
  | alias ->
    let simple = Alias.simple alias in
    match newer_rec_info with
    | None -> simple
    | Some _ ->
      match Simple.merge_rec_info simple ~newer_rec_info with
      | None -> raise Not_found
      | Some simple -> simple

let get_alias_then_canonical_simple_exn t ?min_name_mode typ =
  Type_grammar.get_alias_exn typ
  |> get_canonical_simple_exn t ?min_name_mode

let aliases_of_simple t ~min_name_mode simple =
  let aliases =
    alias_of_simple t simple
    |> Aliases.get_aliases (aliases t)
    |> Alias.Set.filter (fun alias ->
      let name_mode = Alias.name_mode alias in
      match Name_mode.compare_partial_order name_mode min_name_mode with
      | None -> false
      | Some c -> c >= 0)
    |> Alias.set_to_simple_set
  in
  let newer_rec_info = Simple.rec_info simple in
  match newer_rec_info with
  | None -> aliases
  | Some _ ->
    Simple.Set.fold (fun simple simples ->
        match Simple.merge_rec_info simple ~newer_rec_info with
        | None -> simples
        | Some simple -> Simple.Set.add simple simples)
      aliases
      Simple.Set.empty

let aliases_of_simple_allowable_in_types t simple =
  aliases_of_simple t ~min_name_mode:Name_mode.in_types simple

let create_using_resolver_and_symbol_bindings_from t =
  let original_t = t in
  let names_to_types = names_to_types t in
  let t =
    { (create_using_resolver_from t) with
      defined_symbols = original_t.defined_symbols;
    }
  in
  Symbol.Set.fold (fun sym t ->
      let name = Name.symbol sym in
      match Name.Map.find name names_to_types with
      | exception Not_found -> t
      | typ, _binding_time, _name_mode ->
        let level, typ =
          Type_grammar.make_suitable_for_environment0 typ original_t
            ~suitable_for:t (Typing_env_level.empty ())
        in
        let t = add_env_extension_from_level t level in
        add_equation t name typ)
    original_t.defined_symbols
    t

let rec free_names_transitive_of_type_of_name t name ~result =
  let result = Name_occurrences.add_name result name Name_mode.in_types in
  let typ = find t name in
  free_names_transitive0 t typ ~result

and free_names_transitive0 t typ ~result =
  let free_names = Type_grammar.free_names typ in
  let to_traverse = Name_occurrences.diff free_names result in
  if Name_occurrences.is_empty to_traverse then result
  else
    Name_occurrences.fold_names to_traverse
      ~init:result
      ~f:(fun result name ->
        free_names_transitive_of_type_of_name t name ~result)

let free_names_transitive t typ =
  free_names_transitive0 t typ ~result:Name_occurrences.empty