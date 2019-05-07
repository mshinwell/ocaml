
type t = {
  (* When used for [Typing_env_extension], the [defined_names] are those
     that are existentially bound. *)
  defined_names : Flambda_kind.t Name.Map.t;
  equations : Flambda_types.t Name.Map.t;
  cse : Simple.t Flambda_primitive.With_fixed_value.Map.t;
}

let print_with_cache ~cache ppf
      ({ defined_names; equations; cse; } : t) =
  let print_equations ppf equations =
    let equations = Name.Map.bindings equations in
    match equations with
    | [] -> Format.pp_print_string ppf "()"
    | _::_ ->
      Format.pp_print_string ppf "(";
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (name, ty) ->
          Format.fprintf ppf
            "@[<hov 1>%s%a%s :@ %a@]"
            (Misc_color.bold_green ())
            Name.print name
            (Misc_color.reset ())
            (Type_printers.print_with_cache ~cache) ty)
        ppf equations;
      Format.pp_print_string ppf ")"
  in
  if Name.Map.is_empty defined_names then
    Format.fprintf ppf
      "@[<v 1>(\
          @[<hov 1>(equations@ @[<v 1>%a@])@]@;\
          @[<hov 1>(cse@ %a)@])@]"
      print_equations equations
      (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
  else
    Format.fprintf ppf
      "@[<v 1>(\
          @[<hov 1>(defined_names@ @[<v 1>%a@])@]@;\
          @[<hov 1>(equations@ @[<v 1>%a@])@]@;\
          @[<hov 1>(cse@ %a)@])@]"
      (* CR mshinwell: Fix this.  The problem is that Logical_variable prints
         the types *)
      Name.Set.print (Name.Map.keys defined_names)
(*
      (Name.Map.print Flambda_kind.print) defined_names
*)
      print_equations equations
      (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant _t = ()

let empty () =
  { defined_names = Name.Map.empty;
    equations = Name.Map.empty;
    cse = Flambda_primitive.With_fixed_value.Map.empty;
  }

let is_empty { defined_names; equations; cse; } =
  Name.Map.is_empty defined_names
    && Name.Map.is_empty equations
    && Flambda_primitive.With_fixed_value.Map.is_empty cse

let equations t = t.equations

let equations_domain t = Name.Map.keys t.equations

let equations_on_outer_env_domain t =
  Name.Set.diff (equations_domain t) (Name.Map.keys t.defined_names)

let cse t = t.cse

let equal env result t1 t2 =
  let (>>=) = Type_equality_result.(>>=) in
  let env_left =
    Typing_env.add_opened_env_extension
      (Type_equality_env.typing_env_left env)
      t1
  in
  let env_right =
    Typing_env.add_opened_env_extension
      (Type_equality_env.typing_env_right env)
      t2
  in
  let env =
    Type_equality_env.replace_typing_environments env
      ~left:env_left ~right:env_right
  in
  let names_to_check =
    Name.Set.union (equations_on_outer_env_domain t1)
      (equations_on_outer_env_domain t2)
  in
  result
  >>= fun result ->
  Name.Set.fold (fun name result ->
      result
      >>= fun result ->
      let ty1, _ = Typing_env.find_exn env_left name in
      let ty2, _ = Typing_env.find_exn env_right name in
      Type_equality.equal_with_env ~bound_name:name env result ty1 ty2)
    names_to_check
    result
  >>= fun result ->
  if Flambda_primitive.With_fixed_value.Map.equal Simple.equal t1.cse t2.cse
  then result
  else Type_equality_result.types_known_unequal ()

let apply_name_permutation ({ defined_names; equations; cse; } as t)
      perm =
  let defined_names_changed = ref false in
  let defined_names' =
    Name.Map.fold (fun name kind defined_names ->
        let name' = Name_permutation.apply_name perm name in
        if not (name == name') then begin
          defined_names_changed := true
        end;
        Name.Map.add name' kind defined_names)
      defined_names
      Name.Map.empty
  in
  let equations_changed = ref false in
  let equations' =
    Name.Map.fold (fun name ty equations ->
        let name' = Name_permutation.apply_name perm name in
        let ty' =
          Flambda_type0_core.apply_name_permutation ty perm
        in
        if not (name == name' && ty == ty') then begin
          equations_changed := true
        end;
        Name.Map.add name' ty' equations)
      equations
      Name.Map.empty
  in
  let cse_changed = ref false in
  let cse' =
    Flambda_primitive.With_fixed_value.Map.fold (fun prim simple cse' ->
        let simple' = Simple.apply_name_permutation simple perm in
        let prim' =
          Flambda_primitive.With_fixed_value.apply_name_permutation prim
            perm
        in
        if (not (simple == simple')) || (not (prim == prim')) then begin
          cse_changed := true
        end;
        Flambda_primitive.With_fixed_value.Map.add prim' simple' cse')
      cse
      Flambda_primitive.With_fixed_value.Map.empty
  in
  if (not !defined_names_changed)
    && (not !equations_changed)
    && (not !cse_changed)
  then t
  else 
    { defined_names = defined_names';
      equations = equations';
      cse = cse';
    }

let free_names_in_defined_names t =
  Name_occurrences.create_from_name_set_in_types
    (Name.Map.keys t.defined_names)

let free_names_in_equations_and_cse
      { defined_names = _; equations; cse; } =
  let free_names_equations =
    Name.Map.fold (fun name ty free_names ->
        let free_names' = 
          Name_occurrences.add (Type_free_names.free_names ty)
            (Bindable_name.Name name) In_types
        in
        Name_occurrences.union free_names free_names')
      equations
      (Name_occurrences.create ())
  in
  Flambda_primitive.With_fixed_value.Map.fold
    (fun prim (simple : Simple.t) acc ->
      match simple with
      | Const _ | Discriminant _ -> acc
      | Name name ->
        Name_occurrences.add
          (Flambda_primitive.With_fixed_value.free_names prim)
          (Bindable_name.Name name) In_types)
    cse
    free_names_equations

let free_names t =
  Name_occurrences.union (free_names_in_defined_names t)
    (free_names_in_equations_and_cse t)

let free_names_minus_defined_names t =
  Name_occurrences.diff (free_names_in_equations_and_cse t)
    (free_names_in_defined_names t)

let restrict_to_names { defined_names; equations; cse; } allowed_names =
  let allowed_names =
    Name_occurrences.everything_must_only_be_names allowed_names
  in
  let defined_names =
    Name.Map.filter (fun name _kind -> Name.Set.mem name allowed_names)
      defined_names
  in
  let equations =
    Name.Map.filter (fun name _ty -> Name.Set.mem name allowed_names)
      equations
  in
  let cse =
    Flambda_primitive.With_fixed_value.Map.filter
      (fun _prim (simple : Simple.t) ->
        match simple with
        | Name name -> Name.Set.mem name allowed_names
        | Const _ | Discriminant _ -> true)
      cse
  in
  let t =
    { defined_names;
      equations;
      cse;
    }
  in
  invariant t;
  t

let find_equation_opt t name =
  match Name.Map.find name t.equations with
  | exception Not_found -> None
  | ty -> Some ty

let find_opt t name =
  match find_equation_opt t name with
  | None ->
    begin match Name.Map.find name t.defined_names with
    | exception Not_found -> None
    | kind -> Some (Flambda_type0_core.unknown kind)
    end
  | some_ty -> some_ty

let find_exn t name =
  match find_opt t name with
  | Some ty -> ty
  | None ->
    Misc.fatal_errorf "Unbound name %a in@ %a"
      Name.print name
      print t

let tidy t =
  let free_names_minus_defined_names' =
    free_names_minus_defined_names t
  in
  let defined_names = free_names_in_defined_names t in
  let equations =
    Name.Map.map_sharing (fun ty ->
        let rec resolve_aliases ty =
          (* CR mshinwell: Needs check for cycles *)
          match Flambda_type0_core.get_alias ty with
          | None
          | Some (Const _ | Discriminant _) -> ty
          | Some (Name alias) ->
            let alias_is_defined_name =
              Name_occurrences.mem defined_names (Name alias)
            in
            let alias_is_not_used_on_rhs =
              Name_occurrences.mem free_names_minus_defined_names'
                (Name alias)
            in
            if alias_is_defined_name && alias_is_not_used_on_rhs
            then resolve_aliases (find_exn t alias)
            else ty
        in
        resolve_aliases ty)
      t.equations
  in
  let t =
    { t with equations; }
  in
  (* CR mshinwell: We can probably avoid re-computing this by calculating
     it as we go along, just above. *)
  let free_names_minus_defined_names =
    free_names_minus_defined_names t
  in
  let unused_defined_names =
    Name_occurrences.diff defined_names free_names_minus_defined_names
  in
  let allowed =
    Name_occurrences.diff free_names_minus_defined_names
      unused_defined_names
  in
  restrict_to_names t allowed

let add_definition t name kind =
  if Name.Map.mem name t.defined_names then begin
    Misc.fatal_errorf "Typing environment extension already binds \
        name %a:@ %a"
      Name.print name
      print t
  end;
  let t =
    { t with
      defined_names = Name.Map.add name kind t.defined_names
    }
  in
  tidy t

let add_equation t name ty =
  if Name.Map.mem name t.equations then begin
    Misc.fatal_errorf "Equation on %a already exists in@ %a"
      Name.print name
      print t
  end;
  (* CR mshinwell: Must be much more careful about when [tidy] is called *)
  let t =
    { t with
      equations = Name.Map.add name ty t.equations;
    }
  in
  tidy t

let add_or_replace_equation t name ty =
  { t with
    equations = Name.Map.add name ty t.equations;
  }

let add_cse t name prim =
  let cse =
    match Flambda_primitive.With_fixed_value.Map.find prim t.cse with
    | exception Not_found ->
      Flambda_primitive.With_fixed_value.Map.add prim name t.cse
    | _name -> t.cse
  in
  { t with cse; }

type cse_meet_or_join = Meet | Join
let _ : cse_meet_or_join = Meet

let update_cse_for_meet_or_join t _t1 _t2 (_meet_or_join : cse_meet_or_join)
      _names =
  t

(* XXX Uncomment once the rest is working again
  (* XXX This should follow aliases to the canonical name. *)
  let preserved_cse_equations t =
    (* CR-someday mshinwell: This could be improved to preserve some of
       those CSE equations that talk about existentially-bound names. *)
    Flambda_primitive.With_fixed_value.Map.filter
      (fun prim (bound_to_or_value : Simple.t) ->
        match bound_to_or_value with
        | Name name when not (Name.Set.mem name names) ->
          false
        | Name _ | Const _ | Discriminant _ ->
          let free_names_prim =
            Name_occurrences.everything_must_only_be_names
              (Flambda_primitive.With_fixed_value.free_names prim)
          in
          Name.Set.subset free_names_prim names)
      t.cse
  in
  let cse =
    (* XXX This should be intersection for join and union for meet *)
    Flambda_primitive.With_fixed_value.Map.merge
      (fun _prim
          (simple1 : Simple.t option) (simple2 : Simple.t option) ->
        match simple1, simple2 with
        | None, None -> None
        | Some _, None -> simple1
        | None, Some _ -> simple2
        | Some simple1, Some simple2 ->
          (* For the moment just keep this very straightforward. *)
          (* CR-soon mshinwell: Make this take account of alias sets
             properly. *)
          if Simple.equal simple1 simple2 then Some simple1
          else None)
      (preserved_cse_equations t1)
      (preserved_cse_equations t2)
  in
  let t =
    { t with
      cse;
    }
  in
  invariant t;
  t
*)

let meet env (t1 : t) (t2 : t) : t =
  (* Care: as per comment in [Typing_env_extension.meet]. *)
  if is_empty t1 then begin
    t2
  end else if is_empty t2 then begin
    t1
  end else begin
Format.eprintf "Typing_env_level.meet@ %a@ and@ %a@ in env@ %a\n%!" print t1 print t2
Meet_env.print env;
    let t1 = apply_name_permutation t1 (Meet_env.perm_left env) in
    let t2 = apply_name_permutation t2 (Meet_env.perm_right env) in
    let env = Meet_env.env env in
    let env = Typing_env.increment_scope_level env in
    (* The domains of the levels are treated as contravariant.  As such,
       since this is [meet], we perform a union on the domains. *)
    let env = Typing_env.add_opened_env_extension env t1 in
    let env = Typing_env.add_opened_env_extension env t2 in
    let level = Typing_env.current_level env in
    let t =
      Typing_env.cut0 env ~existential_if_defined_at_or_later_than:level
    in
    tidy t
  end

let join env (t1 : t) (t2 : t) : t =
Format.eprintf "Typing_env_level.join@ %a@ and@ %a@ in env@ %a\n%!" print t1 print t2
Join_env.print env;
  let names_with_equations_in_join =
    (* Analogously to above, since this is [join], we perform an
       intersection on the domains of the levels. *)
    Name.Set.inter (equations_on_outer_env_domain t1)
      (equations_on_outer_env_domain t2)
  in
  let defined_names =
    Name.Map.disjoint_union t1.defined_names t2.defined_names
  in
  let t =
    { (empty ()) with
      defined_names;
    }
  in
  let env =
    Join_env.add_opened_extensions env
      ~holds_on_left:t1 ~holds_on_right:t2
  in
  let t =
    Name.Set.fold (fun name t ->
        assert (not (Name.Map.mem name t.equations));
        let ty1 = find_exn t1 name in
        let ty2 = find_exn t2 name in
        let join_ty =
          Both_meet_and_join.join ~bound_name:name env ty1 ty2
        in
        add_or_replace_equation t name join_ty)
      names_with_equations_in_join
      t
  in
  let t =
    update_cse_for_meet_or_join t t1 t2 Join names_with_equations_in_join
  in
  tidy t

let meet_equation t env name ty =
  let t' =
    { (empty ()) with
      equations = Name.Map.singleton name ty;
    }
  in
  let env =
    Meet_env.create env
      ~perm_left:(Name_permutation.create ())
      ~perm_right:(Name_permutation.create ())
  in
  meet env t t'

let defined_names t = t.defined_names

let defined_names_set t =
  Name.Set.fold (fun name defined_names ->
      Bindable_name.Set.add (Name name) defined_names)
    (Name.Map.keys t.defined_names)
    Bindable_name.Set.empty

let defined_names_in_order t =
  Bindable_name.Set.elements (defined_names_set t)
