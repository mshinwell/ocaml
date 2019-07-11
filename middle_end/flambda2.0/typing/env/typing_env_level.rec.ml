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

type t = {
  defined_vars : Flambda_kind.t Variable.Map.t;
  equations : Flambda_types.t Name.Map.t;
  cse : Simple.t Flambda_primitive.Eligible_for_cse.Map.t;
}

let print_with_cache ~cache ppf ({ defined_vars; equations; cse; } : t) =
  let print_equations ppf equations =
    let equations = Name.Map.bindings equations in
    match equations with
    | [] -> Format.pp_print_string ppf "()"
    | _::_ ->
      Format.pp_print_string ppf "(";
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (name, ty) ->
          Format.fprintf ppf
            "@[<hov 1>%a@ :@ %a@]"
            Name.print name
            (Type_printers.print_with_cache ~cache) ty)
        ppf equations;
      Format.pp_print_string ppf ")"
  in
  if Variable.Map.is_empty defined_vars then
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(equations@ @[<hov 1>%a@])@])@ \
        @[<hov 1>(cse@ @[<hov 1>%a@])@]\
        @]"
      print_equations equations
      (Flambda_primitive.Eligible_for_cse.Map.print Simple.print) cse
  else
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(defined_vars@ @[<hov 1>%a@])@]@ \
        @[<hov 1>(equations@ @[<hov 1>%a@])@]@ \
        @[<hov 1>(cse@ @[<hov 1>%a@])@]\
        )@]"
      Variable.Set.print (Variable.Map.keys defined_vars)
      print_equations equations
      (Flambda_primitive.Eligible_for_cse.Map.print Simple.print) cse

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant _t = ()

let apply_name_permutation ({ defined_vars; equations; cse; } as t)
      perm =
  let defined_vars_changed = ref false in
  let defined_vars' =
    Variable.Map.fold (fun var kind defined_vars ->
        let var' = Name_permutation.apply_variable perm var in
        if not (var == var') then begin
          defined_vars_changed := true
        end;
        Variable.Map.add var' kind defined_vars)
      defined_vars
      Variable.Map.empty
  in
  let equations_changed = ref false in
  let equations' =
    Name.Map.fold (fun name typ equations ->
        let name' = Name_permutation.apply_name perm name in
        let typ' = Flambda_type0_core.apply_name_permutation typ perm in
        if not (name == name' && typ == typ') then begin
          equations_changed := true
        end;
        Name.Map.add name' typ' equations)
      equations
      Name.Map.empty
  in
  let cse_changed = ref false in
  let cse' =
    Flambda_primitive.Eligible_for_cse.Map.fold (fun prim simple cse' ->
        let simple' = Simple.apply_name_permutation simple perm in
        let prim' =
          Flambda_primitive.Eligible_for_cse.apply_name_permutation prim perm
        in
        if (not (simple == simple')) || (not (prim == prim')) then begin
          cse_changed := true
        end;
        Flambda_primitive.Eligible_for_cse.Map.add prim' simple' cse')
      cse
      Flambda_primitive.Eligible_for_cse.Map.empty
  in
  if (not !defined_vars_changed)
    && (not !equations_changed)
    && (not !cse_changed)
  then t
  else 
    { defined_vars = defined_vars';
      equations = equations';
      cse = cse';
    }

let free_names { defined_vars; equations; cse; } =
  let free_names_defined_vars =
    Name_occurrences.create_variables (Variable.Map.keys defined_vars)
      Name_occurrence_kind.in_types
  in
  let free_names_equations =
    Name.Map.fold (fun name typ free_names ->
        let free_names' = 
          Name_occurrences.add_name (Type_free_names.free_names typ)
            name Name_occurrence_kind.in_types
        in
        Name_occurrences.union free_names free_names')
      equations
      free_names_defined_vars
  in
  Flambda_primitive.Eligible_for_cse.Map.fold
    (fun prim (bound_to : Simple.t) acc ->
      match Simple.descr bound_to with
      | Const _ | Discriminant _ -> acc
      | Name name ->
        let free_in_prim =
          Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
            (Flambda_primitive.Eligible_for_cse.free_names prim)
            Name_occurrence_kind.in_types
        in
        Name_occurrences.add_name free_in_prim
          name Name_occurrence_kind.in_types)
    cse
    free_names_equations

let empty () =
  { defined_vars = Variable.Map.empty;
    equations = Name.Map.empty;
    cse = Flambda_primitive.Eligible_for_cse.Map.empty;
  }

let is_empty { defined_vars; equations; cse; } =
  Variable.Map.is_empty defined_vars
    && Name.Map.is_empty equations
    && Flambda_primitive.Eligible_for_cse.Map.is_empty cse

let equations t = t.equations

let defined_vars t = t.defined_vars

let defined_vars_in_order t =
  Variable.Set.elements (Variable.Map.keys t.defined_vars)

let cse t = t.cse

let add_definition t var kind =
  if Variable.Map.mem var t.defined_vars then begin
    Misc.fatal_errorf "Environment extension already binds variable %a:@ %a"
      Variable.print var
      print t
  end;
  { t with
    defined_vars = Variable.Map.add var kind t.defined_vars
  }

let check_equation t name ty =
  match Flambda_type0_core.get_alias ty with
  | None -> ()
  | Some simple ->
    match Simple.descr simple with
    | Name name' ->
      if Name.equal name name' then begin
        Misc.fatal_errorf "Directly recursive equation@ %a = %a@ \
            disallowed (Typing_env_level):@ %a"
          Name.print name
          Type_printers.print ty
          print t
      end
    | _ -> ()

let one_equation name ty =
  check_equation (empty ()) name ty;
  { defined_vars = Variable.Map.empty;
    equations = Name.Map.singleton name ty;
    cse = Flambda_primitive.Eligible_for_cse.Map.empty;
  }

let add_or_replace_equation t name ty =
  check_equation t name ty;
  { t with
    equations = Name.Map.add name ty t.equations;
  }

let find_equation t name =
  match Name.Map.find name t.equations with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a not bound in typing environment level:@ %a"
      Name.print name
      print t
  | ty -> ty

let add_cse t prim ~bound_to =
  match Flambda_primitive.Eligible_for_cse.Map.find prim t.cse with
  | exception Not_found ->
    let cse =
      Flambda_primitive.Eligible_for_cse.Map.add prim bound_to t.cse
    in
    { t with cse; }
  | _bound_to -> t

(* XXX Not sure this is correct yet *)
let meet env (t1 : t) (t2 : t) =
  (* Care: the domains of [t1] and [t2] are treated as contravariant.
     As such, since this is [meet], we perform unions on the domains.
     So if one of them is bottom, the result of meeting it with any other
     level is that level, not bottom. *)
  if is_empty t1 then begin
    t2
  end else if is_empty t2 then begin
    t1
  end else begin
    (* CR mshinwell: There may be a more efficient way of doing this.
       Are we wasting time doing meets again when we've already got the
       best types? *)
    let env = Meet_env.env env in
    let env = Typing_env.increment_scope env in
    let level = Typing_env.current_scope env in
    let t1 = Typing_env_extension.create t1 in
    let t2 = Typing_env_extension.create t2 in
    let env = Typing_env.add_env_extension env t1 in
    let env = Typing_env.add_env_extension env t2 in
    let env_extension, _names_in_scope_at_cut =
      Typing_env.cut env ~unknown_if_defined_at_or_later_than:level
    in
    (* XXX This seems dubious as we will freshen defined names each time *)
    Typing_env_extension.pattern_match env_extension ~f:(fun level -> level)
  end

let join env (t1 : t) (t2 : t) : t =
  let t1_defined = Name.set_of_var_set (Variable.Map.keys t1.defined_vars) in
  let t2_defined = Name.set_of_var_set (Variable.Map.keys t2.defined_vars) in
  let all_defined = Name.Set.union t1_defined t2_defined in
  let names_with_equations_in_join =
    Name.Set.diff
      (Name.Set.inter (Name.Map.keys t1.equations) (Name.Map.keys t2.equations))
      all_defined
  in
  let allowed = Typing_env.var_domain env in
  assert (Variable.Set.subset
    (Name.set_to_var_set names_with_equations_in_join) allowed);
  let get_type t name =
    Type_erase_aliases.erase_aliases env ~bound_name:(Some name)
      ~already_seen:Simple.Set.empty ~allowed
      (find_equation t name)
  in
  let t =
    Name.Set.fold (fun name t ->
        assert (not (Name.Map.mem name t.equations));
        let ty1 = get_type t1 name in
        let ty2 = get_type t2 name in
        let join_ty = Api_meet_and_join.join ~bound_name:name env ty1 ty2 in
        add_or_replace_equation t name join_ty)
      names_with_equations_in_join
      (empty ())
  in
  let cse_prim_allowed prim =
    Variable.Set.subset
      (Name_occurrences.variables (
        Flambda_primitive.Eligible_for_cse.free_names prim))
      allowed
  in
  let simple_allowed simple =
    match Simple.descr simple with
    | Name (Var var) -> Variable.Set.mem var allowed
    | Name (Symbol _) | Const _ | Discriminant _ -> true
  in
  let cse =
    Flambda_primitive.Eligible_for_cse.Map.merge
      (fun prim bound_to1 bound_to2 ->
        match bound_to1, bound_to2 with
        | None, None | Some _, None | None, Some _ -> None
        | Some bound_to1, Some bound_to2 ->
          if Simple.equal bound_to1 bound_to2
            && simple_allowed bound_to1
            && cse_prim_allowed prim
          then Some bound_to1
          else None)
      t1.cse t2.cse
  in
  assert (Variable.Map.is_empty t.defined_vars);
  { t with cse; }

let mem t name =
  Name.Map.mem name t.equations
