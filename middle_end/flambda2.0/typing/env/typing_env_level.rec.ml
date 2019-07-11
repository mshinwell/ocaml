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
  (* CR mshinwell: We should have [Flambda_primitive.Eligible_for_cse]
     instead.  Things that are genuine projections shouldn't be CSEd. *)
  cse : Simple.t Flambda_primitive.With_fixed_value.Map.t;
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
      (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
  else
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(defined_vars@ @[<hov 1>%a@])@]@ \
        @[<hov 1>(equations@ @[<hov 1>%a@])@]@ \
        @[<hov 1>(cse@ @[<hov 1>%a@])@]\
        )@]"
      Variable.Set.print (Variable.Map.keys defined_vars)
      print_equations equations
      (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant _t = ()

let empty () =
  { defined_vars = Variable.Map.empty;
    equations = Name.Map.empty;
    cse = Flambda_primitive.With_fixed_value.Map.empty;
  }

let is_empty { defined_vars; equations; cse; } =
  Variable.Map.is_empty defined_vars
    && Name.Map.is_empty equations
    && Flambda_primitive.With_fixed_value.Map.is_empty cse

let equations t = t.equations

let defined_vars t = t.defined_vars

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
    cse = Flambda_primitive.With_fixed_value.Map.empty;
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
  match Flambda_primitive.With_fixed_value.Map.find prim t.cse with
  | exception Not_found ->
    let cse =
      Flambda_primitive.With_fixed_value.Map.add prim bound_to t.cse
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
    Typing_env_extension.to_level env_extension
  end

let join env (t1 : t) (t2 : t) : t =
  (* This restriction will be relaxed in the full type system. *)
  if not (Variable.Map.is_empty t1.defined_vars
           && Variable.Map.is_empty t2.defined_vars)
  then begin
    Misc.fatal_errorf "Cannot join environment levels that define variables:@ \
        %a@ and@ %a"
      print t1
      print t2
  end;
  let names_with_equations_in_join =
    Name.Set.inter (Name.Map.keys t1.equations) (Name.Map.keys t2.equations)
  in
  let t =
    Name.Set.fold (fun name t ->
        assert (not (Name.Map.mem name t.equations));
        let ty1 = find_equation t1 name in
        let ty2 = find_equation t2 name in
        let join_ty = Api_meet_and_join.join ~bound_name:name env ty1 ty2 in
        add_or_replace_equation t name join_ty)
      names_with_equations_in_join
      (empty ())
  in
  let cse =
    Flambda_primitive.With_fixed_value.Map.merge
      (fun _prim bound_to1 bound_to2 ->
        match bound_to1, bound_to2 with
        | None, None | Some _, None | None, Some _ -> None
        | Some bound_to1, Some bound_to2 ->
          if Simple.equal bound_to1 bound_to2 then Some bound_to1
          else None)
      t1.cse t2.cse
  in
  { t with cse; }

let filter_cse cse ~allowed =
  Flambda_primitive.With_fixed_value.Map.filter_map cse
    ~f:(fun prim bound_to ->
      match Simple.descr bound_to with
      | Name (Var var) ->
        if Variable.Set.mem var allowed then Some bound_to
        else None
      | _ ->
        let free_vars =
          Name_occurrences.variables
            (Flambda_primitive.With_fixed_value.free_names prim)
        in
        if Variable.Set.subset free_vars allowed then Some bound_to
        else None)

let erase_aliases env ~allowed t =
  let equations =
    Name.Map.mapi (fun name ty ->
        let bound_name = Some name in
        Type_erase_aliases.erase_aliases env ~bound_name
          ~already_seen:Simple.Set.empty ~allowed ty)
      t.equations
  in
  let cse = filter_cse t.cse ~allowed in
  { t with
    equations;
    cse;
  }

let remove_definitions_and_equations t ~allowed =
  if not (Variable.Set.is_empty
    (Variable.Set.inter allowed (Variable.Map.keys t.defined_vars)))
  then begin
    Misc.fatal_error "[allowed] set must not include any [defined_vars]"
  end;
  let equations =
    Name.Map.filter (fun (name : Name.t) _ty ->
        match name with
        | Var var -> Variable.Set.mem var allowed
        | Symbol _ -> true)
      t.equations
  in
  let cse = filter_cse t.cse ~allowed in
  { defined_vars = Variable.Map.empty;
    equations;
    cse;
  }

let mem t name =
  Name.Map.mem name t.equations

let apply_name_permutation ({ defined_vars; equations; cse; } as t)
      perm =
  let defined_vars_changed = ref false in
  let defined_vars' =
    Name.Map.fold (fun var kind defined_vars ->
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
    Flambda_primitive.With_fixed_value.Map.fold (fun prim simple cse' ->
        let simple' = Simple.apply_name_permutation simple perm in
        let prim' =
          Flambda_primitive.With_fixed_value.apply_name_permutation prim perm
        in
        if (not (simple == simple')) || (not (prim == prim')) then begin
          cse_changed := true
        end;
        Flambda_primitive.With_fixed_value.Map.add prim' simple' cse')
      cse
      Flambda_primitive.With_fixed_value.Map.empty
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
