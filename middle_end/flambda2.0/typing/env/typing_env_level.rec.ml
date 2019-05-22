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
  defined_names : Flambda_kind.t Name.Map.t;
  equations : Flambda_types.t Name.Map.t;
}

let print_with_cache ~cache ppf
      ({ defined_names; equations; } : t) =
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
            (Misc.Color.bold_green ())
            Name.print name
            (Misc.Color.reset ())
            (Type_printers.print_with_cache ~cache) ty)
        ppf equations;
      Format.pp_print_string ppf ")"
  in
  if Name.Map.is_empty defined_names then
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(equations@ @[<v 1>%a@])@])\
        @]"
      print_equations equations
  else
    Format.fprintf ppf
      "@[<v 1>(\
          @[<hov 1>(defined_names@ @[<v 1>%a@])@]@;\
          @[<hov 1>(equations@ @[<v 1>%a@])@]\
          )@]"
      (* CR mshinwell: Fix this.  The problem is that Logical_variable prints
         the types *)
      Name.Set.print (Name.Map.keys defined_names)
(*
      (Name.Map.print Flambda_kind.print) defined_names
*)
      print_equations equations

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant _t = ()

let empty () =
  { defined_names = Name.Map.empty;
    equations = Name.Map.empty;
  }

let is_empty { defined_names; equations; } =
  Name.Map.is_empty defined_names
    && Name.Map.is_empty equations

let equations t = t.equations

let equations_domain t = Name.Map.keys t.equations

let equations_on_outer_env_domain t =
  Name.Set.diff (equations_domain t) (Name.Map.keys t.defined_names)

let restrict_to_names { defined_names; equations; } allowed_names =
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
  let t =
    { defined_names;
      equations;
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

let meet env (t1 : t) (t2 : t) : t =
  (* Care: as per comment in [Typing_env_extension.meet]. *)
  if is_empty t1 then begin
    t2
  end else if is_empty t2 then begin
    t1
  end else begin
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

let meet_equation t env name ty =
  let t' =
    { (empty ()) with
      equations = Name.Map.singleton name ty;
    }
  in
  let env = Meet_env.create env in
  meet env t t'

let defined_names t = t.defined_names

let defined_names_set t =
  Name.Set.fold (fun name defined_names ->
      Bindable_name.Set.add (Name name) defined_names)
    (Name.Map.keys t.defined_names)
    Bindable_name.Set.empty

let defined_names_in_order t =
  Bindable_name.Set.elements (defined_names_set t)
