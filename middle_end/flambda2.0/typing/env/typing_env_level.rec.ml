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

let print_with_cache ~cache ppf ({ defined_names; equations; } : t) =
  let print_equations ppf equations =
    let equations = Name.Map.bindings equations in
    match equations with
    | [] -> Format.pp_print_string ppf "()"
    | _::_ ->
      Format.pp_print_string ppf "(";
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (name, ty) ->
          Format.fprintf ppf
            "@[<hov 1>%s%a%s@ :@ %a@]"
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
        @[<hov 1>(equations@ @[<hov 1>%a@])@])\
        @]"
      print_equations equations
  else
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(defined_names@ @[<hov 1>%a@])@]@;\
        @[<hov 1>(equations@ @[<hov 1>%a@])@]\
        )@]"
      Name.Set.print (Name.Map.keys defined_names)
      print_equations equations

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant _t = ()

let empty =
  { defined_names = Name.Map.empty;
    equations = Name.Map.empty;
  }

let is_empty { defined_names; equations; } =
  Name.Map.is_empty defined_names
    && Name.Map.is_empty equations

let equations t = t.equations

let defined_names t = t.defined_names

let add_definition t name kind =
  if Name.Map.mem name t.defined_names then begin
    Misc.fatal_errorf "Environment extension already binds name %a:@ %a"
      Name.print name
      print t
  end;
  { t with
    defined_names = Name.Map.add name kind t.defined_names
  }

let one_equation name ty =
  { defined_names = Name.Map.empty;
    equations = Name.Map.singleton name ty;
  }

let add_or_replace_equation t name ty =
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
    (* CR mshinwell: There may be a more efficient way of doing this. *)
    let env = Meet_env.env env in
    let env = Typing_env.increment_scope env in
    let env = Typing_env.add_env_extension env t1 in
    let env = Typing_env.add_env_extension env t2 in
    let level = Typing_env.current_scope env in
    let env_extension, _names_in_scope_at_cut =
      Typing_env.cut env ~unknown_if_defined_at_or_later_than:level
    in
    env_extension
  end

let join env (t1 : t) (t2 : t) : t =
  (* This restriction will be relaxed in the full type system. *)
  if not
    (Name.Map.is_empty t1.defined_names && Name.Map.is_empty t2.defined_names)
  then begin
    Misc.fatal_errorf "Cannot join environment levels that define names:@ \
        %a@ and@ %a"
      print t1
      print t2
  end;
  let names_with_equations_in_join =
    Name.Set.inter (Name.Map.keys t1.equations) (Name.Map.keys t2.equations)
  in
  Name.Set.fold (fun name t ->
      assert (not (Name.Map.mem name t.equations));
      let ty1 = find_equation t1 name in
      let ty2 = find_equation t2 name in
      let join_ty = Api_meet_and_join.join ~bound_name:name env ty1 ty2 in
      add_or_replace_equation t name join_ty)
    names_with_equations_in_join
    empty

let erase_aliases t ~allowed =
  let equations =
    Name.Map.map (fun ty -> Type_erase_aliases.erase_aliases ty ~allowed)
      t.equations
  in
  { t with
    equations;
  }

let meet_equation t1 env name ty =
  let t2 =
    { defined_names = Name.Map.empty;
      equations = Name.Map.singleton name ty;
    }
  in
  meet env t1 t2

let remove_definitions_and_equations_thereon t =
  let equations =
    Name.Map.filter (fun name _ty -> not (Name.Map.mem name t.defined_names))
      t.equations
  in
  { defined_names = Name.Map.empty;
    equations;
  }
