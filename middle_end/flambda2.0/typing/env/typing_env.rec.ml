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

type cached = {
  names_to_types : Flambda_types.t Name.Map.t;
  aliases : Aliases.t;
}

type one_level = {
  level : Typing_env_level.t;
  just_after_level : cached;
}

type t = {
  resolver : (Export_id.t -> Flambda_types.t option);
  prev_levels : one_level Scope_level.Map.t;
  (* CR mshinwell: Rename [Scope_level] to [Scope] and remove this
     pair. *)
  current_level : Scope_level.t * one_level;
}

let is_empty t =
  Typing_env_level.is_empty ((snd t.current_level).level)
    && Scope_level.Map.for_all (fun _level (one_level : one_level) ->
           Typing_env_level.is_empty one_level.level)
         t.prev_levels

let print_with_cache ~cache ppf { names_to_types; aliases; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(names_to_types@ %a)@]@ \
      @[<hov 1>(aliases@ %a)@]\
      )@]"
    (Name.Map.print (Type_printers.print_with_cache ~cache)) names_to_types
    Aliases.print aliases

let print_one_level ~cache:_ ppf { level; just_after_level = _; } =
  Typing_env_level.print ppf level

let print_with_cache ~cache ppf
      ({ resolver = _; prev_levels; current_level; } as t) =
  if is_empty t then
    Format.pp_print_string ppf "Empty"
  else
    Printing_cache.with_cache cache ppf "env" t (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(\
            @[<hov 1>(prev_levels@ %a)@]@ \
            @[<hov 1>(current_level@ (%a@ %a))@])@]"
        (Scope_level.Map.print (print_one_level ~cache)) prev_levels
        Scope_level.print (fst current_level)
        (print_one_level ~cache) (snd current_level))

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let empty_one_level =
  let just_after_level =
    { names_to_types = Name.Map.empty;
      aliases = Simple.Map.empty;
    }
  in
  { level = Typing_env_level.empty;
    just_after_level;
  }

let create ~resolver =
  { resolver;
    prev_levels = Scope_level.Map.empty;
    current_level = (Scope_level.initial, empty_one_level ());
  }

let create_using_resolver_from t = create ~resolver:t.resolver

let resolver t = t.resolver

let increment_scope_level_to t level =
  if Scope_level.(<=) level (fst t.current_level) then begin
    Misc.fatal_errorf "Invalid new level %a:@ %a"
      Scope_level.print level
      print t
  end;
  let one_level =
    { level = Typing_env_level.empty ();
      just_after_level = (snd t.current_level).just_after_level;
    }
  in
  let current_level = Scope_level.next (fst t.current_level) in
  let prev_levels =
    Scope_level.Map.add current_level (snd t.current_level) t.prev_levels
  in
  { t with
    prev_levels;
    current_level = (level, one_level);
  }

let increment_scope_level t =
  increment_scope_level_to t (Scope_level.next (fst t.current_level))

let fast_equal t1 t2 =
  t1 == t2

let domain t =
  let names =
    Name.Set.fold (fun name bindable_names ->
        Bindable_name.Set.add (Name name) bindable_names)
      (Name.Map.keys (snd t.current_level).just_after_level.names_to_types)
      Bindable_name.Set.empty
  in
  Name_occurrences.create_from_set_in_terms names

let find_exn t name : Flambda_types.t * binding_type =
  (* CR mshinwell: Maybe this should cause a fatal error and we shouldn't
     rely on catching the exception *)
  Name.Map.find name (snd t.current_level).just_after_level.names_to_types

let invariant _t = ()

let mem t name =
  Name.Map.mem name (snd t.current_level).just_after_level.names_to_types

let find_opt t name =
  match find_exn t name with
  | exception Not_found -> None
  | ty, binding_type -> Some (ty, binding_type)

type sense =
  | New_equation_must_be_more_precise
  | Existing_equation_must_be_more_precise

let add_definition t name kind =
  if mem t name then begin
    Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
      Name.print name
      print t
  end;
  let level =
    Typing_env_level.add_definition (snd t.current_level).level name kind
  in
  let names_to_types =
    Name.Map.add name (Flambda_type0_core.unknown kind)
      (snd t.current_level).just_after_level.names_to_types
  in
  let just_after_level =
    { (snd t.current_level).just_after_level with
      names_to_types;
    }
  in
  let current_level =
    { level;
      just_after_level;
    }
  in
  let t =
    { t with
      current_level = (fst t.current_level, current_level);
    }
  in
  invariant t;
  t

let add_equation t name ty =
  if not (mem t name) then begin
    Misc.fatal_errorf "Cannot add equation on unbound name %a in \
        environment:@ %a"
      Name.print name
      print t
  end;
  invariant_for_new_equation t name ty;
  let alias = Flambda_type0_core.get_alias ty in
  let equation_with_reverse_alias_already_present =
    match alias with
    | None | Some (Const _ | Discriminant _) -> false
    | Some (Name alias) ->
      Name.Set.mem alias (Aliases.aliases_of_simple t (Simple.name name))
  in
  if equation_with_reverse_alias_already_present then
    t
  else
    let level =
      Typing_env_level.add_or_replace_equation (snd t.current_level).level
        name ty
    in
    let names_to_types =
      Name.Map.add name ty
        (snd t.current_level).just_after_level.names_to_types
    in
    let just_after_level =
      { (snd t.current_level).just_after_level with
        names_to_types;
      }
    in
    let current_level =
      { level;
        just_after_level;
      }
    in
    let t =
      { t with
        current_level = (fst t.current_level, current_level);
      }
    in
    invariant t;
    t

let rec add_opened_env_extension t level : t =
  let t_before_equations =
    Name.Map.fold (fun name kind t ->
        add_definition t name kind)
      (Typing_env_level.defined_names level)
      t
  in
  Name.Map.fold (fun name ty t ->
      match find_opt t name with
      | None -> add_equation t name ty
      | Some (existing_ty, _binding_type) ->
        let meet_ty, meet_env_extension =
          let meet_env = Meet_env.create t in
          Api_meet_and_join.meet meet_env ty existing_ty
        in
        let t = add_env_extension t meet_env_extension in
        let as_or_strictly_less_precise =
          Type_equality.equal ~bound_name:(Some name)
            t_before_equations t_before_equations
            meet_ty existing_ty
        in
        let strictly_more_precise = not as_or_strictly_less_precise in
        if strictly_more_precise then add_equation t name meet_ty
        else t)
    (Typing_env_level.equations level)
    t_before_equations

and add_env_extension t env_extension : t =
  Typing_env_extension.pattern_match env_extension
    ~f:(fun level -> add_opened_env_extension t level)

let current_level t = fst (t.current_level)
let current_level_data t = snd (t.current_level)

let cut0 t ~unknown_if_defined_at_or_later_than:min_level =
  if Scope_level.(>) min_level (current_level t) then
    Typing_env_level.empty
  else
    let all_levels =
      Scope_level.Map.add (current_level t) (current_level_data t)
        t.prev_levels
    in
    let strictly_less, at_min_level, strictly_greater =
      Scope_level.Map.split min_level all_levels
    in
    let at_or_after_cut =
      match at_min_level with
      | None -> strictly_greater
      | Some typing_env_level ->
        Scope_level.Map.add min_level typing_env_level strictly_greater
    in
    let t =
      if Scope_level.Map.is_empty strictly_less then
        create ~resolver:t.resolver
      else
        let current_level, current_level_data =
          Scope_level.Map.max_binding strictly_less
        in
        let prev_levels =
          Scope_level.Map.remove current_level strictly_less
        in
        { resolver = t.resolver;
          prev_levels;
          current_level = (current_level, current_level_data);
        }
    in
    invariant t;
    let meet_env = Meet_env.create t in
    Scope_level.Map.fold (fun _level one_level result ->
        Typing_env_level.meet meet_env one_level.level result)
      at_or_after_cut
      Typing_env_level.empty

let cut t ~unknown_if_defined_at_or_later_than =
  let level = cut0 t ~unknown_if_defined_at_or_later_than in
  Typing_env_extension.create level
