(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Join_env = struct end
module Meet_env = struct end
module Type_equality = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make_types (W : Typing_world.Types) = struct
  open! W

  type t = {
    first_definitions : (Name.t * Flambda_types.t) list;
    at_or_after_cut_point : Typing_env.levels_to_entries;
    last_equations_rev : (Name.t * Flambda_types.t) list;
    cse : Simple.t Flambda_primitive.With_fixed_value.Map.t;
  }
end

module type World = sig
  module rec Types : Typing_world.Types
    with module Typing_env_extension = Make_t (Types)
  include Typing_world.S with module Types := Types
end

module Make (W : World) = struct
  include W.Types.Typing_env_extension
  open! W

  let print_with_cache ~cache ppf
        ({ first_definitions; at_or_after_cut_point; last_equations_rev;
           cse; } : t) =
    let print_binding_list =
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (name, ty) ->
          Format.fprintf ppf "@[(%a %a)@]"
            Name.print name
            (Type_printers.print_with_cache ~cache) ty)
    in
    Format.fprintf ppf
      "@[<hov 1>(\
          @[<hov 1>(first_definitions@ %a)@]@ \
          @[<hov 1>(at_or_after_cut_point@ %a)@]@ \
          @[<hov 1>(last_equations_rev@ %a)@]@ \
          @[<hov 1>(cse@ %a)@])@]"
      print_binding_list first_definitions
      (Typing_env.print_levels_to_entries_with_cache ~cache)
        at_or_after_cut_point
      print_binding_list last_equations_rev
      (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let fast_equal t1 t2 = (t1 == t2)

  let equal env t1 t2 =
    (* CR mshinwell: This should be improved *)
    let equal_names_and_types (name1, ty1) (name2, ty2) =
      Name.equal name1 name2
        && Type_equality.equal_with_env env ty1 ty2
    in
    Misc.Stdlib.List.equal equal_names_and_types
        t1.first_definitions t2.first_definitions
      && Scope_level.Map.equal
           (Scope_level.Sublevel.Map.equal
             (fun (name1, (entry1 : Typing_env.typing_environment_entry))
                  (name2, (entry2 : Typing_env.typing_environment_entry)) ->
               Name.equal name1 name2
                 && match entry1, entry2 with
                    | Definition ty1, Definition ty2 ->
                      Type_equality.equal_with_env env ty1 ty2
                    | Equation ty1, Equation ty2 ->
                      Type_equality.equal_with_env env ty1 ty2
                    | CSE prim1, CSE prim2 ->
                      Flambda_primitive.With_fixed_value.equal prim1 prim2
                    | _, _ -> false))
           t1.at_or_after_cut_point
           t2.at_or_after_cut_point
      && Misc.Stdlib.List.equal equal_names_and_types
           t1.last_equations_rev t2.last_equations_rev

  let invariant _t =
    (* CR mshinwell: Work out what to do here.  Probably just a check that
       the ordering is reasonable. *)
    ()

  let empty =
    { first_definitions = [];
      at_or_after_cut_point = Scope_level.Map.empty;
      last_equations_rev = [];
      cse = Flambda_primitive.With_fixed_value.Map.empty;
    }

  let is_empty t = Scope_level.Map.is_empty t.at_or_after_cut_point

  let defined_names t =
    let from_first_definitions =
      Name.Set.of_list (
        List.map (fun (name, _ty) -> name) t.first_definitions)
    in
    Scope_level.Map.fold (fun _level by_sublevel defined_names ->
        Scope_level.Sublevel.Map.fold
          (fun _sublevel (name, (entry : Typing_env.typing_environment_entry))
               defined_names ->
            match entry with
            | Definition _ -> Name.Set.add name defined_names
            | Equation _ | CSE _ -> defined_names)
          by_sublevel
          defined_names)
      t.at_or_after_cut_point
      from_first_definitions

  let equations_on_env t =
    let equations_domain
          { first_definitions = _; at_or_after_cut_point;
            last_equations_rev; cse = _; } =
      let from_at_or_after_cut_point =
        Scope_level.Map.fold (fun _level by_sublevel domain ->
            Scope_level.Sublevel.Map.fold
              (fun _sublevel
                   (name, (entry : Typing_env.typing_environment_entry))
                   domain ->
                match entry with
                | Definition _ -> domain
                | Equation _ | CSE _ -> Name.Set.add name domain)
              by_sublevel
              domain)
          at_or_after_cut_point
          Name.Set.empty
      in
      let from_last_equations_rev =
        Name.Set.of_list (
          List.map (fun (name, _ty) -> name) last_equations_rev)
      in
      Name.Set.union from_at_or_after_cut_point from_last_equations_rev
    in
    Name.Set.diff (equations_domain t) (defined_names t)

  (* CR mshinwell: Move to [Typing_env]? *)
  let free_names_transitive env ty =
    let all_names = ref (Name_occurrences.create ()) in
    let rec loop to_follow =
      all_names := Name_occurrences.union !all_names to_follow;
      match Name_occurrences.choose_and_remove_amongst_everything to_follow with
      | None -> ()
      | Some (name, to_follow) ->
        let ty =
          match Typing_env.find_exn env name with
          | exception Not_found ->
            Misc.fatal_errorf "Unbound name %a whilst finding free names,@ \
                transitively, of %a@ in environment@ %a"
              Name.print name
              Type_printers.print ty
              Typing_env.print env
          | ty, _binding_type -> ty
        in
        let names = Type_free_names.free_names ty in
        loop (Name_occurrences.union to_follow names)
    in
    loop (Type_free_names.free_names ty);
    !all_names

  let free_names_transitive_list (t : t) (env : Typing_env.t) tys =
    let scope_level = Scope_level.next (Typing_env.max_level env) in
    let env = Typing_env.add_or_meet_env_extension env t scope_level in
    List.fold_left (fun names ty ->
        Name_occurrences.union names (free_names_transitive env ty))
      (Name_occurrences.create ())
      tys

  let restrict_to_names t allowed_names =
    let allowed_names = Name_occurrences.everything allowed_names in
    let first_definitions =
      List.filter (fun (name, _ty) ->
          Name.Set.mem name allowed_names)
        t.first_definitions
    in
    let at_or_after_cut_point =
      Scope_level.Map.filter_map (fun _cont_level by_sublevel ->
          let by_sublevel =
            Scope_level.Sublevel.Map.filter_map
              (fun _sublevel ((name, _) as entry) ->
                if Name.Set.mem name allowed_names then Some entry
                else None)
            by_sublevel
          in
          if Scope_level.Sublevel.Map.is_empty by_sublevel then None
          else Some by_sublevel)
        t.at_or_after_cut_point
    in
    let last_equations_rev =
      List.filter (fun (name, _ty) ->
          Name.Set.mem name allowed_names)
        t.last_equations_rev
    in
    let cse =
      Flambda_primitive.With_fixed_value.Map.filter
        (fun _prim (simple : Simple.t) ->
          match simple with
          | Name name -> Name.Set.mem name allowed_names
          | Const _ | Discriminant _ -> true)
        t.cse
    in
    let t =
      { first_definitions;
        at_or_after_cut_point;
        last_equations_rev;
        cse;
      }
    in
    invariant t;
    t

  let restrict_to_definitions t =
    restrict_to_names t (
      Name_occurrences.create_from_set_in_types (defined_names t))

  let restrict_names_to_those_occurring_in_types t env env_allowed_names tys =
    let free_names = free_names_transitive_list t env tys in
    let env_allowed_names = Typing_env.domain env_allowed_names in
    let allowed_names = Name_occurrences.union free_names env_allowed_names in
    restrict_to_names t allowed_names

  type fold_info =
    | Definition_in_extension of T.t
    | Equation of T.t

  let fold t ~init ~(f : _ -> Name.t -> fold_info -> _) =
    let acc =
      List.fold_left (fun acc (name, ty) ->
          f acc name (Definition_in_extension ty))
        init
        (List.rev t.first_definitions)
    in
    let acc =
      Scope_level.Map.fold (fun _level by_sublevel acc ->
          Scope_level.Sublevel.Map.fold
            (fun _sublevel
                 (name, (entry : Typing_env.typing_environment_entry))
                 acc ->
              match entry with
              | Definition ty ->
                f acc name (Definition_in_extension ty)
              | Equation ty ->
                f acc name (Equation ty)
              | CSE _ -> acc)
            by_sublevel
            acc)
        t.at_or_after_cut_point
        acc
    in
    List.fold_left (fun acc (name, ty) ->
        f acc name (Equation ty))
      acc
      t.last_equations_rev

  let add_definition_at_beginning t name ty =
    let first_definitions = (name, ty) :: t.first_definitions in
    { t with
      first_definitions;
    }

  (* CR mshinwell: Invariant check for increased preciseness? *)
  let add_equation t name ty =
    let last_equations_rev = (name, ty) :: t.last_equations_rev in
    { t with
      last_equations_rev;
    }

  let add_cse t name prim =
    let cse =
      match Flambda_primitive.With_fixed_value.Map.find prim t.cse with
      | exception Not_found ->
        Flambda_primitive.With_fixed_value.Map.add prim name t.cse
      | _name -> t.cse
    in
    { t with cse; }

  (* CR mshinwell: These [find] operations need serious optimisation.
     The tricky thing is keeping a "names_to_types" map for the
     "at_or_after_cut_point" map without making [cut] expensive. *)

  let find_first_definitions_exn t name =
    List.assoc name (List.rev t.first_definitions)

  let find_at_or_after_cut_point_exn t name =
    let bindings = Scope_level.Map.bindings t.at_or_after_cut_point in
    let flattened_sublevels_rev =
      List.map (fun (_scope_level, by_sublevel) ->
          List.rev (List.map (fun (_sublevel, binding) -> binding)
            (Scope_level.Sublevel.Map.bindings by_sublevel)))
        bindings
    in
    let flattened_levels_rev = List.rev (List.concat flattened_sublevels_rev) in
    let without_cse =
      Misc.Stdlib.List.filter_map
        (fun (name, (entry : typing_environment_entry)) ->
          match entry with
          | Definition ty | Equation ty -> Some (name, ty)
          | CSE _ -> None)
        flattened_levels_rev
    in
    List.assoc name without_cse

  let find_last_equations_rev_exn t name =
    List.assoc name t.last_equations_rev

  let find_opt t name =
    match find_last_equations_rev_exn t name with
    | ty -> Some ty
    | exception Not_found ->
      match find_at_or_after_cut_point_exn t name with
      | ty -> Some ty
      | exception Not_found ->
        match find_first_definitions_exn t name with
        | ty -> Some ty
        | exception Not_found -> None

  let find t name =
    match find_opt t name with
    | Some ty -> ty
    | None ->
      Misc.fatal_errorf "Unbound name %a in@ %a"
        Name.print name
        print t

  (* CR-someday mshinwell: Attempt to produce a single piece of code that can
     be specialised to produce both [meet] and [join], as we do elsewhere. *)

  let meet (env : Typing_env.t) (t1 : t) (t2 : t) : t =
    if fast_equal t1 t2 then t1
    else if is_empty t1 then t2
    else if is_empty t2 then t1
    else
      let scope_level = Scope_level.next (Typing_env.max_level env) in
      let env = Typing_env.add_or_meet_env_extension env t1 scope_level in
      let env = Typing_env.add_or_meet_env_extension env t2 scope_level in
      Typing_env.cut env ~existential_if_defined_at_or_later_than:scope_level

  let join (env : JE.t) (t1 : t) (t2 : t) : t =
    if fast_equal t1 t2 then t1
    else if is_empty t1 then empty
    else if is_empty t2 then empty
    else
      let env = JE.add_extensions env ~holds_on_left:t1 ~holds_on_right:t2 in
      let names_in_join =
        let equations_in_t1_on_env = equations_on_env t1 in
        let equations_in_t2_on_env = equations_on_env t2 in
        Name.Set.inter equations_in_t1_on_env equations_in_t2_on_env
      in
      let t =
        Name.Set.fold (fun name t ->
            let ty1 = find t1 name in
            let ty2 = find t2 name in
            let join_ty = Meet_and_join.join env ty1 ty2 in
            add_equation t name join_ty)
          names_in_join
          empty
      in
      let preserved_cse_equations t =
        (* CR-someday mshinwell: This could be improved to preserve some of
           those CSE equations that talk about existentially-bound names. *)
        Flambda_primitive.With_fixed_value.Map.filter
          (fun prim (bound_to_or_value : Simple.t) ->
            match bound_to_or_value with
            | Name name when not (Name.Set.mem name names_in_join) ->
              false
            | Name _ | Const _ | Discriminant _ ->
              let free_names_prim =
                Flambda_primitive.With_fixed_value.free_names prim
              in
              Name.Set.subset free_names_prim names_in_join)
          t.cse
      in
      let cse =
        Flambda_primitive.With_fixed_value.Map.merge
          (fun _prim (simple1 : Simple.t option) (simple2 : Simple.t option) ->
            match simple1, simple2 with
            | None, None -> None
            | Some _, None -> simple1
            | None, Some _ -> simple2
            | Some simple1, Some simple2 ->
              (* For the moment just keep this very straightforward. *)
              (* CR-soon mshinwell: Make this take account of aliases. *)
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

  (* CR mshinwell: This needs to do something with [t.cse] perhaps *)
  (* CR mshinwell: Think carefully about whether the freshening is actually
     needed here *)
  let diff t env : t =
    let names_more_precise, _freshened_names_more_precise, _freshening =
      fold t
        ~init:(Name.Set.empty, Name.Set.empty, Freshening.create ())
        ~f:(fun (names_more_precise, freshened_names_more_precise, freshening)
                (name : Name.t)
                (info : fold_info) ->
          match info with
          | Definition_in_extension _ty ->
            let fresh_name, freshening =
              Freshening.freshen_name freshening name
            in
            let names_more_precise =
              Name.Set.add name names_more_precise
            in
            let freshened_names_more_precise =
              Name.Set.add fresh_name freshened_names_more_precise
            in
            names_more_precise, freshened_names_more_precise, freshening
          | Equation ty ->
            let unfreshened_name = name in
            let name = Freshening.apply_name freshening name in
            let ty = T.apply_freshening ty freshening in
            match Typing_env.find_opt env name with
            | None ->
              let names_more_precise =
                Name.Set.add unfreshened_name names_more_precise
              in
              let freshened_names_more_precise =
                Name.Set.add name names_more_precise
              in
              names_more_precise, freshened_names_more_precise, freshening
            | Some (old_ty, _) ->
              let more_precise_using_old_types_for_free_names =
                (* XXX Not sure [env] is right: shouldn't it contain names
                   from the extension too? *)
                Meet_and_join.strictly_more_precise env ty ~than:old_ty
              in
              if more_precise_using_old_types_for_free_names then
                let names_more_precise =
                  Name.Set.add name names_more_precise
                in
                names_more_precise, freshened_names_more_precise, freshening
              else
                let free_names = T.free_names_set ty in
                let more_precise_using_new_types_for_free_names =
                  not (Name.Set.is_empty (
                    Name.Set.inter free_names names_more_precise))
                in
                if more_precise_using_new_types_for_free_names then
                  let names_more_precise =
                    Name.Set.add unfreshened_name names_more_precise
                  in
                  let freshened_names_more_precise =
                    Name.Set.add name names_more_precise
                  in
                  names_more_precise, freshened_names_more_precise, freshening
                else
                  names_more_precise, freshened_names_more_precise, freshening)
    in
    restrict_to_names t
      (Name_occurrences.create_from_set_in_types names_more_precise)

  let apply_permutation_typing_environment_entry
        (entry : typing_environment_entry) perm
        : typing_environment_entry =
    match entry with
    | Definition t ->
      let t' = T.apply_name_permutation t perm in
      if t == t' then entry
      else Definition t'
    | Equation t ->
      let t' = T.apply_name_permutation t perm in
      if t == t' then entry
      else Equation t'
    | CSE prim ->
      let prim' =
        Flambda_primitive.With_fixed_value.apply_name_permutation prim perm
      in
      if prim == prim' then entry
      else CSE prim'

  let apply_name_permutation
        ({ first_definitions; at_or_after_cut_point; last_equations_rev;
           cse; } as t)
        perm : t =
    let first_definitions_changed = ref false in
    let first_definitions' =
      List.map (fun (name, t) ->
          let name' = Name_permutation.apply_name perm name in
          let t' = T.apply_name_permutation t perm in
          if (not (name == name')) && (not (t == t')) then begin
            first_definitions_changed := true
          end;
          name', t')
        first_definitions
    in
    let at_or_after_cut_point' =
      Scope_level.Map.map_sharing (fun by_sublevel ->
          Scope_level.Sublevel.Map.map_sharing
            (fun ((name, (entry : typing_environment_entry)) as datum) ->
              let name' = Name_permutation.apply_name perm name in
              let entry' =
                apply_permutation_typing_environment_entry entry perm
              in
              if name == name' && entry == entry' then datum
              else name', entry')
            by_sublevel)
        at_or_after_cut_point
    in
    let last_equations_rev_changed = ref false in
    let last_equations_rev' =
      List.map (fun (name, t) ->
          let name' = Name_permutation.apply_name perm name in
          let t' = T.apply_name_permutation t perm in
          if (not (name == name')) || (not (t == t')) then begin
            last_equations_rev_changed := true
          end;
          name', t')
        last_equations_rev
    in
    let cse_changed = ref false in
    let cse' =
      Flambda_primitive.With_fixed_value.Map.fold (fun prim simple cse' ->
          let simple' = Name_permutation.apply_simple perm simple in
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
    if (not !first_definitions_changed)
      && at_or_after_cut_point == at_or_after_cut_point'
      && (not !last_equations_rev_changed)
      && (not !cse_changed)
    then t
    else 
      { first_definitions = first_definitions';
        at_or_after_cut_point = at_or_after_cut_point';
        last_equations_rev = last_equations_rev';
        cse = cse';
      }

  let free_names
        { first_definitions; at_or_after_cut_point; last_equations_rev;
          cse; } =
    let defined_names =
      let from_first_definitions =
        Name.Set.of_list (
          List.map (fun (name, _ty) -> name) first_definitions)
      in
      Scope_level.Map.fold (fun _level by_sublevel defined_names ->
          Scope_level.Sublevel.Map.fold
            (fun _sublevel (name, (entry : typing_environment_entry))
                defined_names ->
              match entry with
              | Definition _ -> Name.Set.add name defined_names
              | Equation _ | CSE _ -> defined_names)
            by_sublevel
            defined_names)
        at_or_after_cut_point
        from_first_definitions
    in
    let free_names_first_definitions =
      List.fold_left (fun acc (_name, t) -> free_names t acc)
        acc
        first_definitions
    in
    let free_names_at_or_after_cut_point =
      Scope_level.Map.fold (fun _level by_sublevel acc ->
          Scope_level.Sublevel.Map.fold
            (fun _sublevel (name, (entry : typing_environment_entry)) acc ->
              match entry with
              | Definition t -> free_names t acc
              | Equation t -> free_names t (Name.Set.add name acc)
              | CSE prim ->
                Name.Set.union acc
                  (Flambda_primitive.With_fixed_value.free_names prim))
            by_sublevel
            acc)
        at_or_after_cut_point
        acc
    in
    let free_names_last_equations_rev =
      List.fold_left (fun acc (name, t) ->
          free_names t (Name.Set.add name acc))
        acc
        last_equations_rev
    in
    let free_names_last_equations_rev_and_cse =
      Flambda_primitive.With_fixed_value.Map.fold
        (fun prim (simple : Simple.t) acc ->
          match simple with
          | Const _ | Discriminant _ -> acc
          | Name name ->
            let acc =
              Name.Set.union acc
                (Flambda_primitive.With_fixed_value.free_names prim)
            in
            Name.Set.add name acc)
        cse
        free_names_last_equations_rev
    in
    let free_names =
      Name.Set.union free_names_first_definitions
        (Name.Set.union free_names_at_or_after_cut_point
          free_names_last_equations_rev_and_cse)
    in
    Name_occurrences.create_from_set_in_types
      (Name.Set.diff free_names defined_names)
end
