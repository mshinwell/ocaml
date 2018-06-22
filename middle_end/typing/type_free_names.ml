(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (T : Flambda_type0_internal_intf.S) = struct
  open T

  let free_names_or_alias free_names_contents (or_alias : _ or_alias) acc =
    match or_alias with
    | No_alias contents -> free_names_contents contents acc
    | Type _export_id -> acc
    | Equals simple ->
      match simple with
      | Name name -> Name.Set.add name acc
      | Const _ | Discriminant _ -> acc

  let free_names_unknown_or_join free_names_contents (o : _ unknown_or_join)
        acc =
    match o with
    | Unknown -> acc
    | Join contents_list ->
      List.fold_left (fun free_names contents ->
          free_names_contents contents free_names)
        acc
        contents_list

  let free_names_ty free_names_contents ty acc =
    free_names_or_alias (free_names_unknown_or_join free_names_contents) ty acc

  let free_names_of_kind_naked_number (type n) (_ty : n of_kind_naked_number)
        acc =
    acc

  let rec free_names (t : t) acc =
    match t.descr with
    | Value ty -> free_names_ty free_names_of_kind_value ty acc
    | Naked_number (ty, _kind) ->
      free_names_ty free_names_of_kind_naked_number ty acc
    | Fabricated ty -> free_names_ty free_names_of_kind_fabricated ty acc

  and free_names_of_kind_value (of_kind : of_kind_value) acc =
    match of_kind with
    | Blocks_and_tagged_immediates { blocks; immediates; } ->
      let acc =
        match blocks with
        | Unknown -> acc
        | Known { known_tags_and_sizes; size_at_least_n; } ->
          let acc =
            Tag_and_size.Map.fold (fun _tag_and_size params acc ->
                free_names_parameters params acc)
              known_tags_and_sizes
              acc
          in
          Targetint.OCaml.Map.fold (fun _size params acc ->
              free_names_parameters params acc)
            size_at_least_n
            acc
      in
      begin match immediates with
      | Unknown -> acc
      | Known immediates ->
        Immediate.Map.fold (fun _imm (case : immediate_case) acc ->
            free_names_of_env_extension case.env_extension acc)
          immediates
          acc
      end
    | Boxed_number (Boxed_float n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Boxed_number (Boxed_int32 n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Boxed_number (Boxed_int64 n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Boxed_number (Boxed_nativeint n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Closures { ty; by_closure_id; } ->
      let acc = free_names_dependent_function_type ty acc in
      Closure_id.Map.fold (fun _closure_id (entry : closures_entry) acc ->
          free_names_ty free_names_of_kind_fabricated entry.set_of_closures acc)
        by_closure_id
        acc
    | String _ -> acc

  and free_names_parameters ({ params; env_extension; } : parameters) acc =
    let free_names_params =
      Name.Set.of_list (
        List.map (fun param -> Kinded_parameter.name param) params)
    in
    Name.Set.union acc
      (Name.Set.diff (free_names_of_env_extension env_extension Name.Set.empty)
        free_names_params)

  and free_names_dependent_function_type
        ({ params; results; } : dependent_function_type) acc =
    free_names_parameters params
      (free_names_parameters results acc)

  and free_names_of_kind_fabricated (of_kind : of_kind_fabricated) acc =
    match of_kind with
    | Discriminant discriminant_map ->
      Discriminant.Map.fold
        (fun _discriminant ({ env_extension; } : discriminant_case) acc ->
          free_names_of_env_extension env_extension acc)
        discriminant_map
        acc
    | Set_of_closures set ->
      let acc =
        Closure_id.Map.fold (fun _closure_id ty_fabricated acc ->
            free_names_ty free_names_of_kind_fabricated ty_fabricated acc)
          (extensibility_contents set.closures) acc
      in
      Var_within_closure.Map.fold (fun _var ty_value acc ->
          free_names_ty free_names_of_kind_value ty_value acc)
        (extensibility_contents set.closure_elements) acc
    | Closure closure -> free_names_of_closure closure acc

  and free_names_of_closure (_closure : closure) acc = acc

  and free_names_of_env_extension
        { first_definitions; at_or_after_cut_point; last_equations_rev;
          cse; } acc =
    (* CR mshinwell: This is copied from typing_env_extension.ml, we need to
      share this *)
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
    Name.Set.diff free_names defined_names

  let free_names_set t =
    free_names t Name.Set.empty

  let free_names t =
  let result =
      Name_occurrences.create_from_set_in_types (free_names_set t)
  in
  (*
  Format.eprintf "Free names %a from: %a\n%!"
    Name_occurrences.print result print t;
  *)
  result
end
