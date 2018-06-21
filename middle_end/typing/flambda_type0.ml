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

(* CR mshinwell: This warning appears to be broken (e.g. it claims
   [Meet_and_join_value] is unused) *)
[@@@ocaml.warning "-60"]

(* CR mshinwell: Add invariant check that one-case discriminants don't have
   any equations (for all cases with equations) *)

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module K = Flambda_kind

module Outer_namespace = struct
  module Join_env = Join_env
  module Meet_and_join_value = Meet_and_join_value
  module Meet_and_join_naked_number = Meet_and_join_naked_number
  module Meet_and_join_fabricated = Meet_and_join_fabricated
  module Parameters = Parameters
  module Typing_environment0 = Typing_env
  module Typing_env_extension = Typing_env_extension
  module Type_equality = Type_equality
end

module Make (Expr : Expr_intf.S) = struct
  module T = Flambda_type0_internal_intf.S_impl (Expr)
  include T

  let is_empty_typing_environment (env : typing_environment) =
    Name.Map.is_empty env.names_to_types

  let is_empty_env_extension (ext : env_extension) =
    match ext.first_definitions, ext.last_equations_rev with
    | [], [] -> Scope_level.Map.is_empty ext.at_or_after_cut_point
    | _, _ -> false

  let print_extensibility print_contents ppf (e : _ extensibility) =
    match e with
    | Open contents ->
      Format.fprintf ppf "@[(Open@ %a)@]" print_contents contents
    | Exactly contents ->
      Format.fprintf ppf "@[(Exactly@ %a)@]" print_contents contents

  let _print_mutable_or_immutable print_contents ppf
        (mut : _ mutable_or_immutable) =
    match mut with
    | Immutable contents -> print_contents ppf contents
    | Mutable -> Format.pp_print_string ppf "<mutable>"

  let print_or_alias print_descr ppf (or_alias : _ or_alias) =
    match or_alias with
    | No_alias descr -> print_descr ppf descr
    | Equals simple ->
      Format.fprintf ppf "@[(%s=%s %a)@]"
        (Misc_color.bold_red ())
        (Misc_color.reset ())
        Simple.print simple
    | Type export_id ->
      Format.fprintf ppf "@[(%s=export_id%s %a)@]"
        (Misc_color.bold_red ())
        (Misc_color.reset ())
        Export_id.print export_id

  let unicode = true  (* CR mshinwell: move elsewhere *)

  let print_unknown_or_join print_contents ppf (o : _ unknown_or_join) =
    let colour = Misc_color.bold_red () in
    match o with
    | Unknown -> Format.fprintf ppf "%sT%s" colour (Misc_color.reset ())
    | Join [] ->
      if unicode then
        Format.fprintf ppf "%s\u{22a5}%s" colour (Misc_color.reset ())
      else
        Format.fprintf ppf "%s_|_%s" colour (Misc_color.reset ())
    | Join [contents] -> print_contents ppf contents
    | Join incompatibles ->
      Format.fprintf ppf "@[(Join_incompatible@ (%a))@]"
        (Format.pp_print_list print_contents) incompatibles

  let print_ty_generic print_contents ppf ty =
    (print_or_alias (print_unknown_or_join print_contents)) ppf ty

  let print_of_kind_naked_number (type n) ppf (n : n of_kind_naked_number) =
    match n with
    | Immediate i ->
      Format.fprintf ppf "@[(Naked_immediates@ (%a))@]"
        Immediate.Set.print i
    | Float f ->
      Format.fprintf ppf "@[(Naked_floats@ (%a))@]"
        Float_by_bit_pattern.Set.print f
    | Int32 i ->
      Format.fprintf ppf "@[(Naked_int32s@ (%a))@]"
        Int32.Set.print i
    | Int64 i ->
      Format.fprintf ppf "@[(Naked_int64s@ (%a))@]"
        Int64.Set.print i
    | Nativeint i ->
      Format.fprintf ppf "@[(Naked_nativeints@ (%a))@]"
        Targetint.Set.print i

  let print_ty_naked_number (type n) ppf (ty : n ty_naked_number) =
    print_ty_generic print_of_kind_naked_number ppf ty

  let print_of_kind_value_boxed_number (type n)
        ppf (n : n of_kind_value_boxed_number) =
    match n with
    | Boxed_float f ->
      Format.fprintf ppf "@[(Boxed_float@ (%a))@]"
        print_ty_naked_number f
    | Boxed_int32 i ->
      Format.fprintf ppf "@[(Boxed_int32@ (%a))@]"
        print_ty_naked_number i
    | Boxed_int64 i ->
      Format.fprintf ppf "@[(Boxed_int64@ (%a))@]"
        print_ty_naked_number i
    | Boxed_nativeint i ->
      Format.fprintf ppf "@[(Boxed_nativeint@ (%a))@]"
        print_ty_naked_number i

  let rec print_immediate_case ~cache ppf
        ({ env_extension; } : immediate_case) =
    Format.fprintf ppf "@[<hov 1>(env_extension@ %a)@]"
      (print_typing_env_extension_with_cache ~cache) env_extension

  and print_immediates ~cache ppf cases =
    let no_env_extension =
      Immediate.Map.for_all (fun _imm ({ env_extension; } : immediate_case) ->
          is_empty_env_extension env_extension)
        cases
    in
    if no_env_extension then
      Immediate.Set.print ppf (Immediate.Map.keys cases)
    else
      Immediate.Map.print (print_immediate_case ~cache) ppf cases

  and print_of_kind_value ~cache ppf (of_kind_value : of_kind_value) =
    let blocks_empty
          ({ known_tags_and_sizes; size_at_least_n; } : blocks) =
      Tag_and_size.Map.is_empty known_tags_and_sizes
        && Targetint.OCaml.Map.is_empty size_at_least_n
    in
    match of_kind_value with
    | Blocks_and_tagged_immediates { blocks; immediates; } ->
      begin match blocks, immediates with
      | Known blocks, Known immediates
          when not (blocks_empty blocks)
            && Immediate.Map.is_empty immediates ->
        Format.fprintf ppf "@[<hv 1>(blocks@ @[%a@])@])@]"
          (print_blocks ~cache) blocks
      | Known blocks, Known immediates
          when blocks_empty blocks
            && not (Immediate.Map.is_empty immediates)
            && Immediate.Map.for_all
                 (fun _imm ({ env_extension; } : immediate_case) ->
                   is_empty_env_extension env_extension)
                 immediates ->
        Format.fprintf ppf "@[%a@]"
          Immediate.Set.print (Immediate.Map.keys immediates)
      | _ ->
        Format.fprintf ppf
          "@[<hov 1>(Blocks_and_immediates@ \
             @[<hov 1>(blocks@ %a)@]@ \
             @[<hov 1>(immediates@ %a)@])@]"
          (Or_unknown.print (print_blocks ~cache)) blocks
          (Or_unknown.print (print_immediates ~cache)) immediates
      end
    | Boxed_number n ->
      Format.fprintf ppf "@[(Boxed_number %a)@]"
        print_of_kind_value_boxed_number n
    | Closures closures -> print_closures ~cache ppf closures
    | String str_infos ->
      Format.fprintf ppf "@[(Strings (%a))@]" String_info.Set.print str_infos

  and print_ty_value_with_cache ~cache ppf (ty : ty_value) =
    print_ty_generic (print_of_kind_value ~cache) ppf ty

  and print_ty_value ppf (ty : ty_value) =
    print_ty_value_with_cache ~cache:(Printing_cache.create ()) ppf ty

  and print_ty_value_array ~cache ppf ty_values =
    Format.fprintf ppf "@[[| %a |]@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
        (print_ty_value_with_cache ~cache))
      (Array.to_list ty_values)

  and print_parameters ~cache ppf { params; env_extension; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(params@ %a)@]@ \
        @[<hov 1>(env_extension@ %a)@])@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Kinded_parameter.print) params
      (print_typing_env_extension_with_cache ~cache) env_extension

  and print_dependent_function_type ~cache ppf
        ({ params; results; } : dependent_function_type) =
    Format.fprintf ppf
      "@[<hov 1>(\
         @[<hov 1>(params@ %a)@]@ \
         @[<hov 1>(results@ %a)@])@]"
      (print_parameters ~cache) params
      (print_parameters ~cache) results

  and print_closures ~cache ppf ({ ty; by_closure_id; } : closures) =
    Format.fprintf ppf
      "@[<hov 1>(closures@ \
         @[<hov 1>(ty@ %a)@]@ \
         @[<hov 1>(by_closure_id@ %a)@])@]"
      (print_dependent_function_type ~cache) ty
      (Closure_id.Map.print (print_closures_entry ~cache)) by_closure_id

  and print_closures_entry ~cache ppf (entry : closures_entry) =
    Format.fprintf ppf "@[(set_of_closures@ %a)@]"
      (print_ty_fabricated_with_cache ~cache) entry.set_of_closures

  and print_inlinable_function_declaration_with_cache ~cache ppf
        (decl : inlinable_function_declaration) =
    Printing_cache.with_cache cache ppf "inlinable_fundecl" decl (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(inlinable@ \
          @[<hov 1>(closure_origin@ %a)@]@ \
          @[<hov 1>(continuation_param@ %a)@]@ \
          @[<hov 1>(exn_continuation_param@ %a)@]@ \
          @[<hov 1>(is_classic_mode@ %b)@]@ \
          @[<hov 1>(body@ %a)@]@ \
          @[<hov 1>(free_names_in_body@ %a)@]@ \
          @[<hov 1>(stub@ %b)@]@ \
          @[<hov 1>(dbg@ %a)@]@ \
          @[<hov 1>(inline@ %a)@]@ \
          @[<hov 1>(specialise@ %a)@]@ \
          @[<hov 1>(is_a_functor@ %b)@]@ \
          @[<hov 1>(invariant_params@ %a)@]@ \
          @[<hov 1>(size@ %a)@]@ \
          @[<hov 1>(direct_call_surrogate@ %a)@]@ \
          @[<hov 1>(my_closure@ %a)@])@]"
        Closure_origin.print decl.closure_origin
        Continuation.print decl.continuation_param
        Continuation.print decl.exn_continuation_param
        decl.is_classic_mode
        (Expr.print_with_cache ~cache) decl.body
        Name_occurrences.print decl.free_names_in_body
        decl.stub
        Debuginfo.print_compact decl.dbg
        print_inline_attribute decl.inline
        print_specialise_attribute decl.specialise
        decl.is_a_functor
        Variable.Set.print (Lazy.force decl.invariant_params)
        (Misc.Stdlib.Option.print Format.pp_print_int) (Lazy.force decl.size)
        (Misc.Stdlib.Option.print Closure_id.print) decl.direct_call_surrogate
        Variable.print decl.my_closure)

  and print_inlinable_function_declaration ppf decl =
    print_inlinable_function_declaration_with_cache
      ~cache:(Printing_cache.create ())
      ppf decl

  and print_non_inlinable_function_declarations ppf ~cache:_
        (decl : non_inlinable_function_declarations) =
    Format.fprintf ppf
      "@[(Non_inlinable@ \
        @[(direct_call_surrogate %a)@])@]"
      (Misc.Stdlib.Option.print Closure_id.print) decl.direct_call_surrogate

  and print_function_declarations ~cache ppf
        (decl : function_declarations) =
    match decl with
    | Inlinable decls ->
      Format.fprintf ppf "@[(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          (print_inlinable_function_declaration_with_cache ~cache))
        decls
    | Non_inlinable decl ->
      print_non_inlinable_function_declarations ~cache ppf decl

  and print_set_of_closures ~cache ppf (set : set_of_closures) =
    Printing_cache.with_cache cache ppf "set" set (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(Set_of_closures@ \
            @[<hov 1>(closures@ %a)@]@ \
            @[<hov 1>(closure_elements@ %a)@])@]"
        (print_extensibility (
            Closure_id.Map.print (print_ty_fabricated_with_cache ~cache)))
          set.closures
        (print_extensibility (
            Var_within_closure.Map.print (print_ty_value_with_cache ~cache)))
          set.closure_elements)

  and print_closure ~cache ppf (closure : closure) =
    Format.fprintf ppf "@[<hov 1>(Closure (function_decls@ %a))@]"
      (print_function_declarations ~cache) closure.function_decls

  and print_discriminant_case ~cache ppf ({ env_extension; } : discriminant_case) =
    Format.fprintf ppf "@[<hov 1>(env_extension@ %a)@]"
      (print_typing_env_extension_with_cache ~cache) env_extension

  and print_of_kind_fabricated ~cache ppf (o : of_kind_fabricated) =
    match o with
    | Discriminant discriminant_map ->
      let no_env_extension =
        Discriminant.Map.for_all
          (fun _ ({ env_extension; } : discriminant_case) ->
            is_empty_env_extension env_extension)
          discriminant_map
      in
      if not no_env_extension then
        Format.fprintf ppf "@[<hov 1>(Discriminant@ %a)@]"
          (Discriminant.Map.print (print_discriminant_case ~cache))
          discriminant_map
      else
        Format.fprintf ppf "@[%a@]"
          Discriminant.Set.print (Discriminant.Map.keys discriminant_map)
    | Set_of_closures set -> print_set_of_closures ~cache ppf set
    | Closure closure -> print_closure ~cache ppf closure

  and print_ty_fabricated_with_cache ~cache ppf (ty : ty_fabricated) =
    print_ty_generic (print_of_kind_fabricated ~cache) ppf ty

  and print_ty_fabricated ppf (ty : ty_fabricated) =
    print_ty_fabricated_with_cache ~cache:(Printing_cache.create ()) ppf ty

  and print_descr ~cache ppf (descr : descr) =
    match descr with
    | Value ty ->
      Format.fprintf ppf "@[<hov 1>(Val@ %a)@]"
        (print_ty_value_with_cache ~cache) ty
    | Naked_number (ty, _kind) ->
      Format.fprintf ppf "@[<hov 1>(Naked@ %a)@]" print_ty_naked_number ty
    | Fabricated ty ->
      Format.fprintf ppf "@[<hov 1>(Fab@ %a)@]"
        (print_ty_fabricated_with_cache ~cache) ty

  and print_with_cache ~cache ppf (t : t) =
    match t.phantom with
    | None -> print_descr ~cache ppf t.descr
    | Some In_types ->
      Format.fprintf ppf "@[(Phantom_in_types@ (%a))@]"
        (print_descr ~cache) t.descr
    | Some Debug_only ->
      Format.fprintf ppf "@[(Phantom_debug_only@ (%a))@]"
        (print_descr ~cache) t.descr

  and print ppf (t : t) =
    let cache : Printing_cache.t = Printing_cache.create () in
    print_with_cache ~cache ppf t

  and print_levels_to_entries_with_cache ~cache ppf levels_to_entries =
    Scope_level.Map.print (
      Scope_level.Sublevel.Map.print (fun ppf (name, entry) ->
        Format.fprintf ppf "@[(%a %a)@]"
          Name.print name
          (print_typing_environment_entry_with_cache ~cache) entry))
      ppf levels_to_entries

  and print_typing_environment_entry0_with_cache ~cache ppf
        (entry : typing_environment_entry0) =
    match entry with
    | Definition ty ->
      Format.fprintf ppf "@[(Definition %a)@]"
        (print_with_cache ~cache) ty
    | Equation ty ->
      Format.fprintf ppf "@[(Equation %a)@]"
        (print_with_cache ~cache) ty

  and print_typing_environment_entry_with_cache ~cache ppf
        (entry : typing_environment_entry) =
    match entry with
    | Definition ty ->
      print_typing_environment_entry0_with_cache ~cache ppf
        ((Definition ty) : typing_environment_entry0)
    | Equation ty ->
      print_typing_environment_entry0_with_cache ~cache ppf
        ((Equation ty) : typing_environment_entry0)
    | CSE with_fixed_value ->
      Format.fprintf ppf "@[(CSE %a)@]"
        Flambda_primitive.With_fixed_value.print with_fixed_value

  and print_typing_environment_entry ppf entry =
    print_typing_environment_entry_with_cache
      ~cache:(Printing_cache.create ()) ppf entry

  and print_typing_environment_with_cache ~cache ppf
        ({ resolver = _; aliases; names_to_types; cse; levels_to_entries;
           next_sublevel_by_level = _; were_existentials; } as env) =
    if Name.Map.is_empty names_to_types then
      Format.pp_print_string ppf "Empty"
    else
      Printing_cache.with_cache cache ppf "env" env (fun ppf () ->
        let print_scope_level_and_entry0 ppf (_scope_level, entry) =
          print_typing_environment_entry0_with_cache ~cache ppf entry
        in
        (* CR mshinwell: Add flag to disable this filtering *)
        let names_to_types =
          Name.Map.filter (fun name _entry ->
              not (Name.is_predefined_exception name))
            names_to_types
        in
        let levels_to_entries =
          Scope_level.Map.filter_map (fun _cont_level by_sublevel ->
              let by_sublevel =
                Scope_level.Sublevel.Map.filter_map
                  (fun _sublevel ((name, _) as entry) ->
                    if not (Name.is_predefined_exception name) then Some entry
                    else None)
                by_sublevel
              in
              if Scope_level.Sublevel.Map.is_empty by_sublevel then None
              else Some by_sublevel)
            levels_to_entries
        in
        if Name.Set.is_empty were_existentials
             && Flambda_primitive.With_fixed_value.Map.is_empty cse
        then
          Format.fprintf ppf
            "@[<hov 1>(\
                @[<hov 1>(aliases@ %a)@]@ \
                @[<hov 1>(names_to_types@ %a)@]@ \
                @[<hov 1>(levels_to_entries@ %a)@])@]"
            (Simple.Map.print Name.Set.print) aliases
            (Name.Map.print print_scope_level_and_entry0) names_to_types
            (print_levels_to_entries_with_cache ~cache) levels_to_entries
        else if Name.Set.is_empty were_existentials then
          Format.fprintf ppf
            "@[<hov 1>(\
                @[<hov 1>(aliases@ %a)@]@ \
                @[<hov 1>(names_to_types@ %a)@]@ \
                @[<hov 1>(cse@ %a)@]@ \
                @[<hov 1>(levels_to_entries@ %a)@])@]"
            (Simple.Map.print Name.Set.print) aliases
            (Name.Map.print print_scope_level_and_entry0) names_to_types
            (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
            (print_levels_to_entries_with_cache ~cache) levels_to_entries
        else
          Format.fprintf ppf
            "@[<hov 1>(\
                @[<hov 1>(aliases@ %a)@]@ \
                @[<hov 1>(names_to_types@ %a)@]@ \
                @[<hov 1>(cse@ %a)@]@ \
                @[<hov 1>(levels_to_names@ %a)@]@ \
                @[<hov 1>(were_existentials@ %a)@])@]"
            (Simple.Map.print Name.Set.print) aliases
            (Name.Map.print print_scope_level_and_entry0) names_to_types
            (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
            (print_levels_to_entries_with_cache ~cache) levels_to_entries
            Name.Set.print were_existentials)

  and print_typing_env_extension_with_cache ~cache ppf
        ({ first_definitions; at_or_after_cut_point; last_equations_rev;
           cse; } : env_extension) =
    let print_binding_list =
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (name, ty) ->
          Format.fprintf ppf "@[(%a %a)@]"
            Name.print name
            (print_with_cache ~cache) ty)
    in
    Format.fprintf ppf
      "@[<hov 1>(\
          @[<hov 1>(first_definitions@ %a)@]@ \
          @[<hov 1>(at_or_after_cut_point@ %a)@]@ \
          @[<hov 1>(last_equations_rev@ %a)@]@ \
          @[<hov 1>(cse@ %a)@])@]"
      print_binding_list first_definitions
      (print_levels_to_entries_with_cache ~cache) at_or_after_cut_point
      print_binding_list last_equations_rev
      (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse

  let print_typing_environment ppf env =
    print_typing_environment_with_cache ~cache:(Printing_cache.create ())
      ppf env

  let print_typing_env_extension ppf env_extension =
    print_typing_env_extension_with_cache ~cache:(Printing_cache.create ())
      ppf env_extension

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

  let force_to_kind_value t =
    match t.descr with
    | Value ty_value -> ty_value
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Value]):@ %a"
        print t

  let force_to_kind_naked_immediate (t : t) : Immediate.Set.t ty_naked_number =
    match t.descr with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_immediate) ->
      ty_naked_number
    | Naked_number _
    | Fabricated _
    | Value _ ->
      Misc.fatal_errorf
        "Type has wrong kind (expected [Naked_number Immediate]):@ %a"
        print t

  let force_to_kind_naked_float (t : t)
        : Float_by_bit_pattern.Set.t ty_naked_number =
    match t.descr with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_float) ->
      ty_naked_number
    | Naked_number _
    | Fabricated _
    | Value _ ->
      Misc.fatal_errorf
        "Type has wrong kind (expected [Naked_number Float]):@ %a"
        print t

  let force_to_kind_naked_int32 (t : t) : Int32.Set.t ty_naked_number =
    match t.descr with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_int32) ->
      ty_naked_number
    | Naked_number _
    | Fabricated _
    | Value _ ->
      Misc.fatal_errorf
        "Type has wrong kind (expected [Naked_number Int32]):@ %a"
        print t

  let force_to_kind_naked_int64 (t : t) : Int64.Set.t ty_naked_number =
    match t.descr with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_int64) ->
      ty_naked_number
    | Naked_number _
    | Fabricated _
    | Value _ ->
      Misc.fatal_errorf
        "Type has wrong kind (expected [Naked_number Int64]):@ %a"
        print t

  let force_to_kind_naked_nativeint (t : t) : Targetint.Set.t ty_naked_number =
    match t.descr with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_nativeint) ->
      ty_naked_number
    | Naked_number _
    | Fabricated _
    | Value _ ->
      Misc.fatal_errorf
        "Type has wrong kind (expected [Naked_number Nativeint]):@ %a"
        print t

  let force_to_kind_naked_number (type n) (kind : n K.Naked_number.t) (t : t)
        : n ty_naked_number =
    match t.descr, kind with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_immediate),
        K.Naked_number.Naked_immediate ->
      ty_naked_number
    | Naked_number (ty_naked_number, K.Naked_number.Naked_float),
        K.Naked_number.Naked_float ->
      ty_naked_number
    | Naked_number (ty_naked_number, K.Naked_number.Naked_int32),
        K.Naked_number.Naked_int32 ->
      ty_naked_number
    | Naked_number (ty_naked_number, K.Naked_number.Naked_int64),
        K.Naked_number.Naked_int64 ->
      ty_naked_number
    | Naked_number (ty_naked_number, K.Naked_number.Naked_nativeint),
        K.Naked_number.Naked_nativeint ->
      ty_naked_number
    | Naked_number _, _
    | Fabricated _, _
    | Value _, _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Naked_number %a]):@ %a"
        K.Naked_number.print kind
        print t

  let force_to_kind_fabricated t =
    match t.descr with
    | Fabricated ty_fabricated -> ty_fabricated
    | Value _
    | Naked_number _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Fabricated]):@ %a"
        print t

  let ty_is_obviously_bottom (ty : _ ty) =
    match ty with
    | No_alias (Join []) -> true
    | _ -> false

  let is_obviously_bottom (t : t) =
    match t.descr with
    | Value ty -> ty_is_obviously_bottom ty
    | Naked_number (ty, _) -> ty_is_obviously_bottom ty
    | Fabricated ty -> ty_is_obviously_bottom ty

  let of_ty_value ty_value : t =
    { descr = Value ty_value;
      phantom = None;
    }

  let of_ty_naked_number (type n) (ty_naked_number : n ty_naked_number)
        (kind : n K.Naked_number.t) : t =
    { descr = Naked_number (ty_naked_number, kind);
      phantom = None;
    }

  let of_ty_fabricated ty_fabricated : t =
    { descr = Fabricated ty_fabricated;
      phantom = None;
    }

  (* CR-someday mshinwell: Functions such as [alias] and [bottom] could be
     simplified if [K.t] were a GADT. *)

  let phantomize t (occs : K.Phantom_kind.occurrences) : t =
    let ok () = { t with phantom = Some occs; } in
    match t.phantom with
    | None -> ok ()
    | Some In_types ->
      begin match occs with
      | In_types -> t
      | Debug_only -> ok ()
      end
    | Some Debug_only ->
      begin match occs with
      | Debug_only -> t
      | In_types ->
        Misc.fatal_errorf "Cannot change [Debug_only] phantom type back \
            into an [In_types]"
          print t
      end

  (* CR mshinwell: Share with typing_env_extension.ml *)
  let empty_env_extension : env_extension =
    { first_definitions = [];
      at_or_after_cut_point = Scope_level.Map.empty;
      last_equations_rev = [];
      cse = Flambda_primitive.With_fixed_value.Map.empty;
    }

  type 'a type_accessor = typing_environment -> 'a

  let alias_type_of (kind : K.t) name : t =
    match kind with
    | Value ->
      { descr = Value (Equals name);
        phantom = None;
      }
    | Naked_number Naked_immediate ->
      { descr = Naked_number (Equals name, K.Naked_number.Naked_immediate);
        phantom = None;
      }
    | Naked_number Naked_float ->
      { descr = Naked_number (Equals name, K.Naked_number.Naked_float);
        phantom = None;
      }
    | Naked_number Naked_int32 ->
      { descr = Naked_number (Equals name, K.Naked_number.Naked_int32);
        phantom = None;
      }
    | Naked_number Naked_int64 ->
      { descr = Naked_number (Equals name, K.Naked_number.Naked_int64);
        phantom = None;
      }
    | Naked_number Naked_nativeint ->
      { descr = Naked_number (Equals name, K.Naked_number.Naked_nativeint);
        phantom = None;
      }
    | Fabricated ->
      { descr = Fabricated (Equals name);
        phantom = None;
      }
    | Phantom (occs, phantom_kind) ->
      let descr : descr =
        match phantom_kind with
        | Value -> Value (Equals name)
        | Naked_number Naked_immediate ->
          Naked_number (Equals name, K.Naked_number.Naked_immediate)
        | Naked_number Naked_float ->
          Naked_number (Equals name, K.Naked_number.Naked_float)
        | Naked_number Naked_int32 ->
          Naked_number (Equals name, K.Naked_number.Naked_int32)
        | Naked_number Naked_int64 ->
          Naked_number (Equals name, K.Naked_number.Naked_int64)
        | Naked_number Naked_nativeint ->
          Naked_number (Equals name, K.Naked_number.Naked_nativeint)
        | Fabricated -> Fabricated (Equals name)
      in
      { descr;
        phantom = Some occs;
      }

  let alias_type_of_as_ty_value name : ty_value = Equals name

  let alias_type_of_as_ty_fabricated name : ty_fabricated = Equals name

  let alias_type (kind : K.t) export_id : t =
    match kind with
    | Value ->
      { descr = Value (Type export_id);
        phantom = None;
      }
    | Naked_number Naked_immediate ->
      { descr = Naked_number (Type export_id, K.Naked_number.Naked_immediate);
        phantom = None;
      }
    | Naked_number Naked_float ->
      { descr = Naked_number (Type export_id, K.Naked_number.Naked_float);
        phantom = None;
      }
    | Naked_number Naked_int32 ->
      { descr = Naked_number (Type export_id, K.Naked_number.Naked_int32);
        phantom = None;
      }
    | Naked_number Naked_int64 ->
      { descr = Naked_number (Type export_id, K.Naked_number.Naked_int64);
        phantom = None;
      }
    | Naked_number Naked_nativeint ->
      { descr = Naked_number (Type export_id, K.Naked_number.Naked_nativeint);
        phantom = None;
      }
    | Fabricated ->
      { descr = Fabricated (Type export_id);
        phantom = None;
      }
    | Phantom (occs, phantom_kind) ->
      let descr : descr =
        match phantom_kind with
        | Value -> Value (Type export_id)
        | Naked_number Naked_immediate ->
          Naked_number (Type export_id, K.Naked_number.Naked_immediate)
        | Naked_number Naked_float ->
          Naked_number (Type export_id, K.Naked_number.Naked_float)
        | Naked_number Naked_int32 ->
          Naked_number (Type export_id, K.Naked_number.Naked_int32)
        | Naked_number Naked_int64 ->
          Naked_number (Type export_id, K.Naked_number.Naked_int64)
        | Naked_number Naked_nativeint ->
          Naked_number (Type export_id, K.Naked_number.Naked_nativeint)
        | Fabricated -> Fabricated (Type export_id)
      in
      { descr;
        phantom = Some occs;
      }

  let bottom_as_ty_value () : ty_value =
    No_alias (Join [])

  let bottom_as_ty_fabricated () : ty_fabricated =
    No_alias (Join [])

  let bottom (kind : K.t) : t =
    match kind with
    | Value ->
      { descr = Value (No_alias (Join []));
        phantom = None;
      }
    | Naked_number Naked_immediate ->
      { descr =
          Naked_number (No_alias (Join []), K.Naked_number.Naked_immediate);
        phantom = None;
      }
    | Naked_number Naked_float ->
      { descr = Naked_number (No_alias (Join []), K.Naked_number.Naked_float);
        phantom = None;
      }
    | Naked_number Naked_int32 ->
      { descr = Naked_number (No_alias (Join []), K.Naked_number.Naked_int32);
        phantom = None;
      }
    | Naked_number Naked_int64 ->
      { descr = Naked_number (No_alias (Join []), K.Naked_number.Naked_int64);
        phantom = None;
      }
    | Naked_number Naked_nativeint ->
      { descr =
          Naked_number (No_alias (Join []), K.Naked_number.Naked_nativeint);
        phantom = None;
      }
    | Fabricated ->
      { descr = Fabricated (No_alias (Join []));
        phantom = None;
      }
    | Phantom (occs, phantom_kind) ->
      let descr : descr =
        match phantom_kind with
        | Value -> Value (No_alias (Join []))
        | Naked_number Naked_immediate ->
          Naked_number (No_alias (Join []), K.Naked_number.Naked_immediate)
        | Naked_number Naked_float ->
          Naked_number (No_alias (Join []), K.Naked_number.Naked_float)
        | Naked_number Naked_int32 ->
          Naked_number (No_alias (Join []), K.Naked_number.Naked_int32)
        | Naked_number Naked_int64 ->
          Naked_number (No_alias (Join []), K.Naked_number.Naked_int64)
        | Naked_number Naked_nativeint ->
          Naked_number (No_alias (Join []), K.Naked_number.Naked_nativeint)
        | Fabricated -> Fabricated (No_alias (Join []))
      in
      { descr;
        phantom = Some occs;
      }

  let any_value_as_ty_value () : ty_value =
    No_alias Unknown

  let any_fabricated_as_ty_fabricated () : ty_fabricated =
    No_alias Unknown

  let any_naked_float_as_ty_naked_float () : _ ty_naked_number =
    No_alias Unknown

  let any_value () : t =
    { descr = Value (any_value_as_ty_value ());
      phantom = None;
    }

  let no_blocks : blocks =
    { known_tags_and_sizes = Tag_and_size.Map.empty;
      size_at_least_n = Targetint.OCaml.Map.empty;
    }

  let any_tagged_immediate () : t =
    { descr =
        Value (No_alias (Join [Blocks_and_tagged_immediates {
          immediates = Unknown;
          blocks = Known no_blocks;
        }]));
      phantom = None;
    }

  let any_naked_immediate () : t =
    { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_immediate);
      phantom = None;
    }

  let any_naked_float () : t =
    { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_float);
      phantom = None;
    }

  let any_naked_int32 () : t =
    { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_int32);
      phantom = None;
    }

  let any_naked_int64 () : t =
    { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_int64);
      phantom = None;
    }

  let any_naked_nativeint () : t =
    { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_nativeint);
      phantom = None;
    }

  let any_fabricated () : t =
    { descr = Fabricated (No_alias Unknown);
      phantom = None;
    }

  let unknown (kind : K.t) =
    match kind with
    | Value ->
      { descr = Value (No_alias Unknown);
        phantom = None;
      }
    | Naked_number Naked_immediate ->
      { descr =
          Naked_number (No_alias Unknown, K.Naked_number.Naked_immediate);
        phantom = None;
      }
    | Naked_number Naked_float ->
      { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_float);
        phantom = None;
      }
    | Naked_number Naked_int32 ->
      { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_int32);
        phantom = None;
      }
    | Naked_number Naked_int64 ->
      { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_int64);
        phantom = None;
      }
    | Naked_number Naked_nativeint ->
      { descr =
          Naked_number (No_alias Unknown, K.Naked_number.Naked_nativeint);
        phantom = None;
      }
    | Fabricated ->
      { descr = Fabricated (No_alias Unknown);
        phantom = None;
      }
    | Phantom (occs, phantom_kind) ->
      let descr : descr =
        match phantom_kind with
        | Value -> Value (No_alias Unknown)
        | Naked_number Naked_immediate ->
          Naked_number (No_alias Unknown, K.Naked_number.Naked_immediate)
        | Naked_number Naked_float ->
          Naked_number (No_alias Unknown, K.Naked_number.Naked_float)
        | Naked_number Naked_int32 ->
          Naked_number (No_alias Unknown, K.Naked_number.Naked_int32)
        | Naked_number Naked_int64 ->
          Naked_number (No_alias Unknown, K.Naked_number.Naked_int64)
        | Naked_number Naked_nativeint ->
          Naked_number (No_alias Unknown, K.Naked_number.Naked_nativeint)
        | Fabricated -> Fabricated (No_alias Unknown)
      in
      { descr;
        phantom = Some occs;
      }

  let these_naked_immediates (is : Immediate.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Immediate is in
    { descr =
        Naked_number (No_alias (Join [of_kind]),
          K.Naked_number.Naked_immediate);
      phantom = None;
    }

  let these_naked_floats (is : Float_by_bit_pattern.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Float is in
    { descr =
        Naked_number (No_alias (Join [of_kind]),
          K.Naked_number.Naked_float);
      phantom = None;
    }

  let these_naked_int32s (is : Int32.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Int32 is in
    { descr =
        Naked_number (No_alias (Join [of_kind]),
          K.Naked_number.Naked_int32);
      phantom = None;
    }

  let these_naked_int64s (is : Int64.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Int64 is in
    { descr =
        Naked_number (No_alias (Join [of_kind]),
          K.Naked_number.Naked_int64);
      phantom = None;
    }

  let these_naked_nativeints (is : Targetint.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Nativeint is in
    { descr =
        Naked_number (No_alias (Join [of_kind]),
          K.Naked_number.Naked_nativeint);
      phantom = None;
    }

  let this_naked_immediate i =
    these_naked_immediates (Immediate.Set.singleton i)

  let this_naked_float f =
    these_naked_floats (Float_by_bit_pattern.Set.singleton f)

  let this_naked_float_as_ty_naked_float f =
    let fs = Float_by_bit_pattern.Set.singleton f in
    let of_kind : _ of_kind_naked_number = Float fs in
    No_alias (Join [of_kind])

  let this_naked_int32 i =
    these_naked_int32s (Int32.Set.singleton i)

  let this_naked_int64 i =
    these_naked_int64s (Int64.Set.singleton i)

  let this_naked_nativeint i =
    these_naked_nativeints (Targetint.Set.singleton i)

(* This one is tricky
  let tag_immediate (t : t) : t =
    match t with
    | Naked_number (ty_naked_number, Naked_immediate) ->


      Value (No_alias (Ok (No_alias (
        Tagged_immediate ty_naked_immediate))))
    | Value _
    | Naked_number _
    | Fabricated _
    | Phantom _ ->
      Misc.fatal_errorf "Type of wrong kind for [tag_immediate]: %a"
        print t
*)

  let check_not_phantom t reason =
    match t.phantom with
    | None -> ()
    | Some _ ->
      Misc.fatal_errorf "Type given to [%s] cannot be phantom: %a"
        reason
        print t

  let box_float (t : t) : t =
    check_not_phantom t "box_float";
    match t.descr with
    | Naked_number (ty_naked_float, K.Naked_number.Naked_float) ->
      { descr =
          Value (No_alias (Join [
            Boxed_number (Boxed_float ty_naked_float)]));
        phantom = None;
      }
    | Value _
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_float]: %a"
        print t

  let box_int32 (t : t) : t =
    check_not_phantom t "box_int32";
    match t.descr with
    | Naked_number (ty_naked_int32, K.Naked_number.Naked_int32) ->
      { descr =
          Value (No_alias (Join [
            Boxed_number (Boxed_int32 ty_naked_int32)]));
        phantom = None;
      }
    | Value _
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a"
        print t

  let box_int64 (t : t) : t =
    check_not_phantom t "box_int64";
    match t.descr with
    | Naked_number (ty_naked_int64, K.Naked_number.Naked_int64) ->
      { descr =
          Value (No_alias (Join [
            Boxed_number (Boxed_int64 ty_naked_int64)]));
        phantom = None;
      }
    | Value _
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a"
        print t

  let box_nativeint (t : t) : t =
    check_not_phantom t "box_nativeint";
    match t.descr with
    | Naked_number (ty_naked_nativeint, K.Naked_number.Naked_nativeint) ->
      { descr =
          Value (No_alias (Join [
            Boxed_number (Boxed_nativeint ty_naked_nativeint)]));
        phantom = None;
      }
    | Value _
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a"
        print t

  let these_tagged_immediates imms : t =
    if Immediate.Set.is_empty imms then
      bottom (K.value ())
    else
      let immediates =
        Immediate.Set.fold (fun imm map ->
            let case : immediate_case =
              { env_extension = empty_env_extension;
              }
            in
            Immediate.Map.add imm case map)
          imms
          Immediate.Map.empty
      in
      let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
        { immediates = Known immediates;
          blocks = Known no_blocks;
        }
      in
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates
            blocks_and_tagged_immediates]));
        phantom = None;
      }

  (* CR mshinwell: share code with previous function *)
  let these_tagged_immediates_with_envs env_map =
    if Immediate.Map.is_empty env_map then
      bottom (K.value ())
    else
      let immediates =
        Immediate.Map.map (fun env_extension : immediate_case ->
            { env_extension; })
          env_map
      in
      let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
        { immediates = Known immediates;
          blocks = Known no_blocks;
        }
      in
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates
            blocks_and_tagged_immediates]));
        phantom = None;
      }

  let this_tagged_immediate imm =
    these_tagged_immediates (Immediate.Set.singleton imm)

  let any_tagged_bool () =
    let bools =
      Immediate.Set.add Immediate.bool_false
        (Immediate.Set.add Immediate.bool_true Immediate.Set.empty)
    in
    these_tagged_immediates bools

  let this_boxed_float f = box_float (this_naked_float f)
  let this_boxed_int32 f = box_int32 (this_naked_int32 f)
  let this_boxed_int64 f = box_int64 (this_naked_int64 f)
  let this_boxed_nativeint f = box_nativeint (this_naked_nativeint f)

  let these_boxed_floats f = box_float (these_naked_floats f)
  let these_boxed_int32s f = box_int32 (these_naked_int32s f)
  let these_boxed_int64s f = box_int64 (these_naked_int64s f)
  let these_boxed_nativeints f = box_nativeint (these_naked_nativeints f)

  let these_discriminants_as_ty_fabricated discriminants_to_env_extension
        : ty_fabricated =
    let discriminant_map =
      Discriminant.Map.map (fun env_extension : discriminant_case ->
          { env_extension; })
        discriminants_to_env_extension
    in
    No_alias (Join [Discriminant discriminant_map])

  let these_discriminants discriminants_to_env_extension : t =
    { descr = Fabricated (
        these_discriminants_as_ty_fabricated discriminants_to_env_extension);
      phantom = None;
    }

  let this_discriminant_as_ty_fabricated discriminant =
    let discriminant_map =
      Discriminant.Map.singleton discriminant
        ({ env_extension = empty_env_extension; } : discriminant_case)
    in
    No_alias (Join [Discriminant discriminant_map])

  let this_discriminant discriminant : t =
    { descr = Fabricated (
        this_discriminant_as_ty_fabricated discriminant);
      phantom = None;
    }

  let any_discriminant_as_ty_fabricated () : ty_fabricated =
    No_alias Unknown

  let this_immutable_string_as_ty_value str : ty_value =
    let str : String_info.t =
      { contents = Contents str;
        (* CR mshinwell: Possibility for exception? *)
        size = Targetint.OCaml.of_int (String.length str);
      }
    in
    let str = String_info.Set.singleton str in
    No_alias (Join [String str])

  let this_immutable_string str : t =
    { descr = Value (this_immutable_string_as_ty_value str);
      phantom = None;
    }

  let immutable_string_as_ty_value ~size : ty_value =
    let str : String_info.t =
      { contents = Unknown_or_mutable;
        size;
      }
    in
    let str = String_info.Set.singleton str in
    No_alias (Join [String str])

  let immutable_string ~size : t =
    { descr = Value (immutable_string_as_ty_value ~size);
      phantom = None;
    }

  let mutable_string ~size : t =
    let str : String_info.t =
      { contents = Unknown_or_mutable;
        size;
      }
    in
    let str = String_info.Set.singleton str in
    { descr = Value (No_alias (Join [String str]));
      phantom = None;
    }

  let kind (t : t) =
    match t.phantom with
    | None ->
      begin match t.descr with
      | Value _ -> K.value ()
      | Naked_number (_, K.Naked_number.Naked_immediate) -> K.naked_immediate ()
      | Naked_number (_, K.Naked_number.Naked_float) -> K.naked_float ()
      | Naked_number (_, K.Naked_number.Naked_int32) -> K.naked_int32 ()
      | Naked_number (_, K.Naked_number.Naked_int64) -> K.naked_int64 ()
      | Naked_number (_, K.Naked_number.Naked_nativeint) -> K.naked_nativeint ()
      | Fabricated _ -> K.fabricated ()
      end
    | Some occurrences ->
      let phantom_kind =
        let module PK = K.Phantom_kind in
        match t.descr with
        | Value _ -> PK.Value
        | Naked_number (_, K.Naked_number.Naked_immediate) ->
          PK.Naked_number Naked_immediate
        | Naked_number (_, K.Naked_number.Naked_float) ->
          PK.Naked_number Naked_float
        | Naked_number (_, K.Naked_number.Naked_int32) ->
          PK.Naked_number Naked_int32
        | Naked_number (_, K.Naked_number.Naked_int64) ->
          PK.Naked_number Naked_int64
        | Naked_number (_, K.Naked_number.Naked_nativeint) ->
          PK.Naked_number Naked_nativeint
        | Fabricated _ -> PK.Fabricated
      in
      match occurrences with
      | In_types -> K.phantom In_types phantom_kind
      | Debug_only -> K.phantom Debug_only phantom_kind

  let create_parameters_from_types ts : parameters =
    let params =
      List.mapi (fun index t ->
          let kind = kind t in
          let var = Variable.create (Format.sprintf "param%d" index) in
          let parameter = Parameter.wrap var in
          Kinded_parameter.create parameter kind)
        ts
    in
    { params;
      env_extension = empty_env_extension;
    }

  let create_blocks ~tag ~size ~field_tys : blocks =
    let fields = create_parameters_from_types field_tys in
    let known_tags_and_sizes =
      Tag_and_size.Map.singleton (Tag_and_size.create tag size) fields
    in
    { known_tags_and_sizes;
      size_at_least_n = Targetint.OCaml.Map.empty;
    }

  let mutable_float_array ~size : t =
    match Targetint.OCaml.to_int_option size with
    | None ->
      (* CR mshinwell: Here and below, this should be a normal compilation
         error, not a fatal error. *)
      Misc.fatal_error "Mutable float array too long for host"
    | Some size ->
      let field_tys = List.init size (fun _index -> any_naked_float ()) in
      let size = Targetint.OCaml.of_int size in
      let blocks = create_blocks ~tag:Tag.double_array_tag ~size ~field_tys in
      let blocks_imms : blocks_and_tagged_immediates =
        { immediates = Known Immediate.Map.empty;
          blocks = Known blocks;
        }
      in
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
      }

  let immutable_float_array fields : t =
    match Targetint.OCaml.of_int_option (Array.length fields) with
    | None ->
      Misc.fatal_error "Immutable float array too long for target"
    | Some size ->
      let field_tys =
        Array.map (fun ty_naked_number : t ->
            { descr =
                Naked_number (ty_naked_number, K.Naked_number.Naked_float);
              phantom = None;
            })
          fields
      in
      let blocks =
        create_blocks ~tag:Tag.double_array_tag ~size
          ~field_tys:(Array.to_list field_tys)
      in
      let blocks_imms : blocks_and_tagged_immediates =
        { immediates = Known Immediate.Map.empty;
          blocks = Known blocks;
        }
      in
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
      }

  let this_immutable_float_array fields : t =
    let make_field f : _ ty_naked_number =
      No_alias (Join [Float (Float_by_bit_pattern.Set.singleton f)])
    in
    let fields = Array.map make_field fields in
    immutable_float_array fields

  let block tag ~(fields : t mutable_or_immutable array) =
    (* CR mshinwell: We should check the field kinds against the tag. *)
    match Targetint.OCaml.of_int_option (Array.length fields) with
    | None ->
      Misc.fatal_error "Block too long for target"
    | Some size ->
      let field_tys =
        Array.map
          (fun (field : _ mutable_or_immutable) : t ->
            match field with
            | Immutable t -> t
            | Mutable -> any_value ())
          fields
      in
      let blocks =
        create_blocks ~tag ~size ~field_tys:(Array.to_list field_tys)
      in
      let blocks_imms : blocks_and_tagged_immediates =
        { immediates = Known Immediate.Map.empty;
          blocks = Known blocks;
        }
      in
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
        phantom = None;
      }

  (* CR mshinwell: bad name *)
  let block_of_values tag ~(fields : ty_value mutable_or_immutable array) =
    let fields =
      Array.map
        (fun (field : _ mutable_or_immutable) : t mutable_or_immutable ->
          match field with
          | Immutable ty_value ->
            Immutable {
              descr = Value ty_value;
            }
          | Mutable -> Mutable)
        fields
    in
    block tag ~fields

  let block_of_unknown_values tag ~size =
    let fields =
      Array.init size (fun _index : _ mutable_or_immutable ->
        Immutable (any_value_as_ty_value ()))
    in
    block_of_values tag ~fields

  let any_boxed_float () = box_float (any_naked_float ())
  let any_boxed_int32 () = box_int32 (any_naked_int32 ())
  let any_boxed_int64 () = box_int64 (any_naked_int64 ())
  let any_boxed_nativeint () = box_nativeint (any_naked_nativeint ())

  let check_of_kind t (expected_kind : K.t) =
    let actual_kind = kind t in
    if not (K.equal actual_kind expected_kind) then begin
      Misc.fatal_errorf "Type has wrong kind: have %a but expected %a"
        K.print actual_kind
        K.print expected_kind
    end

  let bottom_like t = bottom (kind t)
  let unknown_like t = unknown (kind t)

  let create_inlinable_function_declaration ~is_classic_mode ~closure_origin
        ~continuation_param ~exn_continuation_param ~params ~body ~code_id
        ~results ~stub ~dbg ~inline ~specialise ~is_a_functor
        ~invariant_params ~size
        ~direct_call_surrogate ~my_closure : function_declarations =
    Inlinable [({
      closure_origin;
      continuation_param;
      exn_continuation_param;
      is_classic_mode;
      params;
      body;
      code_id;
      free_names_in_body = Expr.free_names body;
      results;
      stub;
      dbg;
      inline;
      specialise;
      is_a_functor;
      invariant_params;
      size;
      direct_call_surrogate;
      my_closure;
    } : inlinable_function_declaration)]

  let create_non_inlinable_function_declaration ~direct_call_surrogate
        : function_declarations =
    let decl : non_inlinable_function_declarations =
      { direct_call_surrogate;
      }
    in
    Non_inlinable decl

  let closure function_decls : ty_fabricated =
    No_alias (Join [Closure { function_decls; }])

  let closures_entry ~set_of_closures : closures_entry =
    { set_of_closures; }

  let closures ty by_closure_id : t =
    { descr = Value (No_alias (Join [Closures { ty; by_closure_id; }]));
    }

  let set_of_closures ~closures ~closure_elements =
    let set_of_closures : set_of_closures =
      { closures;
        closure_elements;
      }
    in
    let no_closures =
      match set_of_closures.closures with
      | Open _ -> false
      | Exactly map -> Closure_id.Map.is_empty map
    in
    let descr : descr =
      if no_closures then
        Fabricated (No_alias (Join []))
      else
        Fabricated (No_alias (Join [Set_of_closures set_of_closures]))
    in
    { descr;
    }

  let apply_name_permutation_unknown_or_join unknown_or_join perm =
    match unknown_or_join with
    | Unknown -> unknown_or_join
    | Join of_kind_foos ->
      let something_changed = ref false in
      let of_kind_foos =
        List.map (fun (of_kind_foo, existing_perm) ->
            let new_perm = Name_permutation.compose existing_perm new_perm in
            if not (new_perm == existing_perm) then begin
              something_changed := true
            end;
            of_kind_foo, new_perm)
          of_kind_foos
      in
      if not !something_changed then unknown_or_join
      else Join of_kind_foos

  let apply_name_permutation_ty ty perm =
    match ty with
    | No_alias unknown_or_join ->
      let unknown_or_join' =
        apply_name_permutation_unknown_or_join unknown_or_join perm
      in
      if unknown_or_join == unknown_or_join' then ty
      else No_alias unknown_or_join'
    | Type _ -> ty
    | Equals simple ->
      let simple' = Name_permutation.apply_simple perm simple in
      if simple == simple' then ty
      else Equals simple'

  let apply_name_permutation_descr descr perm =
    match descr with
    | Value ty_value ->
      let ty_value' = apply_name_permutation_ty ty_value perm in
      if ty_value == ty_value' then descr
      else Value ty_value'
    | Naked_number (ty_naked_number, kind) ->
      let ty_naked_number' = apply_name_permutation_ty ty_naked_number perm in
      if ty_naked_number == ty_naked_number' then descr
      else Naked_number (ty_naked_number', kind)
    | Fabricated ty_fabricated ->
      let ty_fabricated' = apply_name_permutation_ty ty_fabricated perm in
      if ty_fabricated == ty_fabricated' then descr
      else Fabricated ty_fabricated'

  let apply_name_permutation ({ descr; } as t) perm =
    let descr' = apply_name_permutation_descr descr perm in
    if descr == descr' then t
    else { descr = descr'; }

  let get_alias t =
    match t.descr with
    | Value (Equals simple) -> Some simple
    | Value _ -> None
    | Naked_number (Equals simple, _) -> Some simple
    | Naked_number _ -> None
    | Fabricated (Equals simple) -> Some simple
    | Fabricated _ -> None

(*
  (* CR mshinwell: Add comment that this forms an equivalence relation *)
  let function_declarations_compatible
        (decl1 : function_declaration)
        (decl2 : function_declaration) =
    let check (params1 : parameters) (params2 : parameters) =
      let arity1 = Kinded_parameter.arity params1.params in
      let arity2 = Kinded_parameter.arity params2.params in
      Flambda_arity.equal arity1 arity2
        || (Flambda_arity.all_values arity1 && Flambda_arity.all_values arity2)
    in
    check decl1.ty.params decl2.ty.params
      && check decl1.ty.result decl2.ty.result
*)

  module T1 = struct
    include T

    let print = print
    let print_typing_environment_entry = print_typing_environment_entry
    let print_typing_environment = print_typing_environment
    let print_typing_env_extension = print_typing_env_extension
    let print_parameters = print_parameters
    let bottom = bottom
    let alias_type_of = alias_type_of
    let free_names = free_names
    let free_names_set = free_names_set
    let unknown = unknown
    let force_to_kind_value = force_to_kind_value
    let force_to_kind_naked_number = force_to_kind_naked_number
    let force_to_kind_naked_int32 = force_to_kind_naked_int32
    let force_to_kind_naked_int64 = force_to_kind_naked_int64
    let force_to_kind_naked_nativeint = force_to_kind_naked_nativeint
    let force_to_kind_naked_float = force_to_kind_naked_float
    let force_to_kind_naked_immediate = force_to_kind_naked_immediate
    let force_to_kind_fabricated = force_to_kind_fabricated
    let kind = kind
    let is_empty_typing_environment = is_empty_typing_environment
    let rename_variables = rename_variables
    let rename_variables_env_extension = rename_variables_env_extension
    let rename_variables_parameters = rename_variables_parameters
    let print_ty_value = print_ty_value
    let print_ty_naked_number = print_ty_naked_number
    let print_ty_fabricated = print_ty_fabricated
    let ty_is_obviously_bottom = ty_is_obviously_bottom
    let any_value_as_ty_value = any_value_as_ty_value
    let any_fabricated_as_ty_fabricated = any_fabricated_as_ty_fabricated
    let bottom_as_ty_value = bottom_as_ty_value
    let bottom_as_ty_fabricated = bottom_as_ty_fabricated
    let is_obviously_bottom = is_obviously_bottom
    let get_alias = get_alias
    let create_parameters_from_types = create_parameters_from_types
  end

  module rec Make_meet_or_join : functor
    (E : Either_meet_or_join_intf.S with module T := T1)
      ->
      sig
        module Meet_and_join : Meet_and_join_intf.S_for_types
          with module T := T1
      end
  = functor
    (E : Either_meet_or_join_intf.S with module T := T1)
  -> struct
    (* CR mshinwell: Work out which properties we need to prove, e.g.
       Distributivity of meet over join:
         X n (X' u Y') == (X n X') u (X n Y'). *)
    module rec Make_meet_and_join : functor
      (S : Meet_and_join_spec_intf.S with module T := T1)
        ->
        Meet_and_join_intf.S
          with module T := T1
          with type of_kind_foo = S.of_kind_foo
      =
    functor (S : Meet_and_join_spec_intf.S with module T := T1) ->
    struct
      type of_kind_foo = S.of_kind_foo

      let unknown_or_join_is_bottom (uj : _ unknown_or_join) =
        match uj with
        | Join [] -> true
        | Unknown | Join _ -> false

      let unknown_or_join_is_unknown (uj : _ unknown_or_join) =
        match uj with
        | Join _ -> false
        | Unknown -> true

      let rec join_on_unknown_or_join env
            (uj1 : S.of_kind_foo unknown_or_join)
            (uj2 : S.of_kind_foo unknown_or_join)
            : S.of_kind_foo unknown_or_join =
        if uj1 == uj2 then uj1
        else
          match uj1, uj2 with
          | Unknown, _ | _, Unknown -> Unknown
          | Join [], Join [] -> Join []
          | Join of_kind_foos1, Join of_kind_foos2 ->
            (* We rely on the invariant in flambda_type0_intf.ml.
               Everything in [of_kind_foos1] is mutually incompatible with each
               other; likewise in [of_kind_foos2]. *)
            let of_kind_foos =
              List.fold_left (fun of_kind_foos (of_kind_foo, perm) ->
                  (* [of_kind_foo] can be compatible with at most one of the
                     elements of [of_kind_foos]. *)
                  let found_one = ref false in
                  let joined =
                    List.map (fun (of_kind_foo', perm') ->
                        let join =
                          (* N.B. If we are here, [S.meet_or_join_of_kind_foo]
                             must be a "join" operation. *)
                          S.meet_or_join_of_kind_foo env perm perm'
                            of_kind_foo of_kind_foo'
                        in
                        match join with
                        | Ok (of_kind_foo, _env_extension) ->
                          if !found_one then begin
                            (* CR mshinwell: Add detail showing what was
                               wrong. *)
                            Misc.fatal_errorf "Invariant broken for [Join]"
                          end;
                          found_one := true;
                          of_kind_foo, Name_permutation.create ()
                        | Absorbing -> of_kind_foo', perm')
                      of_kind_foos
                  in
                  if not !found_one then (of_kind_foo, perm) :: of_kind_foos
                  else joined)
                of_kind_foos2
                of_kind_foos1
            in
            Join of_kind_foos

      and join_ty env
            (or_alias1 : S.of_kind_foo ty) (or_alias2 : S.of_kind_foo ty)
            : S.of_kind_foo ty =
        if Join_env.fast_check_extensions_same_both_sides env
          && or_alias1 == or_alias2
        then or_alias1
        else
          let unknown_or_join1, canonical_simple1 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
              (Join_env.environment_on_left env)
              ~force_to_kind:S.force_to_kind
              ~print_ty:S.print_ty
              or_alias1
          in
          let unknown_or_join2, canonical_simple2 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
              (Join_env.environment_on_right env)
              ~force_to_kind:S.force_to_kind
              ~print_ty:S.print_ty
              or_alias2
          in
          let all_aliases1 =
            match canonical_simple1 with
            | None -> Name.Set.empty
            | Some canonical_simple ->
              Typing_env.aliases_of_simple (Join_env.environment_on_left env)
                canonical_simple
          in
          let all_aliases2 =
            match canonical_simple2 with
            | None -> Name.Set.empty
            | Some canonical_simple ->
              Typing_env.aliases_of_simple (Join_env.environment_on_right env)
                canonical_simple
          in
          let all_aliases = Name.Set.inter all_aliases1 all_aliases2 in
          let alias_both_sides = Name.Set.choose_opt all_aliases in
          match alias_both_sides with
          | Some name -> Equals (Simple.name name)
          | None ->
            let alias1 = Name.Set.choose_opt all_aliases1 in
            let alias2 = Name.Set.choose_opt all_aliases2 in
            match alias1, alias2 with
            | Some name1, _ when unknown_or_join_is_bottom unknown_or_join2 ->
              Equals (Simple.name name1)
            | _, Some name2 when unknown_or_join_is_bottom unknown_or_join1 ->
              Equals (Simple.name name2)
            | None, None ->
              let unknown_or_join =
                join_on_unknown_or_join env perm1 perm2
                  unknown_or_join1 unknown_or_join2
              in
              if unknown_or_join == unknown_or_join1 then begin
                assert (match or_alias1 with No_alias _ -> true | _ -> false);
                or_alias1
              end else if unknown_or_join == unknown_or_join2 then begin
                assert (match or_alias2 with No_alias _ -> true | _ -> false);
                or_alias2
              end else begin
                No_alias unknown_or_join
              end
            | _, _ ->
              let unknown_or_join =
                join_on_unknown_or_join env perm1 perm2
                  unknown_or_join1 unknown_or_join2
              in
              No_alias unknown_or_join

      let rec meet_on_unknown_or_join env perm1 perm2
            (ou1 : S.of_kind_foo unknown_or_join)
            (ou2 : S.of_kind_foo unknown_or_join)
            : S.of_kind_foo unknown_or_join * env_extension =
        if ou1 == ou2 then
          ou1, Typing_env_extension.empty
        else
          match ou1, ou2 with
          | Unknown, ou2 -> ou2, Typing_env_extension.empty
          | ou1, Unknown -> ou1, Typing_env_extension.empty
          | Join of_kind_foos1, Join of_kind_foos2 ->
            let of_kind_foos, env_extension_from_meet =
              List.fold_left
                (fun (of_kind_foos, env_extension_from_meet) of_kind_foo ->
                  let new_env_extension_from_meet =
                    ref (Typing_env_extension.empty)
                  in
                  let of_kind_foos =
                    Misc.Stdlib.List.filter_map (fun of_kind_foo' ->
                        let meet =
                          let env = Join_env.create env in
                          S.meet_or_join_of_kind_foo env perm1 perm2
                            of_kind_foo of_kind_foo'
                        in
                        match meet with
                        | Ok (of_kind_foo, new_env_extension_from_meet') ->
                          new_env_extension_from_meet :=
                            Typing_env_extension.meet env
                              new_env_extension_from_meet'
                                !new_env_extension_from_meet;
                          Some of_kind_foo
                        | Absorbing -> None)
                      of_kind_foos
                  in
                  let env_extension_from_meet =
                    Typing_env_extension.meet env
                      env_extension_from_meet !new_env_extension_from_meet;
                  in
                  of_kind_foos, env_extension_from_meet)
                (of_kind_foos2, Typing_env_extension.empty)
                of_kind_foos1
            in
            let same_as input_of_kind_foos =
              List.compare_lengths input_of_kind_foos of_kind_foos = 0
                && List.for_all2 (fun input_of_kind_foo of_kind_foo ->
                       input_of_kind_foo == of_kind_foo)
                     input_of_kind_foos of_kind_foos
            in
            if same_as of_kind_foos1 then ou1, env_extension_from_meet
            else if same_as of_kind_foos2 then ou2, env_extension_from_meet
            else Join of_kind_foos, env_extension_from_meet

      and meet_ty env perm1 perm2
            (or_alias1 : S.of_kind_foo ty)
            (or_alias2 : S.of_kind_foo ty)
            : S.of_kind_foo ty * env_extension =
        if or_alias1 == or_alias2 then begin
          or_alias1, Typing_env_extension.empty
        end else begin
          let unknown_or_join1, canonical_simple1 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty' env
              ~force_to_kind:S.force_to_kind
              ~print_ty:S.print_ty
              or_alias1
          in
          let unknown_or_join2, canonical_simple2 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty' env
              ~force_to_kind:S.force_to_kind
              ~print_ty:S.print_ty
              or_alias2
          in
          let add_equation_if_on_a_name env_extension (simple : Simple.t) ty =
            match simple with
            | Name name ->
              Typing_env_extension.add_equation env_extension name ty
            | Const _ | Discriminant _ -> env_extension
          in
          match canonical_simple1, canonical_simple2 with
          | Some simple1, Some simple2 when Simple.equal simple1 simple2 ->
            Equals simple1, Typing_env_extension.empty
          | Some simple1, _ when unknown_or_join_is_unknown unknown_or_join2 ->
            Equals simple1, Typing_env_extension.empty
          | _, Some simple2 when unknown_or_join_is_unknown unknown_or_join1 ->
            Equals simple2, Typing_env_extension.empty
          | Some simple1, Some simple2 ->
            let meet_unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env perm1 perm2
                unknown_or_join1 unknown_or_join2
            in
            let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple1 meet_ty
            in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple2 (S.to_type (Equals simple1))
            in
            Equals simple1, env_extension_from_meet
          | Some simple1, None ->
            let meet_unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env perm1 perm2
                unknown_or_join1 unknown_or_join2
            in
            let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple1 meet_ty
            in
            Equals simple1, env_extension_from_meet
          | None, Some simple2 ->
            let meet_unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env perm1 perm2
                unknown_or_join1 unknown_or_join2
            in
            let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple2 meet_ty
            in
            Equals simple2, env_extension_from_meet
          | None, None ->
            let unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env perm1 perm2
                unknown_or_join1 unknown_or_join2
            in
            if unknown_or_join == unknown_or_join1 then begin
              assert (match or_alias1 with No_alias _ -> true | _ -> false);
              or_alias1, env_extension_from_meet
            end else if unknown_or_join == unknown_or_join2 then begin
              assert (match or_alias2 with No_alias _ -> true | _ -> false);
              or_alias2, env_extension_from_meet
            end else begin
              No_alias unknown_or_join, env_extension_from_meet
            end
        end

      let meet_or_join_ty env or_alias1 or_alias2 =
        E.switch meet_ty join_ty env or_alias1 or_alias2
    end and Meet_and_join : sig
      include Meet_and_join_intf.S_for_types with module T := T1
    end = struct
      let meet_or_join env t1 t2 : t * env_extension =
        if Join_env.fast_check_extensions_same_both_sides env
          && Type_equality.fast_equal t1 t2
        then t1, Typing_env_extension.empty
        else begin
          Join_env.invariant env;
          ensure_phantomness_matches t1 t2
            (Printf.sprintf "kind mismatch upon %s" E.name);
          let descr, equations =
            match t1.descr, t2.descr with
            | Value ty_value1, Value ty_value2 ->
              let ty_value, equations =
                Meet_and_join_value.meet_or_join_ty env perm1 perm2
                  ty_value1 ty_value2
              in
              if ty_value == ty_value1 then t1.descr, equations
              else if ty_value == ty_value2 then t2.descr, equations
              else Value ty_value, equations
            | Naked_number (ty_naked_number1, kind1),
                Naked_number (ty_naked_number2, kind2) ->
              let module N = K.Naked_number in
              begin match kind1, kind2 with
              | N.Naked_immediate, N.Naked_immediate ->
                let ty_naked_number, equations =
                  Meet_and_join_naked_immediate.meet_or_join_ty env perm1 perm2
                    ty_naked_number1 ty_naked_number2
                in
                Naked_number (ty_naked_number, N.Naked_immediate), equations
              | N.Naked_float, N.Naked_float ->
                let ty_naked_number, equations =
                  Meet_and_join_naked_float.meet_or_join_ty env perm1 perm2
                    ty_naked_number1 ty_naked_number2
                in
                Naked_number (ty_naked_number, N.Naked_float), equations
              | N.Naked_int32, N.Naked_int32 ->
                let ty_naked_number, equations =
                  Meet_and_join_naked_int32.meet_or_join_ty env perm1 perm2
                    ty_naked_number1 ty_naked_number2
                in
                Naked_number (ty_naked_number, N.Naked_int32), equations
              | N.Naked_int64, N.Naked_int64 ->
                let ty_naked_number, equations =
                  Meet_and_join_naked_int64.meet_or_join_ty env perm1 perm2
                    ty_naked_number1 ty_naked_number2
                in
                Naked_number (ty_naked_number, N.Naked_int64), equations
              | N.Naked_nativeint, N.Naked_nativeint ->
                let ty_naked_number, equations =
                  Meet_and_join_naked_nativeint.meet_or_join_ty env perm1 perm2
                    ty_naked_number1 ty_naked_number2
                in
                Naked_number (ty_naked_number, N.Naked_nativeint), equations
              | _, _ ->
                Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
                  E.name
                  print t1
                  print t2
              end
            | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
              let ty_fabricated, equations =
                Meet_and_join_fabricated.meet_or_join_ty env perm1 perm2
                  ty_fabricated1 ty_fabricated2
              in
              if ty_fabricated == ty_fabricated1 then
                t1.descr, equations
              else if ty_fabricated == ty_fabricated2 then
                t2.descr, equations
              else
                Fabricated ty_fabricated, equations
            | (Value _ | Naked_number _ | Fabricated _), _ ->
              Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
                E.name
                print t1
                print t2
          in
          let t =
            if t1.descr == descr then t1
            else if t2.descr == descr then t2
            else {
              descr;
            }
          in
          t, equations
        end
    end and Meet_and_join_value :
      Meet_and_join_intf.S
        with module T := T1
        with type of_kind_foo = of_kind_value
      = Outer_namespace.Meet_and_join_value.Make (T1)
          (Make_meet_and_join) (Meet_and_join_naked_immediate)
          (Meet_and_join_naked_float) (Meet_and_join_naked_int32)
          (Meet_and_join_naked_int64) (Meet_and_join_naked_nativeint)
          (Meet_and_join_fabricated) (Both_meet_and_join)
          (Typing_env) (Typing_env_extension) (Join_env) (Parameters) (E)
    and Meet_and_join_naked_number : sig
      (* CR mshinwell: Deal with this signature somehow *)
      module Naked_immediate :
        Meet_and_join_intf.S
          with module T := T1
          with type of_kind_foo = Immediate.Set.t of_kind_naked_number
      module Naked_float :
        Meet_and_join_intf.S
          with module T := T1
          with type of_kind_foo =
            Numbers.Float_by_bit_pattern.Set.t of_kind_naked_number
      module Naked_int32 :
        Meet_and_join_intf.S
          with module T := T1
          with type of_kind_foo = Numbers.Int32.Set.t of_kind_naked_number
      module Naked_int64 :
        Meet_and_join_intf.S
          with module T := T1
          with type of_kind_foo = Numbers.Int64.Set.t of_kind_naked_number
      module Naked_nativeint :
        Meet_and_join_intf.S
          with module T := T1
          with type of_kind_foo = Targetint.Set.t of_kind_naked_number
    end = Outer_namespace.Meet_and_join_naked_number.Make
      (T1) (Make_meet_and_join) (Typing_env) (Typing_env_extension) (E)
    and Meet_and_join_naked_immediate :
      Meet_and_join_intf.S
        with module T := T1
        with type of_kind_foo = Immediate.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_immediate
    and Meet_and_join_naked_float :
      (* CR mshinwell: See if we can abstract these naked number cases some
         more? *)
      Meet_and_join_intf.S
        with module T := T1
        with type of_kind_foo = Float_by_bit_pattern.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_float
    and Meet_and_join_naked_int32 :
      Meet_and_join_intf.S
        with module T := T1
        with type of_kind_foo = Int32.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_int32
    and Meet_and_join_naked_int64 :
      Meet_and_join_intf.S
        with module T := T1
        with type of_kind_foo = Int64.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_int64
    and Meet_and_join_naked_nativeint :
      Meet_and_join_intf.S
        with module T := T1
        with type of_kind_foo = Targetint.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_nativeint
    and Meet_and_join_fabricated :
      Meet_and_join_intf.S
        with module T := T1
        with type of_kind_foo = of_kind_fabricated
      = Outer_namespace.Meet_and_join_fabricated.Make
          (T1) (Make_meet_and_join) (Meet_and_join_value) (Both_meet_and_join)
          (Typing_env) (Typing_env_extension) (Join_env) (E)
  end and Meet : sig
    module Meet_and_join : Meet_and_join_intf.S_for_types with module T := T1
  end = Make_meet_or_join (For_meet)
  and Join : sig
    module Meet_and_join : Meet_and_join_intf.S_for_types with module T := T1
  end = Make_meet_or_join (For_join)
  and Both_meet_and_join : Meet_and_join_intf.S_both with module T := T1
    = struct
      module T = T1

      let meet env t1 t2 =
        Meet.Meet_and_join.meet_or_join (Join_env.create env) t1 t2

      let join env t1 t2 =
        let join_ty, _env_extension =
          Join.Meet_and_join.meet_or_join env t1 t2
        in
        join_ty

      let as_or_more_precise env t1 ~than:t2 =
        if Type_equality.fast_equal t1 t2 then true
        else
          let meet_t, _env_extension = meet env t1 t2 in
          Type_equality.equal meet_t t1

      let strictly_more_precise env t1 ~than:t2 =
        if Type_equality.fast_equal t1 t2 then false
        else
          let meet_t, _env_extension = meet env t1 t2 in
          Type_equality.equal meet_t t1
            && not (Type_equality.equal meet_t t2)
    end
  and Typing_env :
    Typing_env_intf.S with module T := T1
      = Outer_namespace.Typing_environment0.Make (T1)
          (Typing_env_extension) (Both_meet_and_join) (Type_equality)
  and Typing_env_extension :
    Typing_env_extension_intf.S with module T := T1
      = Outer_namespace.Typing_env_extension.Make (T1)
          (Typing_env) (Both_meet_and_join) (Type_equality) (Join_env)
  and Type_equality :
    Type_equality_intf.S with module T := T1
      = Outer_namespace.Type_equality.Make (T1) (Expr) (Typing_env_extension)
  and Join_env :
    Join_env_intf.S with module T := T1
    = Outer_namespace.Join_env.Make (T1) (Typing_env) (Typing_env_extension)
  and Parameters :
    Parameters_intf.S with module T := T1
    = Outer_namespace.Parameters.Make (T1) (Typing_env) (Typing_env_extension)
        (Both_meet_and_join) (Join_env)
  and For_meet : Either_meet_or_join_intf.S with module T := T1
  = struct
    let name = "meet"

    type meet_or_join = Meet | Join

    let op : meet_or_join = Meet
    let (_ : meet_or_join) = Join

    let unknown_is_identity = true
    let unknown_is_absorbing = false

    (* CR mshinwell: Write functors to generate these patterns *)
    module Immediate = struct
      module Set = struct
        type t = Immediate.Set.t
        let union_or_inter = Immediate.Set.inter
      end

      module Map = struct
        type 'a t = 'a Immediate.Map.t

        let union_or_inter = Immediate.Map.inter
      end
    end

    module Float_by_bit_pattern = struct
      module Set = struct
        type t = Float_by_bit_pattern.Set.t
        let union_or_inter = Float_by_bit_pattern.Set.inter
      end
    end

    module Int32 = struct
      module Set = struct
        type t = Int32.Set.t
        let union_or_inter = Int32.Set.inter
      end
    end

    module Int64 = struct
      module Set = struct
        type t = Int64.Set.t
        let union_or_inter = Int64.Set.inter
      end
    end

    module Targetint = struct
      module Set = struct
        type t = Targetint.Set.t
        let union_or_inter = Targetint.Set.inter
      end

      module OCaml = struct
        module Map = struct
          type 'a t = 'a Targetint.OCaml.Map.t

          let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
                t1 t2 =
            Targetint.OCaml.Map.inter in_both t1 t2
        end
      end
    end

    module Closure_id = struct
      module Map = struct
        type 'a t = 'a Closure_id.Map.t

        let union_or_inter = Closure_id.Map.inter

        (* CR mshinwell: implement these *)
        let union_or_inter_and_left _f _t1 _t2 = assert false
      end
    end

    module Var_within_closure = struct
      module Map = struct
        type 'a t = 'a Var_within_closure.Map.t

        let union_or_inter = Var_within_closure.Map.inter

        let union_or_inter_and_left _f _t1 _t2 = assert false
      end
    end

    module Tag = struct
      module Map = struct
        type 'a t = 'a Tag.Map.t

        let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
              t1 t2 =
          Tag.Map.inter in_both t1 t2
      end
    end

    module Discriminant = struct
      module Map = struct
        type 'a t = 'a Discriminant.Map.t

        let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
              t1 t2 =
          Discriminant.Map.inter in_both t1 t2
      end
    end

    let switch meet _join join_env thing1 thing2 =
      meet (Join_env.central_environment join_env) thing1 thing2

    let switch' meet _join join_env thing1 thing2 =
      meet (Join_env.central_environment join_env) thing1 thing2
  end and For_join : Either_meet_or_join_intf.S with module T := T1
  = struct
    let name = "join"

    type meet_or_join = Meet | Join

    let op : meet_or_join = Join
    let (_ : meet_or_join) = Meet

    let unknown_is_identity = false
    let unknown_is_absorbing = true

    module Immediate = struct
      module Set = struct
        type t = Immediate.Set.t
        let union_or_inter = Immediate.Set.union
      end

      module Map = struct
        type 'a t = 'a Immediate.Map.t

        let union_or_inter = Immediate.Map.union
      end
    end

    module Float_by_bit_pattern = struct
      module Set = struct
        type t = Float_by_bit_pattern.Set.t
        let union_or_inter = Float_by_bit_pattern.Set.union
      end
    end

    module Int32 = struct
      module Set = struct
        type t = Int32.Set.t
        let union_or_inter = Int32.Set.union
      end
    end

    module Int64 = struct
      module Set = struct
        type t = Int64.Set.t
        let union_or_inter = Int64.Set.union
      end
    end

    module Targetint = struct
      module Set = struct
        type t = Targetint.Set.t
        let union_or_inter = Targetint.Set.union
      end

      module OCaml = struct
        module Map = struct
          type 'a t = 'a Targetint.OCaml.Map.t

          let union_or_inter_both = Targetint.OCaml.Map.union_both
        end
      end
    end

    module Closure_id = struct
      module Map = struct
        type 'a t = 'a Closure_id.Map.t

        let union_or_inter = Closure_id.Map.union

        let union_or_inter_and_left f t1 t2 =
          Closure_id.Map.union f t1 t2
      end
    end

    module Var_within_closure = struct
      module Map = struct
        type 'a t = 'a Var_within_closure.Map.t

        let union_or_inter = Var_within_closure.Map.union

        let union_or_inter_and_left f t1 t2 =
          Var_within_closure.Map.union f t1 t2
      end
    end

    module Tag = struct
      module Map = struct
        type 'a t = 'a Tag.Map.t

        let union_or_inter_both = Tag.Map.union_both
      end
    end

    module Discriminant = struct
      module Map = struct
        type 'a t = 'a Discriminant.Map.t

        let union_or_inter_both = Discriminant.Map.union_both
      end
    end

    let switch _meet join join_env thing1 thing2 =
      join join_env thing1 thing2, Typing_env_extension.empty

    let switch' _meet join join_env thing1 thing2 =
      join join_env thing1 thing2
  end

  let meet = Both_meet_and_join.meet
  let join = Both_meet_and_join.join
  let as_or_more_precise = Both_meet_and_join.as_or_more_precise
  let strictly_more_precise = Both_meet_and_join.strictly_more_precise
  let fast_equal = Type_equality.fast_equal
  let equal = Type_equality.equal

  module T2 = struct
    type nonrec typing_environment = typing_environment
    type nonrec typing_environment_entry = typing_environment_entry
    type nonrec typing_environment_entry0 = typing_environment_entry0
    type nonrec env_extension = env_extension
    type nonrec flambda_type = flambda_type
    type nonrec t_in_context = t_in_context
    type nonrec join_env = join_env
    type nonrec parameters = parameters
    type nonrec 'a ty = 'a ty
    type nonrec 'a unknown_or_join = 'a unknown_or_join
  end
end
