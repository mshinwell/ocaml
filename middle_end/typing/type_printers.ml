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
end
