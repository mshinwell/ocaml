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

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Flambda_types = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  let print_or_alias print_descr ppf (or_alias : _ Flambda_types.or_alias) =
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

  let print_unknown_or_join print_contents ppf
        (o : _ Flambda_types.unknown_or_join) =
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

  let print_of_kind_naked_number (type n) ppf
        ((n : n Flambda_types.of_kind_naked_number), _perm) =
    match n with
    | Immediate i ->
      Format.fprintf ppf "@[(Naked_immediates@ (%a))@]"
        Immediate.Set.print i
    | Float f ->
      Format.fprintf ppf "@[(Naked_floats@ (%a))@]"
        Numbers.Float_by_bit_pattern.Set.print f
    | Int32 i ->
      Format.fprintf ppf "@[(Naked_int32s@ (%a))@]"
        Numbers.Int32.Set.print i
    | Int64 i ->
      Format.fprintf ppf "@[(Naked_int64s@ (%a))@]"
        Numbers.Int64.Set.print i
    | Nativeint i ->
      Format.fprintf ppf "@[(Naked_nativeints@ (%a))@]"
        Targetint.Set.print i

  let print_ty_naked_number (type n) ppf
        (ty : n Flambda_types.ty_naked_number) =
    print_ty_generic print_of_kind_naked_number ppf ty

  let print_of_kind_value_boxed_number (type n)
        ppf (n : n Flambda_types.of_kind_value_boxed_number) =
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

  let rec print_of_kind_value ~cache ppf
          ((of_kind_value : Flambda_types.of_kind_value), _) =
    match of_kind_value with
    | Blocks_and_tagged_immediates { blocks; immediates; } ->
      (* CR mshinwell: Improve so that we elide blocks and/or immediates when
         they're empty.  Similarly we can elide the extensions when empty. *)
      Format.fprintf ppf
        "@[<hov 1>(Blocks_and_immediates@ \
          @[<hov 1>(blocks@ %a)@]@ \
          @[<hov 1>(immediates@ %a)@])@]"
        (Blocks.print_with_cache ~cache) blocks
        (Immediates.print ~cache) immediates
    | Boxed_number n ->
      Format.fprintf ppf "@[(Boxed_number %a)@]"
        print_of_kind_value_boxed_number n
    | Closures { by_closure_id; } ->
      Closures_entry_by_closure_id.print ~cache ppf by_closure_id
    | String str_infos ->
      Format.fprintf ppf "@[(Strings (%a))@]" String_info.Set.print str_infos

  and print_ty_value_with_cache ~cache ppf (ty : Flambda_types.ty_value) =
    print_ty_generic (print_of_kind_value ~cache) ppf ty

  and print_ty_value ppf (ty : Flambda_types.ty_value) =
    print_ty_value_with_cache ~cache:(Printing_cache.create ()) ppf ty

  and print_ty_value_array ~cache ppf ty_values =
    Format.fprintf ppf "@[[| %a |]@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
        (print_ty_value_with_cache ~cache))
      (Array.to_list ty_values)

  and print_inlinable_function_declaration_with_cache ~cache ppf
        (({ closure_origin;
           continuation_param;
           exn_continuation_param;
           is_classic_mode;
           params;
           body;
           code_id;
           free_names_in_body;
           stub;
           result_arity;
           dbg;
           inline;
           specialise;
           is_a_functor;
           invariant_params;
           size;
           direct_call_surrogate;
           my_closure;
         } : Flambda_types.inlinable_function_declaration) as decl) =
    Printing_cache.with_cache cache ppf "inlinable_fundecl" decl (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(Inlinable@ \
          @[<hov 1>(closure_origin@ %a)@]@ \
          @[<hov 1>(continuation_param@ %a)@]@ \
          @[<hov 1>(exn_continuation_param@ %a)@]@ \
          @[<hov 1>(is_classic_mode@ %b)@]@ \
          @[<hov 1>(params@ %a)@]@ \
          @[<hov 1>(body@ %a)@]@ \
          @[<hov 1>(free_names_in_body@ %a)@]@ \
          @[<hov 1>(stub@ %b)@]@ \
          @[<hov 1>(result_arity@ %b)@]@ \
          @[<hov 1>(dbg@ %a)@]@ \
          @[<hov 1>(inline@ %a)@]@ \
          @[<hov 1>(specialise@ %a)@]@ \
          @[<hov 1>(is_a_functor@ %b)@]@ \
          @[<hov 1>(invariant_params@ %a)@]@ \
          @[<hov 1>(size@ %a)@]@ \
          @[<hov 1>(direct_call_surrogate@ %a)@]@ \
          @[<hov 1>(my_closure@ %a)@])@]"
        Closure_origin.print closure_origin
        Continuation.print continuation_param
        Continuation.print exn_continuation_param
        is_classic_mode
        Kinded_parameter.List.print params
        (Expr.print_with_cache ~cache) body
        Name_occurrences.print free_names_in_body
        stub
        Flambda_arity.print result_arity
        Debuginfo.print_compact dbg
        Inline_attribute.print inline
        Specialise_attribute.print specialise
        is_a_functor
        Variable.Set.print (Lazy.force invariant_params)
        (Misc.Stdlib.Option.print Format.pp_print_int) (Lazy.force size)
        (Misc.Stdlib.Option.print Closure_id.print) direct_call_surrogate
        Variable.print my_closure)

  and print_inlinable_function_declaration ppf decl =
    print_inlinable_function_declaration_with_cache
      ~cache:(Printing_cache.create ())
      ppf decl

  and print_function_declarations ~cache ppf
        (decl : function_declarations) =
    match decl with
    | Inlinable decl ->
      print_inlinable_function_declaration_with_cache ~cache ppf decl
    | Non_inlinable -> Format.pp_print_string ppf "Non_inlinable"

  and print_of_kind_fabricated ~cache ppf
        ((o : Flambda_types.of_kind_fabricated), _) =
    match o with
    | Discriminant discriminants ->
      Format.fprintf ppf "@[<hov 1>(Discriminants@ %a)@]"
        Discriminants.print discriminants
    | Set_of_closures { closures; } ->
      Closure_ids.print ~cache ppf closures

  and print_ty_fabricated_with_cache ~cache ppf
        (ty : Flambda_types.ty_fabricated) =
    print_ty_generic (print_of_kind_fabricated ~cache) ppf ty

  and print_ty_fabricated ppf (ty : Flambda_types.ty_fabricated) =
    print_ty_fabricated_with_cache ~cache:(Printing_cache.create ()) ppf ty

  and print_with_cache ~cache ppf (t : Flambda_types.t) =
    match t with
    | Value ty ->
      Format.fprintf ppf "@[<hov 1>(Val@ %a)@]"
        (print_ty_value_with_cache ~cache) ty
    | Naked_number (ty, _kind) ->
      Format.fprintf ppf "@[<hov 1>(Naked@ %a)@]" print_ty_naked_number ty
    | Fabricated ty ->
      Format.fprintf ppf "@[<hov 1>(Fab@ %a)@]"
        (print_ty_fabricated_with_cache ~cache) ty

  and print ppf (t : t) =
    let cache : Printing_cache.t = Printing_cache.create () in
    print_with_cache ~cache ppf t
end
