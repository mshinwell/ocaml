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

let print_or_alias print_descr ppf (or_alias : _ Type_grammar.or_alias) =
  match or_alias with
  | No_alias descr -> print_descr ppf descr
  | Equals simple ->
    Format.fprintf ppf "@[(%s=%s %a)@]"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Simple.print simple
  | Type export_id ->
    Format.fprintf ppf "@[(%s=export_id%s %a)@]"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Export_id.print export_id

let unicode = true  (* CR mshinwell: move elsewhere *)

let print_unknown_or_join print_contents ppf
      (o : _ Type_grammar.unknown_or_join) =
  let colour = Flambda_colours.error () in
  match o with
  | Unknown ->
    if unicode then
      Format.fprintf ppf "%s\u{22a4}%s" colour (Flambda_colours.normal ())
    else
      Format.fprintf ppf "%sT%s" colour (Flambda_colours.normal ())
  | Bottom ->
    if unicode then
      Format.fprintf ppf "%s\u{22a5}%s" colour (Flambda_colours.normal ())
    else
      Format.fprintf ppf "%s_|_%s" colour (Flambda_colours.normal ())
  | Ok contents -> print_contents ppf contents

let print_ty_generic print_contents ppf ty =
  (print_or_alias (print_unknown_or_join print_contents)) ppf ty

let print_of_kind_naked_number (type n) ppf
      (n : n Type_grammar.of_kind_naked_number) =
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
      (ty : n Type_grammar.ty_naked_number) =
  print_ty_generic print_of_kind_naked_number ppf ty

let _print_ty_naked_immediate_with_cache ~cache:_ ppf ty =
  print_ty_generic (print_of_kind_naked_number) ppf ty

let print_ty_naked_int32_with_cache ~cache:_ ppf ty =
  print_ty_generic (print_of_kind_naked_number) ppf ty

let print_ty_naked_int64_with_cache ~cache:_ ppf ty =
  print_ty_generic (print_of_kind_naked_number) ppf ty

let print_ty_naked_nativeint_with_cache ~cache:_ ppf ty =
  print_ty_generic (print_of_kind_naked_number) ppf ty

let print_ty_naked_float_with_cache ~cache:_ ppf ty =
  print_ty_generic (print_of_kind_naked_number) ppf ty

let print_ty_naked_immediate_with_cache ~cache:_ ppf ty =
  print_ty_generic (print_of_kind_naked_number) ppf ty

let print_ty_naked_int32 ppf ty =
  print_ty_naked_int32_with_cache ~cache:(Printing_cache.create ())
    ppf ty

let print_ty_naked_int64 ppf ty =
  print_ty_naked_int64_with_cache ~cache:(Printing_cache.create ())
    ppf ty

let print_ty_naked_nativeint ppf ty =
  print_ty_naked_nativeint_with_cache ~cache:(Printing_cache.create ())
    ppf ty

let print_ty_naked_float ppf ty =
  print_ty_naked_float_with_cache ~cache:(Printing_cache.create ())
    ppf ty

let _print_ty_naked_immediate ppf ty =
  print_ty_naked_immediate_with_cache ~cache:(Printing_cache.create ())
    ppf ty

let print_of_kind_value_boxed_number (type n)
      ppf (n : n Type_grammar.of_kind_value_boxed_number) =
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
        (of_kind_value : Type_grammar.of_kind_value) =
  match of_kind_value with
  | Blocks_and_tagged_immediates { blocks; immediates; } ->
    (* CR mshinwell: Improve so that we elide blocks and/or immediates when
       they're empty. *)
    Format.fprintf ppf
      "@[<hov 1>(Blocks_and_immediates@ \
        @[<hov 1>(blocks@ %a)@]@ \
        @[<hov 1>(immediates@ %a)@]\
        )@]"
      (Or_unknown.print (Blocks.print_with_cache ~cache)) blocks
      (Or_unknown.print (Immediates.print_with_cache ~cache)) immediates
  | Boxed_number n ->
    Format.fprintf ppf "@[<hov 1>(Boxed_number@ %a)@]"
      print_of_kind_value_boxed_number n
  | Closures { by_closure_id; } ->
    Closures_entry_by_set_of_closures_contents.print_with_cache ~cache
      ppf by_closure_id
  | String str_infos ->
    Format.fprintf ppf "@[<hov 1>(Strings@ (%a))@]"
      String_info.Set.print str_infos
  | Array { length; } ->
    Format.fprintf ppf "@[<hov 1>(Array@ (length@ %a))@]"
      (print_ty_value_with_cache ~cache) length

and print_ty_value_with_cache ~cache ppf (ty : Type_grammar.ty_value) =
  print_ty_generic (print_of_kind_value ~cache) ppf ty

and print_inlinable_function_declaration_with_cache ~cache ppf
      (({ function_decl; rec_info;
        } : Type_grammar.inlinable_function_declaration) as decl) =
  Printing_cache.with_cache cache ppf "inlinable_fundecl" decl
    (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(Inlinable@ \
          @[<hov 1>(function_decl@ %a)@]@ \
          @[<hov 1>(rec_info@ %a)@]\
          )@]"
        Term_language_function_declaration.print_compact function_decl
        Rec_info.print rec_info)

and print_function_declaration_with_cache ~cache ppf
      (decl : Type_grammar.function_declaration) =
  match decl with
  | Inlinable decl ->
    print_inlinable_function_declaration_with_cache ~cache ppf decl
  | Non_inlinable { param_arity; result_arity; recursive; } ->
    Format.fprintf ppf
      "@[<hov 1>(Non_inlinable@ \
       @[<hov 1>(param_arity@ %a)@]@ \
       @[<hov 1>(result_arity@ %a)@] \
       @[<hov 1>(recursive@ %a)@]\
       )@]"
      Flambda_arity.print param_arity
      Flambda_arity.print result_arity
      Recursive.print recursive

and print_of_kind_fabricated ~cache ppf
      (o : Type_grammar.of_kind_fabricated) =
  match o with
  | Discriminants discriminants ->
    Format.fprintf ppf "@[<hov 1>(Discriminants@ %a)@]"
      (Discriminants.print_with_cache ~cache) discriminants

and print_ty_fabricated_with_cache ~cache ppf
      (ty : Type_grammar.ty_fabricated) =
  print_ty_generic (print_of_kind_fabricated ~cache) ppf ty

and print_with_cache ~cache ppf (t : Type_grammar.t) =
  match t with
  | Value ty ->
    Format.fprintf ppf "@[<hov 1>(Val@ %a)@]"
      (print_ty_value_with_cache ~cache) ty
  | Naked_number (ty, _kind) ->
    Format.fprintf ppf "@[<hov 1>(Naked@ %a)@]" print_ty_naked_number ty
  | Fabricated ty ->
    Format.fprintf ppf "@[<hov 1>(Fab@ %a)@]"
      (print_ty_fabricated_with_cache ~cache) ty

and print ppf t =
  let cache : Printing_cache.t = Printing_cache.create () in
  print_with_cache ~cache ppf t

let print_ty_value ppf ty =
  print_ty_value_with_cache ~cache:(Printing_cache.create ()) ppf ty

let print_ty_fabricated ppf ty =
  print_ty_fabricated_with_cache ~cache:(Printing_cache.create ()) ppf ty
