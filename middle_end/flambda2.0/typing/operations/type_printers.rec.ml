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


let unicode = true  (* CR mshinwell: move elsewhere *)


let print_ty_generic print_contents ppf ty =
  (print_or_alias (print_unknown_or_join print_contents)) ppf ty


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

and print_ty_value_with_cache ~cache ppf (ty : Type_grammar.ty_value) =
  print_ty_generic (print_of_kind_value ~cache) ppf ty


and print_of_kind_fabricated ~cache ppf
      (o : Type_grammar.of_kind_fabricated) =
  match o with
  | Discriminants discriminants ->
    Format.fprintf ppf "@[<hov 1>(Discriminants@ %a)@]"
      (Discriminants.print_with_cache ~cache) discriminants

and print_ty_fabricated_with_cache ~cache ppf
      (ty : Type_grammar.ty_fabricated) =
  print_ty_generic (print_of_kind_fabricated ~cache) ppf ty
