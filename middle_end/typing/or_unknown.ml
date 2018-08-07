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

type 'a t =
  | Known of 'a
  | Unknown

let print f ppf t =
  match t with
  | Known contents -> Format.fprintf ppf "@[(Known %a)@]" f contents
  | Unknown -> Format.pp_print_string ppf "Unknown"

let equal equal_contents t1 t2 =
  match t1, t2 with
  | Unknown, Unknown -> true
  | Known contents1, Known contents2 -> equal_contents contents1 contents2
  | Unknown, Known _
  | Known _, Unknown -> false
