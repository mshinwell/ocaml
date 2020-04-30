(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = Mutable | Immutable

let print ppf t =
  match t with
  | Mutable -> Format.pp_print_string ppf "Mutable"
  | Immutable -> Format.pp_print_string ppf "Immutable"

let compare t1 t2 =
  match t1, t2 with
  | Mutable, Mutable | Immutable, Immutable -> 0
  | Mutable, Immutable -> -1
  | Immutable, Mutable -> 1

let join t1 t2 =
  match t1, t2 with
  | Immutable, Immutable -> Immutable
  | Mutable, Mutable
  | Immutable, Mutable
  | Mutable, Immutable -> Mutable

let to_lambda t : Asttypes.mutable_flag =
  match t with
  | Mutable -> Mutable
  | Immutable -> Immutable
