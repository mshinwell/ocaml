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

type t =
  | Always_specialise
  | Never_specialise
  | Default_specialise

let print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | Always_specialise -> fprintf ppf "Always_specialise"
  | Never_specialise -> fprintf ppf "Never_specialise"
  | Default_specialise -> fprintf ppf "Default_specialise"
