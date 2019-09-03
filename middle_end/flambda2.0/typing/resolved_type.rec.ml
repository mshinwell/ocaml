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

[@@@ocaml.warning "+a-30-40-41-42"]

type resolved_t =
  | Value of Type_of_kind_value0.t
  | Naked_number of Type_of_kind_naked_number0.t
  | Fabricated of Type_of_kind_fabricated0.t

type t =
  | Const of Simple.Const.t
  | Discriminant of Discriminant.t
  | Resolved of resolved_t Or_unknown_or_bottom.t
