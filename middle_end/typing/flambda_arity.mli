(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Generalization of the concepts of "number of arguments" and "number
    of return values". *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = Flambda_kind.t list

val length : t -> int

val is_all_values : t -> bool

val is_all_naked_floats : t -> bool

val compatible : t -> if_used_at:t -> bool

include Hashtbl.With_map with type t := t
