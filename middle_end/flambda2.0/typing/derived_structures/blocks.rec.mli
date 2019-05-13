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

type t

type open_or_closed = Open | Closed of Tag.t

val create_bottom : unit -> t

val create
   : field_tys:Flambda_types.t list
  -> Flambda_kind.t
  -> open_or_closed
  -> t

val invariant : t -> unit

val is_empty : t -> bool

val classify : t -> unit Or_unknown_or_bottom.t

val map_types : t -> f:(Flambda_types.t -> Flambda_types.t) -> t

include Type_structure_intf.S
  with type t := t
  with type flambda_type := Flambda_types.t
  with type join_env := Join_env.t
  with type meet_env := Meet_env.t
  with type type_equality_env := Type_equality_env.t
  with type type_equality_result := Type_equality_result.t
  with type typing_env_extension := Typing_env_extension.t
