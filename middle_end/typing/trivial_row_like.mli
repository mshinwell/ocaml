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

(** Simple instantiation of [Row_like.S] for use when the index ranges over
    sets of some kind of thing (not containing any names) and there is no
    relational product information involved, just an environment extension
    for each case. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (Thing_without_names : Map.With_set) (T : Typing_world.S) :
  Trivial_row_like_intf.S
    with module Flambda_type := T.Flambda_type
    with module Join_env := T.Join_env
    with module Typing_env := T.Typing_env
    with module Typing_env_extension := T.Typing_env_extension
