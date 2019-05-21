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

(** Construction of meet and join operations for types of kind
    (Naked_number Int32). *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Meet_and_join_naked_number_intf.S
  with type flambda_type := Flambda_types.t
  with type typing_env := Typing_env.t
  with type meet_env := Meet_env.t
  with type typing_env_extension := Typing_env_extension.t
  with module Naked_number := Int32
