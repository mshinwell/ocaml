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
    (Naked_number Float). *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Meet_and_join_naked_number_intf.S
  with type flambda_type := Flambda_types.t
  with type 'a ty := 'a Flambda_types.ty
  with type 'a of_kind_naked_number := 'a Flambda_types.of_kind_naked_number
  with type typing_env := Typing_env.t
  with type meet_env := Meet_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type naked_number_kind := Flambda_kind.naked_float
  with module Naked_number := Numbers.Float_by_bit_pattern
