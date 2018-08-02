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

(** Dependent function types.

    Logical variables introduced by the parameter types may be used in the
    result types.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (T : Typing_world.S) :
  Function_type_intf.S
    with module Flambda_type0_core := T.Flambda_type0_core
    with module Join_env := T.Join_env
    with module Typing_env := T.Typing_env
    with module Typing_env_extension := T.Typing_env_extension
