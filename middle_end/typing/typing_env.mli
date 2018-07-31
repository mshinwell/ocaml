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

(** Typing environments. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (T : Typing_world.S)
  : Typing_env_intf.S
      with module Flambda_type := T.Flambda_type
      with module Function_parameters := T.Function_parameters
      with module Join_env := T.Join_env
      with module Typing_env := T.Typing_env
      with module Typing_env_extension := T.Typing_env_extension
