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

module type S = sig
  module rec Flambda_type : Flambda_type0_internal_intf.S
    with module Typing_env_extension := Typing_env_extension
  and Typing_env : Typing_env_intf.S
    with module Flambda_type := Flambda_type
  and Typing_env_extension : Typing_env_extension_intf.S
    with module Flambda_type := Flambda_type
  and Join_env : Join_env_intf.S
    with module Flambda_type := Flambda_type
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
end
