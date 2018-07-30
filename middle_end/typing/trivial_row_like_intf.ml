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
  module Flambda_type : sig type t end
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end
  module Thing_without_names : Map.With_set

  include Row_like_intf.S
    with module Tag := Unit
    with module Index := Thing_without_names.Set
    with module Maps_to := Typing_env_extension
    with module Flambda_type := Flambda_type
    with module Join_env := Join_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
end
