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
  module Flambda_types : sig
    type t
    type 'a ty
    type of_kind_fabricated
  end
  module Join_env : sig type t end
  module Meet_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  module Make
    (E : Either_meet_or_join_intf.S
     with module Join_env := Join_env
     with module Meet_env := Meet_env
     with module Typing_env := Typing_env
     with module Typing_env_extension := Typing_env_extension)
  : Make_meet_or_join_intf.S_applied
     with module Flambda_types := Flambda_types
     with module Join_env := Join_env
     with module Meet_env := Meet_env
     with module Typing_env := Typing_env
     with module Typing_env_extension := Typing_env_extension
     with type of_kind_foo := Flambda_types.of_kind_fabricated
end
