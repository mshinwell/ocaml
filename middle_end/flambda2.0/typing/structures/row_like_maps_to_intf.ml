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

(** Interface to be satisfied by the right-hand side of a [Row_like]
    mapping. *)

module type S = sig
  module Join_env : sig type t end
  module Meet_env : sig type t end
  module Type_equality_env : sig type t end
  module Type_equality_result : sig type t end
  module Typing_env_extension : sig type t end

  type t

  val bottom : unit -> t

  val add_or_meet_equations
     : t
    -> Meet_env.t
    -> Typing_env_extension.t
    -> t

  val widen : t -> to_match:t -> t

  include Type_structure_intf.S
    with type t := t
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Type_equality_env := Type_equality_env
    with module Type_equality_result := Type_equality_result
    with module Typing_env_extension := Typing_env_extension
end
