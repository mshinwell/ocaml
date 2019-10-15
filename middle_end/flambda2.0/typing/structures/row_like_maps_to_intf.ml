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
  type join_env
  type meet_env
  type type_equality_env
  type type_equality_result
  type typing_env_extension

  type t

  val widen : t -> to_match:t -> t

  include Type_structure_intf.S
    with type t := t
    with type join_env := join_env
    with type meet_env := meet_env
    with type type_equality_env := type_equality_env
    with type type_equality_result := type_equality_result
    with type typing_env_extension := typing_env_extension
end
