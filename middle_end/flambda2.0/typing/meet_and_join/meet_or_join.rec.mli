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

(** Construction of either the meet or the join operation on Flambda types. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make
  (E : Lattice_ops_intf.S
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type typing_env_extension := Typing_env_extension.t) :
sig
  (** Perform a meet or a join operation, in the given environment, on the
      given types. *)
  (* CR mshinwell: Document [bound_name]. *)
  val meet_or_join
     : ?bound_name:Name.t
    -> Meet_env.t
    -> Type_grammar.t
    -> Type_grammar.t
    -> Type_grammar.t * Typing_env_extension.t
end
