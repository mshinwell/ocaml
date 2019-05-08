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

(** Implementation of the parts of meet and join operations on Flambda types
    that are kind-independent, for example handling of aliases. The
    implementation is parameterised over whether a meet or join operation is to
    be produced; and over the corresponding kind-dependent meet or join
    operation for the innards of the types. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: We should enforce that [S] must have been specialised
   using the same [E]. *)
module Make
  (E : Either_meet_or_join_intf
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension)
  (S : Meet_and_join_spec_intf
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Typing_env_extension := Typing_env_extension) :
sig
  val meet_or_join_ty
     : ?bound_name:Name.t
    -> Join_env.t
    -> S.of_kind_foo Flambda_types.ty
    -> S.of_kind_foo Flambda_types.ty
    -> S.of_kind_foo Flambda_types.ty * Typing_env_extension.t
end
