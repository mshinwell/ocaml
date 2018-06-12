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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module type S = sig
  type env_extension
  type typing_environment
  type join_env

  type 'a ty

  type of_kind_foo

  val meet_or_join_ty
     : join_env
    -> of_kind_foo ty
    -> of_kind_foo ty
    -> of_kind_foo ty * env_extension
end

module type S_for_types = sig
  type flambda_type
  type typing_environment
  type env_extension
  type join_env

  val meet_or_join
     : join_env
    -> flambda_type
    -> flambda_type
    -> flambda_type * env_extension

  val as_or_more_precise
     : typing_environment
    -> flambda_type
    -> than:flambda_type
    -> bool

  val strictly_more_precise
     : typing_environment
    -> flambda_type
    -> than:flambda_type
    -> bool
end
