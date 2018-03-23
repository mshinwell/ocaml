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
  type typing_environment
  type equations

  type 'a ty

  type of_kind_foo

  (* Least upper bound of two types of a particular kind. *)
  val join_ty
     : typing_environment
    -> typing_environment
    -> of_kind_foo ty
    -> of_kind_foo ty
    -> of_kind_foo ty

  (* Greatest lower bound of two types of a particular kind. *)
  val meet_ty
     : typing_environment
    -> typing_environment
    -> of_kind_foo ty
    -> of_kind_foo ty
    -> of_kind_foo ty * equations
end

module type S_for_types = sig
  type flambda_type
  type t_in_context
  type equations

  val meet
     : bias_towards:t_in_context
    -> t_in_context
    -> flambda_type * equations

  val join : t_in_context -> t_in_context -> flambda_type

  val meet_equations
     : resolver:(Export_id.t -> flambda_type option)
    -> equations
    -> equations
    -> equations

  val join_equations
     : resolver:(Export_id.t -> flambda_type option)
    -> equations
    -> equations
    -> equations
end
