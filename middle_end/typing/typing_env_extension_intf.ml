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
  type flambda_type

  type t = env_extension

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val equal
     : equal_type:(flambda_type -> flambda_type -> bool)
    -> t
    -> t
    -> bool

  val fast_equal : t -> t -> bool

  val empty : t

  val add_definition_at_beginning
     : t
    -> Name.t
    -> flambda_type
    -> t

  val add_equation
     : t
    -> Name.t
    -> flambda_type
    -> t

  val meet : typing_environment -> t -> t -> t

  val join : typing_environment -> t -> t -> t
end
