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

(** Interface to type equality functions. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module Flambda_types : sig type t end

  val fast_equal : Flambda_types.t -> Flambda_types.t -> bool

  val equal : Flambda_types.t -> Flambda_types.t -> bool
end
