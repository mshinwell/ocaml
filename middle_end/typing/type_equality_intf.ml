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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module type S = sig
  module T : sig
    type flambda_type
  end

  val fast_equal : T.flambda_type -> T.flambda_type -> bool

  val equal : T.flambda_type -> T.flambda_type -> bool
end
