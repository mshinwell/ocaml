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

(** The interface of a module that provides generic meet-and-join operations
    after it has been specialised to either meet or join. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  type flambda_type
  type 'a ty
  type 'a of_kind_naked_number
  type join_env
  type typing_env_extension
  type of_kind_foo

  val kind : unit -> Flambda_kind.t

  val to_type : of_kind_foo ty -> flambda_type

  val force_to_kind : flambda_type -> of_kind_foo ty

  (* CR mshinwell: Rename to [print_ty_with_cache]. *)
  val print_ty
     : cache:Printing_cache.t
    -> Format.formatter
    -> of_kind_foo ty
    -> unit

  val meet_or_join_of_kind_foo
     : join_env
    -> of_kind_foo
    -> of_kind_foo
    -> (of_kind_foo * typing_env_extension) Or_absorbing.t
end
