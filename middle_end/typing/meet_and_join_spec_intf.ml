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

(** The signature of a module that specifies meet and join operations for
    a particular kind of types. *)
module type S = sig
  type typing_environment
  type env_extension

  type flambda_type
  type of_kind_foo

  type 'a ty

  val kind : Flambda_kind.t

  val to_type : of_kind_foo ty -> flambda_type

  val force_to_kind : flambda_type -> of_kind_foo ty

  val print_ty : Format.formatter -> of_kind_foo ty -> unit

  val meet_of_kind_foo
     : typing_environment
    -> of_kind_foo
    -> of_kind_foo
    -> (of_kind_foo * env_extension) Or_bottom.t

  (* If the supplied types are compatible, the join must be pushed inside
     their structure, and [Ok] returned.  Otherwise [Unknown] must be
     returned. *)
  (* CR mshinwell: add comment about requirement for equivalence
     relationness *)
  val join_of_kind_foo
     : typing_environment
    -> typing_environment
    -> of_kind_foo
    -> of_kind_foo
    -> of_kind_foo Or_unknown.t
end
