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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The signature of a module that specifies meet and join operations for
    a particular kind of types. *)
module type S = sig
  module T : Typing_world_abstract.S

  type of_kind_foo

  val kind : unit -> Flambda_kind.t

  val to_type : of_kind_foo T.Flambda_types.ty -> T.Flambda_types.t

  val force_to_kind : T.Flambda_types.t -> of_kind_foo T.Flambda_types.ty

  (* CR mshinwell: Rename to [print_ty_with_cache]. *)
  val print_ty
     : cache:Printing_cache.t
    -> Format.formatter
    -> of_kind_foo T.Flambda_types.ty
    -> unit

  val meet_or_join_of_kind_foo
     : T.Join_env.t
    -> of_kind_foo
    -> of_kind_foo
    -> (of_kind_foo * T.Typing_env_extension.t) Or_absorbing.t
end
