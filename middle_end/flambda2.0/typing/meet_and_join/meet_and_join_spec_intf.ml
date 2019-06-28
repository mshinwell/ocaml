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

module type Meet_and_join_spec_intf = sig
  module Flambda_types : sig
    type t
    type 'a ty
  end

  module Join_env : sig type t end
  module Typing_env_extension : sig type t end

  type of_kind_foo

  val kind : unit -> Flambda_kind.t

  val to_type : of_kind_foo Flambda_types.ty -> Flambda_types.t

  val force_to_kind : Flambda_types.t -> of_kind_foo Flambda_types.ty

  (* CR mshinwell: Rename to [print_ty_with_cache]. *)
  val print_ty
     : cache:Printing_cache.t
    -> Format.formatter
    -> of_kind_foo Flambda_types.ty
    -> unit

  val meet_or_join_of_kind_foo
     : Join_env.t
    -> of_kind_foo
    -> of_kind_foo
    -> (of_kind_foo * Typing_env_extension.t) Or_absorbing.t
end
