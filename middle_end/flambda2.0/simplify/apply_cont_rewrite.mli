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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Id : sig
  type t
  include Identifiable.S with type t := t

  val create : unit -> t
end

val create
   : original_params:Kinded_parameter.t list
  -> used_params:Kinded_parameter.t list
  -> extra_params_and_args:Continuation_uses_env.extra_params_and_args
  -> used_extra_params:(Kinded_parameter.t * Simple.t) list
  -> t

val extra_params : t -> Kinded_parameter.Set.t

val extra_args : t -> Id.t -> Simple.t list

val rewrite_use : t -> Id.t -> Apply_cont.t -> Apply_cont.t
