(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Environments and result structures used during simplification. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module rec Env : sig
  include Simplify_env_and_result_intf.Env
    with type result = Result.t
end and Result : sig
  include Simplify_env_and_result_intf.Result
    with type env = Env.t
end
