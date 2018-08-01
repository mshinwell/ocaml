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

module type S = sig
  module Flambda_type0_core : sig
    type t
    module Set_of_closures_entry : sig type t end
  end
  module Join_env : sig type t end
  module Typing_env : sig type t end

  type t

  val invariant : t -> unit

  type open_or_closed = Open | Closed

  val create
     : Flambda_type0_core.Set_of_closures_entry.t Closure_id.Map.t
    -> open_or_closed
    -> t

  val meet : Typing_env.t -> t -> t -> t Or_bottom.t

  val join : Join_env.t -> t -> t -> t
end
