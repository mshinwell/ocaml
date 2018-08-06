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
    type closures_entry
    type set_of_closures_entry
  end
  module Join_env : sig type t end
  module Meet_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  val meet
     : Meet_env.t
    -> Flambda_type0_core.t
    -> Flambda_type0_core.t
    -> Flambda_type0_core.t * Typing_env_extension.t

  val join
     : Join_env.t
    -> Flambda_type0_core.t
    -> Flambda_type0_core.t
    -> Flambda_type0_core.t

  val meet_closures_entry
     : Meet_env.t
    -> Flambda_type0_core.closures_entry
    -> Flambda_type0_core.closures_entry
    -> (Flambda_type0_core.closures_entry * Typing_env_extension.t) Or_bottom.t

  val join_closures_entry
     : Join_env.t
    -> Flambda_type0_core.closures_entry
    -> Flambda_type0_core.closures_entry
    -> Flambda_type0_core.closures_entry

  val meet_set_of_closures_entry
     : Meet_env.t
    -> Flambda_type0_core.set_of_closures_entry
    -> Flambda_type0_core.set_of_closures_entry
    -> (Flambda_type0_core.set_of_closures_entry * Typing_env_extension.t)
         Or_bottom.t

  val join_set_of_closures_entry
     : Join_env.t
    -> Flambda_type0_core.set_of_closures_entry
    -> Flambda_type0_core.set_of_closures_entry
    -> Flambda_type0_core.set_of_closures_entry

  val as_or_more_precise
     : Typing_env.t
    -> Flambda_type0_core.t
    -> than:Flambda_type0_core.t
    -> bool

  val strictly_more_precise
     : Typing_env.t
    -> Flambda_type0_core.t
    -> than:Flambda_type0_core.t
    -> bool
end
