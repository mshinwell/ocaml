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
    module Closures_entry : sig type t end
  end
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  (* CR mshinwell: This module is unpleasant.  We should arrange things so that
     [Var_within_closure.Set.Map] exists.  (This should be easier now that
     things brought in using "include" can be shadowed.) *)
  module Var_within_closure_set : sig
    type t = Var_within_closure.Set.t

    include Map.With_set with type t := t
  end

  module Closure_id_and_var_within_closure_set : sig
    type t = Closure_id.t * Var_within_closure_set.t

    include Map.With_set with type t := t
    include Contains_names.S with type t := t
  end

  type t

  val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

  (** Describe one or more closures by giving for each one the closure ID
      and the set of variables in the closure. *)
  val create_exactly_multiple
     : Flambda_type0_core.Closures_entry.t
         Closure_id_and_var_within_closure_set.Map.t
    -> t

  (** Describe one or more closures that contain at least the given closure
      variables. *)
  val create_at_least_multiple
     : Flambda_type0_core.Closures_entry.t Var_within_closure_set.Map.t
    -> t

  (** Greatest lower bound of two values of type [t]. *)
  val meet
     : Typing_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> t
    -> t
    -> t Or_bottom.t

  (** Least upper bound of two values of type [t]. *)
  val join
     : Join_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> t
    -> t
    -> t

  include Contains_names.S with type t := t
end
