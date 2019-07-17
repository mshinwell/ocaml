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

(** Typing environment extensions: typing environment levels that are
    surrounded by a binder that captures defined names as existentials. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val print_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> t
  -> unit

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val empty : unit -> t

val is_empty : t -> bool

val create : Typing_env_level.t -> t

val pattern_match : t -> f:(Typing_env_level.t -> 'a) -> 'a

val one_equation : Name.t -> Flambda_types.t -> t

val add_or_replace_equation : t -> Name.t -> Flambda_types.t -> t

val concat : t -> t -> t

val meet : Meet_env.t -> t -> t -> t

module Make_join (Id : Identifiable.S) : sig
  module Extra_cse_bindings : sig
    type t = private {
      extra_params : Kinded_parameter.t list;
      bound_to : Simple.t Id.Map.t list;
    }
  end

  val n_way_join
     : Typing_env.t
    -> (Typing_env.t * Id.t * t) list
    -> t * Extra_cse_bindings.t
end

val mem : t -> Name.t -> bool
