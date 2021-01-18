(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Partial dead code elimination, with particular application to the
    sinking of boxing operations, performed during Simplify. *)

[@@@ocaml.warning "+a-30-40-41-42"]

module EPA = Continuation_extra_params_and_args
module KP = Kinded_parameter
module P = Flambda_primitive
module RI = Apply_cont_rewrite_id
module T = Flambda_type
module TE = Flambda_type.Typing_env

module For_downwards_acc : sig
  type t

  val print : Format.formatter -> t -> unit

  val empty : t
end

module For_downwards_env : sig
  type t

  val print : Format.formatter -> t -> unit

  val empty : t

  val consider_simplified_simple
     : t
    -> Simple.t
    -> in_apply_cont:bool
    -> t * For_downwards_acc.t

  val consider_simplified_primitive
     : t
    -> For_downwards_acc.t
    -> TE.t
    -> bound_to:Variable.t
    -> Flambda_primitive.t
    -> t * For_downwards_acc.t

  module Join_result : sig
    type nonrec t = private
      { pdce_at_join_point : t;
        extra_params : EPA.t;
        extra_equations : T.t Name.Map.t;
        extra_allowed_names : Name_occurrences.t;
      }
  end

  (** [join] adds PDCE equations into [pdce_at_fork] at the next scope level
      after that given by the [typing_env_at_fork]. *)
  val join
     : typing_env_at_fork:TE.t
    -> use_info:'a list
    -> get_pdce:('a -> t)
    -> Join_result.t option
end

(*
module For_upwards_env : sig
  type t

  val print : Format.formatter -> t -> unit

  val empty : t
end

module For_upwards_acc : sig
  type t

  val print : Format.formatter -> t -> unit

  val empty : t
end
*)
