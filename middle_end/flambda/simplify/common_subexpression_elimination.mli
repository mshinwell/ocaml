(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Maintenance of environments and associated calculations for
    common subexpression elimination, performed during Simplify. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module EPA = Continuation_extra_params_and_args
module KP = Kinded_parameter
module P = Flambda_primitive
module RI = Apply_cont_rewrite_id
module T = Flambda_type
module TE = Flambda_type.Typing_env

type t

val print : Format.formatter -> t -> unit

val empty : t

val add : t -> P.Eligible_for_cse.t -> bound_to:Simple.t -> t

val find : t -> P.Eligible_for_cse.t -> Simple.t option

val concat : t -> t -> t

val meet : t -> t -> t

module Join_result : sig
  type nonrec t = private
    { cse_at_join_point : t;
      extra_params : EPA.t;
      extra_equations : T.t Name.Map.t;
      extra_allowed_names : Name_occurrences.t;
    }
end

val join
   : typing_env_at_fork:TE.t
  -> cse_at_fork:t
  -> cse_at_each_use:(TE.t * RI.t * t) list
  -> params:KP.t list
  -> Join_result.t
