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

(** Rewrites applied to [Apply_cont] expressions in order to reflect
    changes in continuation arities consequential to addition or removal of
    parameters. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val print : Format.formatter -> t -> unit

val create
   : original_params:Kinded_parameter.t list
  -> used_params:Kinded_parameter.Set.t
  -> extra_params:Kinded_parameter.t list
  -> extra_args:Continuation_extra_params_and_args.Extra_arg.t list
       Apply_cont_rewrite_id.Map.t
  -> used_extra_params:Kinded_parameter.Set.t
  -> t

val extra_params : t -> Kinded_parameter.t list

val extra_args
   : t
  -> Apply_cont_rewrite_id.t
  -> Continuation_extra_params_and_args.Extra_arg.t list

val rewrite_use
  : simplify_named:(
     'a
  -> Flambda.Named.t
  -> result_var:Var_in_binding_pos.t
  -> Reachable.t * 'a)
  -> 'a
  -> t
  -> Apply_cont_rewrite_id.t
  -> Flambda.Apply_cont.t
  -> 'a *
     Flambda.Expr.t * Flambda.Apply_cont.t
       * (Flambda.Expr.t -> Flambda.Expr.t)
       * Simple.t list