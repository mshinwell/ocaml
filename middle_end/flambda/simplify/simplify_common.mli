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

(** Miscellaneous utility functions and types used by the simplifier. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val simplify_projection
   : Downwards_acc.t
  -> original_term:Flambda.Named.t
  -> deconstructing:Flambda_type.t
  -> shape:Flambda_type.t
  -> result_var:Var_in_binding_pos.t
  -> result_kind:Flambda_kind.t
  -> Simplified_named.t
     * Flambda_type.Typing_env_extension.t
     * Downwards_acc.t

type cse =
  | Invalid of Flambda_type.t
  (* CR mshinwell: Use a record type for the following and all of the
     simplify_*primitive.mli files *)
  | Applied of
      (Simplified_named.t * Flambda_type.Typing_env_extension.t
        * Simple.t list * Downwards_acc.t)
  | Not_applied of Downwards_acc.t

val try_cse
   : Downwards_acc.t
  -> original_prim:Flambda_primitive.t
  -> result_kind:Flambda_kind.t
  -> min_name_mode:Name_mode.t
  -> args:Simple.t list
  -> result_var:Variable.t
  -> cse

val update_exn_continuation_extra_args
   : Upwards_acc.t
  -> exn_cont_use_id:Apply_cont_rewrite_id.t
  -> Apply_expr.t
  -> Apply_expr.t

(** Create a projection from a tuple (assumed to be a [size]-tuple of
    OCaml values). *)
val project_tuple
   : dbg:Debuginfo.t
  -> size:int
  -> field:int
  -> Simple.t
  -> Flambda.Named.t

(** Split a direct over-application into a full application followed by
    the application of the leftover arguments. *)
val split_direct_over_application
  : Apply_expr.t
 -> param_arity:Flambda_arity.With_subkinds.t
 -> Flambda.Expr.t
