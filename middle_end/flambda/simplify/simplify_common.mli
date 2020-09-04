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

type 'a after_rebuild =
     Flambda.Expr.t
  -> Upwards_acc.t
  -> 'a

type 'a rebuild =
     Upwards_acc.t
  -> after_rebuild:'a after_rebuild
  -> 'a

type ('a, 'b) down_to_up =
     Downwards_acc.t
  -> rebuild:'a rebuild
  -> 'b

type 'a expr_simplifier =
     Downwards_acc.t
  -> 'a
  -> down_to_up:(Flambda.Expr.t * Upwards_acc.t,
       Flambda.Expr.t * Upwards_acc.t) down_to_up
  -> Flambda.Expr.t * Upwards_acc.t

val rebuild_invalid : (Flambda.Expr.t * Upwards_acc.t) rebuild

val simplify_projection
   : Downwards_acc.t
  -> original_term:Flambda.Named.t
  -> deconstructing:Flambda_type.t
  -> shape:Flambda_type.t
  -> result_var:Var_in_binding_pos.t
  -> result_kind:Flambda_kind.t
  -> Reachable.t * Flambda_type.Typing_env_extension.t * Downwards_acc.t

type cse =
  | Invalid of Flambda_type.t
  (* CR mshinwell: Use a record type for the following and all of the
     simplify_*primitive.mli files *)
  | Applied of
      (Reachable.t * Flambda_type.Typing_env_extension.t
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

type add_wrapper_for_switch_arm_result = private
  | Apply_cont of Flambda.Apply_cont.t
  | New_wrapper of Continuation.t * Flambda.Continuation_handler.t

val add_wrapper_for_switch_arm
   : Upwards_acc.t
  -> Flambda.Apply_cont.t
  -> use_id:Apply_cont_rewrite_id.t
  -> Flambda_arity.With_subkinds.t
  -> add_wrapper_for_switch_arm_result

val add_wrapper_for_fixed_arity_apply
   : Upwards_acc.t
  -> use_id:Apply_cont_rewrite_id.t
  -> Flambda_arity.With_subkinds.t
  -> Apply_expr.t
  -> Flambda.Expr.t

val update_exn_continuation_extra_args
   : Upwards_acc.t
  -> exn_cont_use_id:Apply_cont_rewrite_id.t
  -> Apply_expr.t
  -> Apply_expr.t

val bind_let_bound
   : bindings:((Bindable_let_bound.t * Reachable.t) list)
  -> body:Flambda.Expr.t
  -> Flambda.Expr.t

(** Create [Let_symbol] expression(s) around a given body.  Two optimisations
    are performed:
    1. Best efforts are made not to create the [Let_symbol](s) if it/they
       would be redundant.
    2. Closure variables are removed if they are not used according to the
       given [r].  Such [r] must have seen all uses in the whole
       compilation unit. *)
val create_let_symbols
   : Upwards_acc.t
  -> Symbol_scoping_rule.t
  -> Code_age_relation.t
  -> Simplify_envs.Lifted_constant.t
  -> Flambda.Expr.t
  -> Flambda.Expr.t * Upwards_acc.t

val place_lifted_constants
   : Upwards_acc.t
  -> Symbol_scoping_rule.t
  -> lifted_constants_from_defining_expr:Simplify_envs.Lifted_constant_state.t
  -> lifted_constants_from_body:Simplify_envs.Lifted_constant_state.t
  -> put_bindings_around_body:(body:Flambda.Expr.t -> Flambda.Expr.t)
  -> body:Flambda.Expr.t
  -> critical_deps_of_bindings:Name_occurrences.t
  -> Flambda.Expr.t * Upwards_acc.t

(** Create the projection of a tuple (assumed to be a size-tuple of
   ocaml values. *)
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
