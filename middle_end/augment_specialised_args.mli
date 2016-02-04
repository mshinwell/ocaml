(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

(** Helper module for adding specialised arguments to sets of closures. *)

module What_to_specialise : sig
  type t

  val create : set_of_closures:Flambda.set_of_closures -> t

  type fun_var_and_group = {
    fun_var : Variable.t;
    group : Variable.t;
  }

  val new_specialised_arg
     : t
    -> fun_vars_and_groups:fun_var_and_group list
    -> defining_expr_in_terms_of_existing_outer_vars:Projection.t
    -> projection:Projectee.Var_and_projectee.t
    -> t
end

module type S = sig
  val pass_name : string
  val variable_suffix : string

  type user_data

  val precondition
     : backend:(module Backend_intf.S)
    -> env:Inline_and_simplify_aux.Env.t
    -> set_of_closures:Flambda.set_of_closures
    -> user_data option

  val what_to_specialise
     : env:Inline_and_simplify_aux.Env.t
    -> closure_id:Closure_id.t
    -> function_decl:Flambda.function_declaration
    -> set_of_closures:Flambda.set_of_closures
    -> user_data:user_data
    -> what_to_specialise:What_to_specialise.t
    -> What_to_specialise.t
end

module Make (T : S) : sig
  val rewrite_set_of_closures
     : backend:(module Backend_intf.S)
    -> env:Inline_and_simplify_aux.Env.t
    -> set_of_closures:Flambda.set_of_closures
    -> Flambda.expr option
end
