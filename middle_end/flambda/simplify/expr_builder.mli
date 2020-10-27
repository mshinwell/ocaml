(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Functions for rebuilding expressions that are used during
    simplification.  Unlike the basic creation functions in [Expr] these
    functions do things such as keeping track of free names and
    avoiding generation of unused bindings. *)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

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

(** Place lifted constants whose defining expressions involve [Name]s (for
    example those bound by a [Let] or a [Let_cont]) that are about to go out
    of scope. *)
val place_lifted_constants
   : Upwards_acc.t
  -> Symbol_scoping_rule.t
  -> lifted_constants_from_defining_expr:Simplify_envs.Lifted_constant_state.t
  -> lifted_constants_from_body:Simplify_envs.Lifted_constant_state.t
  -> put_bindings_around_body:(body:Flambda.Expr.t -> Flambda.Expr.t)
  -> body:Flambda.Expr.t
  -> critical_deps_of_bindings:Name_occurrences.t
  -> Flambda.Expr.t * Upwards_acc.t
