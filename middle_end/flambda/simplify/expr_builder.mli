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

open! Flambda.Import

(** Create [Let] binding(s) around a given body.  (The type of this function
    prevents it from being used to create "let symbol" bindings; use the
    other functions in this module instead.)  Bindings will be elided if they
    are unused.
    The [name_occurrences] in the provided [uacc] must contain exactly the
    free names of the [body]. *)
val make_new_let_bindings
   : Upwards_acc.t
  -> bindings_outermost_first:(Bindable_let_bound.t * Simplified_named.t) list
  -> body:Expr.t
  -> Expr.t * Upwards_acc.t

(** Create the "let symbol" binding(s) around a given body necessary to define
    the given lifted constant.  Two optimisations are performed:
    1. Best efforts are made not to create the binding(s) if it/they
       would be redundant.
    2. Closure variables are removed if they are not used according to the
       given [uacc].  (Such [uacc] must have seen all uses in the whole
       compilation unit.)
    The [name_occurrences] in the provided [uacc] must contain exactly the
    free names of the [body]. *)
val create_let_symbols
   : Upwards_acc.t
  -> Symbol_scoping_rule.t
  -> Code_age_relation.t
  -> Simplify_envs.Lifted_constant.t
  -> body:Expr.t
  -> Expr.t * Upwards_acc.t

(** Place lifted constants whose defining expressions involve [Name]s (for
    example those bound by a [Let] or a [Let_cont]) that are about to go out
    of scope.
    The [name_occurrences] in the provided [uacc] must contain exactly the
    free names of the [body]. *)
val place_lifted_constants
   : Upwards_acc.t
  -> Symbol_scoping_rule.t
  -> lifted_constants_from_defining_expr:Simplify_envs.Lifted_constant_state.t
  -> lifted_constants_from_body:Simplify_envs.Lifted_constant_state.t
  -> put_bindings_around_body:
       (Upwards_acc.t -> body:Expr.t -> Expr.t * Upwards_acc.t)
  -> body:Expr.t
  -> critical_deps_of_bindings:Name_occurrences.t
  -> Expr.t * Upwards_acc.t
